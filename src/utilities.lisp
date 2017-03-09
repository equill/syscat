(in-package #:syscat)

(defun cidr-to-ints (cidrstr)
  "Converts a CIDR representation of a subnet, e.g. 127.0.0.1/8 into a list of two integers."
  (let
    ((parts (cl-ppcre:split "/" cidrstr)))
    (list
      (cl-cidr-notation:parse-ip (first parts))
      (parse-integer (second parts) :junk-allowed nil))))

(defmethod check-for-single-asn ((db neo4cl:neo4j-rest-server))
  (let ((result
          (mapcar #'car
                  (neo4cl:extract-rows-from-get-request
                    (neo4cl:neo4j-transaction
                      db
                      `((:STATEMENTS
                          ((:STATEMENT . "MATCH (a:asn) return a.uid")))))))))
    (if
       ;; More than one ASN
       (> (length result) 1)
       (error 'restagraph:client-error :message "More than one ASN found in the database")
       ;; Clearly we only either one or none.
       ;; If the latter, we're returning NIL automagically
       (car result))))

(defmethod check-for-single-vrf ((db neo4cl:neo4j-rest-server)
                                 (asn string))
  (let ((result
          (mapcar #'car
                  (neo4cl:extract-rows-from-get-request
                    (neo4cl:neo4j-transaction
                      db
                      `((:STATEMENTS
                          ((:STATEMENT
                             . ,(format nil "MATCH (:asn {uid: '~A'})-[:VrfGroups]->(r:vrfGroups) return r.uid" asn))))))))))
    (if
       ;; More than one VRF
       (> (length result) 1)
       (error 'restagraph:client-error :message "More than one VRF found in the database")
       ;; Clearly we only either one or none.
       ;; If the latter, we're returning NIL automagically
       (car result))))

(defmethod find-subnet ((db neo4cl:neo4j-rest-server)
                        (asn string)
                        (vrf string)
                        &key cidr)
  (neo4cl:extract-rows-from-get-request
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT
             . ,(if vrf
                    (format nil "p=MATCH (:asn {uid: '~A'})-[:VrfGroups]->(:vrfGroup {uid: '~A'})-[:Subnets*]->(s:ipv4Subnets {uid: '~A'}) return p"
                            asn vrf (restagraph::sanitise-uid cidr))
                    (format nil "p=MATCH (:asn {uid: '~A'})-[:Subnets*]->(s:ipv4Subnets {uid: '~A'}) return p"
                            asn (restagraph::sanitise-uid cidr))))))))))

(defmethod find-supernet ((db neo4cl:neo4j-rest-server)
                          (subnet string)
                          (asn string)
                          (vrf string)
                          (path list))
  (let ((candidates
          (mapcar
            #'car
            (neo4cl:extract-rows-from-get-request
              (neo4cl:neo4j-transaction
                db
                `((:STATEMENTS
                    ((:STATEMENT
                       . ,(format nil "MATCH (:asn {uid: '~A'})~A~A-[:Subnets]->(s:ipv4Subnets) return s.uid"
                                  asn
                                  (if vrf
                                      (format nil "-[:VrfGroups]->(:vrfGroups {uid: '~A'})" vrf)
                                      "")
                                  (format nil "~{-[:Subnets]->(:ipv4Subnets {uid: '~A'})~}" path)))))))))))))

(defmethod find-ip-address
  ((db neo4cl:neo4j-rest-server)
   (raw-address string)
   &key (asn 0))
  (let ((address (if (cl-ppcre:all-matches "[0-9]\.[0-9]\.[0-9]\.[0-9]"
                                           raw-address)
                   (cl-cidr-notation:parse-ip raw-address)
                   (sb-bsd-sockets:make-inet6-address raw-address))))
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH p=(a:asn {uid: '~A'})-[]->(i:~A {uid: '~A'}) RETURN p"
                     asn
                     (if (equal (length address) 4)
                       "ipv4Address"
                       "ipv6Address")
                     address))))))))

(defun ipv4-network-address (cidr &key broadcast)
  "Takes a CIDR subnet spec, e.g. 192.168.0.1/24, and returns its network address.
   If :broadcast is specified as non-null, calculates the broadcast address for the subnet."
  (let* ((mask-bit (if broadcast 1 0))
         (address-parts (cl-ppcre:split "/" cidr))
         (address-int (cl-cidr-notation:parse-ip (first address-parts)))
         (prefix-length (parse-integer (second address-parts) :junk-allowed nil)))
    (cond
      ;; Sanity-check
      ((not (and (integerp prefix-length)
                 (>= prefix-length 0)
                 (<= prefix-length 32)))
       (error "Prefix-length must be an integer between 0 and 32"))
      ;; If the prefix-length is 32, the address _is_ the network address
      ((equal prefix-length 32)
       (first address-parts))
      ;; Otherwise, calculate the network address
      (t
       (progn
         (loop for i from 0 to (- 32 prefix-length)
               do (setf (ldb (byte 1 i) address-int) mask-bit))
         (cl-cidr-notation:ip-string
           address-int))))))

(defun parent-ipv4-subnet-p (netaddr1 netaddr2)
  "Checks whether netaddr1 is a supernet of netaddr2, where both are strings in CIDR format.
   Currently only works for IPv4."
  (let*
    ((result t)
     (net1 (cidr-to-ints netaddr1))
     (prefix1 (second net1))
     (net2 (cidr-to-ints netaddr2))
     (prefix2 (second net2)))
    (if
      (>= prefix1 prefix2)
      (error "Prefix 1 is longer than prefix 2")
      (loop for i from (- 32 prefix1) to 31
            do (unless (= (ldb (byte 1 i) (first net1))
                          (ldb (byte 1 i) (first net2)))
                 (setf result nil))))
    result))

(defgeneric find-parent-subnet (db asn vrfgroup subnet &optional candidate-parent)
  (:documentation "Find the subnet with the longest prefix-length that could plausibly be a parent subnet to the one supplied."))

(defmethod find-parent-subnet ((db neo4cl:neo4j-rest-server)
                               (asn string)
                               (vrfgroup string)
                               (subnet string)
                               &optional candidate-parent)
  (let ((netparts (cidr-to-ints subnet)))
    (if (and candidate-parent
             (>= (second (cidr-to-ints candidate-parent))
                 (second netparts)))
      (error 'restagraph::client-error :message "Proposed parent subnet has a longer prefix than the child")
      (let ((candidate-list
              (neo4cl::extract-rows-from-get-request
                (neo4cl:neo4j-transaction
                  db
                  `((:STATEMENTS
                      ((:STATEMENT
                         . (format
                             nil
                             "MATCH (:asn {uid: '~A'})-[:VrfGroups]->(:vrfGroup {uid: '~A'})-[:Subnets]->?"
                             asn vrfgroup)))))))))
        candidate-list))))

(defgeneric add-subnet (db subnet &key asn vrf)
  (:documentation "IPAM method to add a subnet."))

(defmethod add-subnet ((db neo4cl:neo4j-rest-server)
                       (subnet string)
                       &key asn
                       vrf)
  (let* ((asnc (or asn
                  (let
                    ((asn-list
                       (mapcar #'car
                               (neo4cl::extract-rows-from-get-request
                                 (neo4cl:neo4j-transaction
                                   db
                                   `((:STATEMENTS
                                       ((:STATEMENT
                                          .  "MATCH (a:asn) RETURN a.uid")))))))))
                    (cond
                      ((= (length asn-list) 0)
                       (error 'restagraph:client-error :message "No ASN supplied, and no ASN found"))
                      ((> (length asn-list) 1)
                       (error 'restagraph:client-error :message "No ASN supplied, and more than one ASN found"))
                      (t
                       (car asn-list))))))
        (vrfgroup
          (or vrf
                  (let
                    ((vrf-list
                       (mapcar #'car
                               (neo4cl::extract-rows-from-get-request
                                 (neo4cl:neo4j-transaction
                                   db
                                   `((:STATEMENTS
                                       ((:STATEMENT
                                          .  (format
                                          nil
                                          "MATCH (:asn {uid: '~A'})-[:VrfGroups]-(v:vrfGroup) RETURN v.uid"
                                          asnc))))))))))
                    (cond
                      ((= (length vrf-list) 0)
                       (error 'restagraph:client-error :message "No VRF supplied, and no VRF found"))
                      ((> (length vrf-list) 1)
                       (error 'restagraph:client-error :message "No VRF supplied, and more than one VRF found"))
                      (t
                       (car vrf-list)))))))
    (format nil "~A/~A" asnc vrfgroup)))
