(in-package #:syscat)

(defmethod find-ip-address
  ((db neo4cl:neo4j-rest-server)
   (raw-address string)
   &key (asn 0))
  (let ((address (if (cl-ppcre:all-matches "[0-9]\.[0-9]\.[0-9]\.[0-9]"
                                           raw-address)
                   (sb-bsd-sockets:make-inet-address raw-address)
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

(defun ipv4-network-address (cidr)
  "Takes a CIDR subnet spec, e.g. 192.168.0.1/24, and returns its network address"
  (let* ((address-parts (cl-ppcre:split "/" cidr))
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
                do (setf (ldb (byte 1 i) address-int) 0))
          (cl-cidr-notation:ip-string
            address-int))))))
