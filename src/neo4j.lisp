(in-package #:syscat)

(defmethod find-subnet ((db neo4cl:neo4j-rest-server)
                        (org string)
                        (vrf string)
                        (subnet string)
                        &optional path)
  (restagraph:log-message
    :debug
    (format nil "find-subnet: Search for subnet ~A in path /~A~A~{/~A~}"
            subnet
            org
            (if (equal vrf "")
              ""
              (format nil "/~A" vrf))
            path))
  ;; Sanity-check
  (unless (ipv4-subnet-p subnet)
    (error 'restagraph:client-error
           :message "This is not a CIDR-FORMAT IPv4 subnet"))
  ;; Isolate the components of the subnet
  (let* ((parts (cl-ppcre:split "/" subnet))
         (net-addr (restagraph::sanitise-uid (first parts)))
         (prefix (second parts)))
    ;; Check whether we already have a match on this path
    (let ((match
            (restagraph:get-resources
              db
              (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Subnets/ipv4Subnets/~A"
                      org
                      (if (equal vrf "")
                        ""
                        (format nil "/VrfGroups/vrfGroups/~A" vrf))
                      path
                      net-addr))))
      ;; If we have a match, return the path to it
      (if (and
            match
            (equal prefix (cdr (assoc :prefixlength match))))
        (progn
          (restagraph:log-message :debug (format nil "Found match: ~A" match))
          (let ((newpath (append path (list (cdr (assoc :uid match))))))
            (restagraph:log-message
              :debug
              (format nil "Found exact match for '~A'. Returning subnet path '~{/~A~}'."
                      match newpath))
            newpath))
        ;; If not, check whether we have a supernet of the subnet we're looking for
        (let* ((candidates
                 (restagraph:get-resources
                   db
                   (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Subnets"
                           org
                           (if (equal vrf "") "" (format nil "/VrfGroups/vrfGroups/~A" vrf))
                           path)))
               (supernet
                 (remove-if-not
                   #'(lambda (s)
                       (restagraph:log-message
                         :debug
                         (format nil "Testing ~A/~A from ~A"
                                 (cdr (assoc :original--uid s)) (cdr (assoc :prefixlength s)) s))
                       (parent-ipv4-subnet-p
                         (format nil "~A/~A"
                                 (cdr (assoc :original--uid s)) (cdr (assoc :prefixlength s)))
                         subnet))
                   candidates)))
          (progn
            (restagraph:log-message
              :debug
              (format nil "Exact match not found. Looking for a supernet in ~A" candidates))
            ;; Do we have a supernet (and only one) to check under?
            (if (equal (length supernet) 1)
              (progn
                (restagraph:log-message :debug (format nil "Supernet found: ~A" supernet))
                (let ((newpath (append path (list (cdr (assoc :uid (car supernet)))))))
                  (restagraph:log-message
                    :debug
                    (format nil "Supernet identified. Finding the location of ~A under /~A~A~{/~A~}"
                            subnet
                            org
                            (if (equal vrf "")
                              ""
                              (format nil "/~A" vrf))
                            newpath))
                  (find-subnet db org vrf subnet newpath)))
              ;; No candidate supernet found
              (restagraph:log-message :debug "No exact match or supernet found on this path."))))))))

(defmethod find-parent-subnet ((db neo4cl:neo4j-rest-server)
                               (subnet string)
                               (org string)
                               (vrfgroup string)
                               (path list))
  (restagraph:log-message
    :debug
    (format nil "Searching for parent subnet for ~A under organisation '~A' and VRF '~A'."
            subnet org vrfgroup))
  ;; Get a list of candidate supernets
  (let ((candidates
          ;; Remove nulls from the list returned by the coming filter
          (remove-if
            #'null
            (mapcar #'(lambda (c)
                        (restagraph:log-message
                          :debug
                          (format nil "Testing candidate parent subnet ~A" c))
                        (when (parent-ipv4-subnet-p
                                (format nil "~A/~A"
                                        (cdr (assoc :uid c))
                                        (cdr (assoc :prefixlength c)))
                                subnet)
                          (cdr (assoc :uid c))))
                    ;; Extract the list of candidate supernets
                    (restagraph:get-resources
                      db
                      (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Subnets"
                              org
                              (if (equal vrfgroup "")
                                ""
                                (format nil "/VrfGroups/vrfGroups/~A" vrfgroup))
                              path))))))
    (restagraph:log-message
      :debug
      (format nil "Candidate supernets for ~A under ~{/~A~}: ~A" subnet path candidates))
    (cond
      ;; If there's no candidate, this is where the search stops. Return the path to this point.
      ((null candidates)
       (restagraph:log-message
         :debug
         (format nil "End of the line. Using the parent path ~{/~A~}" path))
       path)
      ;; If there's more than one candidate, return an error because this is illegal.
      ;; FIXME: report the path so the user can fix the problem.
      ((> (length candidates) 1)
       (error 'restagraph:integrity-error
              :message (format nil "More than one candidate supernet; this is not a valid situation.")))
      ;; Remaining option: there's one candidate. Test it.
      (t
       (restagraph:log-message
         :debug
         (format nil "Found a candidate supernet for ~A: recursing into ~{/~A~}."
                 subnet (append path (list (car candidates)))))
       (find-parent-subnet db subnet org vrfgroup (append path (list (car candidates))))))))

(defmethod insert-subnet ((db neo4cl:neo4j-rest-server)
                          (org string)
                          (vrf string)
                          (subnet string))
  (restagraph:log-message
    :debug
    (format nil "Attempting to insert subnet '~A' under organisation '~A' and VRF-group '~A'."
            subnet org vrf))
  ;; Sanity check: is it already there?
  (when (find-subnet db org vrf subnet)
    (error 'restagraph:client-error :message "Subnet already exists"))
  ;; Precalculate stuff
  (let* ((subnet-parts (cl-ppcre:split "/" subnet))
         (net-addr (restagraph::sanitise-uid (first subnet-parts)))
         (prefixlength (second subnet-parts))
         (parent-path (find-parent-subnet db subnet org vrf ()))
         ;; Pre-extract the list of candidate subnets for relocation.
         ;; If we do this now, we don't accidentally include the one we just inserted.
         (subnets
           (remove-if #'null
                      (mapcar #'(lambda (s)
                                  (when (parent-ipv4-subnet-p
                                          subnet
                                          (format nil "~A/~A"
                                                  (cdr (assoc :uid s))
                                                  (cdr (assoc :prefixlength s))))
                                    s))
                              (restagraph::get-resources
                                db
                                (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Subnets"
                                        org
                                        (if (equal vrf "")
                                          ""
                                          (format nil "/VrfGroups/vrfGroups/~A" vrf))
                                        parent-path)))))
         ;; Pre-extract the list of candidate addresses for relocation.
         (addresses
           (remove-if-not
             #'(lambda (a)
                 (parent-ipv4-subnet-p subnet (cdr (assoc :uid a))))
             (restagraph::get-resources
               db
               (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Addresses"
                       org
                       (if (equal vrf "")
                         ""
                         (format nil "/VrfGroups/vrfGroups/~A" vrf))
                       parent-path))))
         ;; Build the path to insert the new subnet
         (insert-path
           (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Subnets/ipv4Subnets"
                   org
                   (if (equal vrf "")
                     ""
                     (format nil "/VrfGroups/vrfGroups/~A" vrf))
                   parent-path)))
    ;; Insert the subnet
    (restagraph:log-message
      :debug
      (format nil "Inserting the subnet ~A/~A using the path ~A."
              net-addr prefixlength insert-path))
    (restagraph:store-dependent-resource db
                                         insert-path
                                         `(("uid" . ,net-addr)
                                           ("prefixlength" . ,prefixlength)))
    ;; Using the list of candidates we retrieved earlier,
    ;; identify subnets of the one we just inserted, and move them under it.
    (if subnets
      (mapcar #'(lambda (s)
                  (restagraph:log-message
                    :debug
                    (format nil "Relocating subnet ~A under its new parent." s))
                  ;; Now actually move it
                  (restagraph:move-dependent-resource
                    db
                    ;; Existing path of subnet
                    (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}"
                            org
                            (if (equal vrf "")
                              ""
                              (format nil "/VrfGroups/vrfGroups/~A" vrf))
                            (append parent-path (list (cdr (assoc :uid s)))))
                    ;; New parent for subnet
                    (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Subnets"
                            org
                            (if (equal vrf "")
                              ""
                              (format nil "/VrfGroups/vrfGroups/~A" vrf))
                            (append parent-path (list net-addr)))))
              subnets)
      (restagraph:log-message :debug "No subnets to relocate"))
    ;; Find all IP addresses directly attached to the supernet which fit in this subnet;
    ;; move them under this one.
    (if addresses
      (mapcar #'(lambda (a)
                  (restagraph:log-message
                    :debug
                    (format nil "Relocating address ~A under its new parent." a))
                  ;; Now actually move it
                  (restagraph:move-dependent-resource
                    db
                    ;; Existing path of subnet
                    (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Addresses/ipv4Addresses/~A"
                            org
                            (if (equal vrf "")
                              ""
                              (format nil "/VrfGroups/vrfGroups/~A" vrf))
                            parent-path
                            (cdr (assoc :uid a)))
                    ;; New parent for subnet
                    (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Addresses"
                            org
                            (if (equal vrf "")
                              ""
                              (format nil "/VrfGroups/vrfGroups/~A" vrf))
                            (append parent-path (list net-addr)))))
              addresses)
      (restagraph:log-message :debug "No addresses to relocate")))
  ;; Return nil, because the test suite expects this.
  ;; Seriously, there's nothing useful to actually report,
  ;; and we raise error conditions when anything goes wrong anyway.
  nil)

(defmethod delete-subnet ((db neo4cl:neo4j-rest-server)
                          (org string)
                          (vrf string)
                          (subnet string))
  (let* ((subnet-path (find-subnet db org vrf subnet))
         (parent-path (butlast subnet-path)))
    ;; Sanity-check
    (unless subnet-path
      (error 'restagraph:client-error :message "No such subnet."))
    ;; Move subnets to the parent
    (mapcar #'(lambda (s)
                (restagraph:log-message :debug (format nil "Relocating subnet ~A" s))
                (restagraph:move-dependent-resource
                  db
                  ;; Existing path
                  (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}"
                          org
                          (if (equal vrf "")
                              ""
                              (format nil "/VrfGroups/vrfGroups/~A" vrf))
                          (append subnet-path (list (cdr (assoc :uid s)))))
                  ;; New parent path
                  (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Subnets"
                          org
                          (if (equal vrf "")
                              ""
                              (format nil "/VrfGroups/vrfGroups/~A" vrf))
                          parent-path)))
            ;; Get a list of subnets
            (car
              (restagraph:get-resources
                db
                (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Subnets/ipv4Subnets"
                        org
                        (if (equal vrf "")
                            ""
                            (format nil "/VrfGroups/vrfGroups/~A" vrf))
                        subnet-path))))
    ;; Addresses: move them to the parent
    (mapcar #'(lambda (a)
                (restagraph:log-message :debug (format nil "Relocating address ~A" a))
                (restagraph:move-dependent-resource
                  db
                  ;; Existing path
                  (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Addresses/ipv4Addresses/~A"
                          org
                          (if (equal vrf "")
                              ""
                              (format nil "/VrfGroups/vrfGroups/~A" vrf))
                          subnet-path
                          (cdr (assoc :uid a)))
                  ;; New parent path
                  (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Addresses"
                          org
                          (if (equal vrf "")
                              ""
                              (format nil "/VrfGroups/vrfGroups/~A" vrf))
                          parent-path)))
            ;; Get a list of addresses
            (car
              (restagraph:get-resources
                db
                (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Addresses/ipv4Addresses"
                        org
                        (if (equal vrf "")
                            ""
                            (format nil "/VrfGroups/vrfGroups/~A" vrf))
                        subnet-path))))
    ;; Delete the subnet itself
    (restagraph:delete-resource-by-path
      db
      (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}"
              org
              (if (equal vrf "")
                  ""
                  (format nil "/VrfGroups/vrfGroups/~A" vrf))
              subnet-path)
      :delete-dependent t)))

(defmethod find-ipv4address ((db neo4cl:neo4j-rest-server)
                             (address string)
                             (org string)
                             (vrf string))
  (let ((parent-path (find-parent-subnet db address org vrf ())))
    (when parent-path
      (let ((result (restagraph:get-resources
                      db
                      (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Addresses/ipv4Addresses/~A"
                              org
                              (if (equal vrf "")
                                  ""
                                  (format nil "/VrfGroups/vrfGroups/~A" vrf))
                              parent-path
                              address))))
        (when result
          (append parent-path (list (cdr (assoc :uid result)))))))))

(defmethod insert-ipv4address ((db neo4cl:neo4j-rest-server)
                               (address string)
                               (org string)
                               (vrf string))
  (let ((parent-subnet (find-parent-subnet db address org vrf ())))
    ;; Sanity-check: is it already there?
    (if (restagraph:get-resources
          db
          (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Addresses/ipv4Addresses/~A"
                  org
                  (if (equal vrf "")
                      ""
                      (format nil "/VrfGroups/vrfGroups/~A" vrf))
                  parent-subnet
                  address))
        ;; It's a duplicate; raise an error
        (error 'restagraph:integrity-error :message "Address already exists.")
        ;; All is well; carry on
        (progn
          (restagraph:store-dependent-resource
            db
            (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Addresses/ipv4Addresses"
                    org
                    (if (equal vrf "")
                        ""
                        (format nil "/VrfGroups/vrfGroups/~A" vrf))
                    parent-subnet)
            `(("uid" . ,address)))
          ;; Return NIL, because there's no earthly reason to return anything else.
          nil))))

(defmethod delete-ipv4address ((db neo4cl:neo4j-rest-server)
                               (address string)
                               (org string)
                               (vrf string))
  (restagraph:delete-resource-by-path
    db
    (format nil "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Addresses/ipv4Addresses/~A"
            org
            (if (equal vrf "")
                ""
                (format nil "/VrfGroups/vrfGroups/~A" vrf))
            (find-parent-subnet db address org vrf ())
            address)
            :delete-dependent t)
  ;; Return NIL, because there's no earthly reason to return anything else.
  nil)
