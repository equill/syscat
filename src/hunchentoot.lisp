(in-package #:syscat)

;; Default initial state, to make sure 'startup has something to work from
(defvar *syscat-acceptor* nil)

;;; Define a logging method
(defmethod tbnl:acceptor-log-message ((acceptor restagraph::restagraph-acceptor)
                                      log-level
                                      format-string
                                      &rest format-arguments)
  (restagraph:log-message log-level (append (list format-string) format-arguments)))

(defun format-subnet-path (org vrf subnet-list)
  "Generate a URI for the path to a subnet."
  (format nil
          (if (= 6 (ipaddress:ip-version (car subnet-list)))
              "/organisations/~A~A~{/Subnets/ipv6Subnets/~A~}"
              "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}")
          org
          (if (and vrf (not (equal vrf "")))
              (format nil "/VrfGroups/vrfGroups/~A" vrf)
              "")
          (mapcar #'make-subnet-uid subnet-list)))

(defmethod make-subnet-uid ((subnet ipaddress:ip-address))
  (with-output-to-string (str)
    (write (ipaddress:as-string subnet) :stream str :escape nil)
    (write "_" :stream str :escape nil)
    (write (ipaddress:prefix-length subnet) :stream str)))

(defun subnet-dispatcher-v1 ()
  "Hunchentoot dispatch function for the IPAM-specific REST API, version 1. Subnet subset."
  (handler-case
    (cond
      ;;
      ;; Create a subnet
      ((and (equal (tbnl:request-method*) :POST)
            (tbnl:post-parameter "subnet")
            (tbnl:post-parameter "org"))
       (restagraph:log-message :debug
                               (format nil "Dispatching POST request for URI ~A"
                                       (tbnl:request-uri*)))
       (cond
         ;; Sanity-check: does the organisation exist?
         ((not (restagraph:get-resources
                 (restagraph::datastore *syscat-acceptor*)
                 (concatenate 'string "/organisations/" (tbnl:post-parameter "org"))))
          (restagraph::return-client-error
            (format nil "Organisation '~A' does not exist" (tbnl:post-parameter "org"))))
         ;; It's passed all the sanity checks so far; insert it
         (t
           (let ((result
                   (insert-subnet (restagraph::datastore *syscat-acceptor*)
                                  (tbnl:post-parameter "org")
                                  (or (tbnl:post-parameter "vrf") "")
                                  (if (ipaddress:ipv4-subnet-p (tbnl:post-parameter "subnet"))
                                    (ipaddress:make-ipv4-subnet (tbnl:post-parameter "subnet"))
                                    (ipaddress:make-ipv6-subnet (tbnl:post-parameter "subnet"))))))
             ;; Return it to the client for confirmation
             (restagraph:log-message
               :debug
               (format nil
                       (if result
                         "Stored subnet ~A. Now retrieving it for positive confirmation."
                         "Subnet ~A was already present. Retrieving it for positive confirmation.")
                       (tbnl:post-parameter "subnet")))
             (setf (tbnl:content-type*) "application/json")
             (setf (tbnl:return-code*) (if result
                                         tbnl:+http-created+
                                         tbnl:+http-ok+))
             (format-subnet-path
               (tbnl:post-parameter "org")
               (tbnl:post-parameter "vrf")
               (find-subnet (restagraph::datastore *syscat-acceptor*)
                            (tbnl:post-parameter "org")
                            (or (tbnl:post-parameter "vrf") "")
                            (if (ipaddress:ipv4-subnet-p (tbnl:post-parameter "subnet"))
                              (ipaddress:make-ipv4-subnet (tbnl:post-parameter "subnet"))
                              (ipaddress:make-ipv6-subnet (tbnl:post-parameter "subnet")))))))))
      ;;
      ;; Search for a subnet
      ((and (equal (tbnl:request-method*) :GET)
            (tbnl:get-parameter "subnet")
            (tbnl:get-parameter "org"))
       (restagraph:log-message :debug
                               (format nil "Dispatching GET request for URI ~A"
                                       (tbnl:request-uri*)))
       ;; Go look for it
       (handler-case
         (let ((result (find-subnet (restagraph::datastore *syscat-acceptor*)
                                    (tbnl:get-parameter "org")
                                    (or (tbnl:get-parameter "vrf") "")
                                    (if (ipaddress:ipv4-subnet-p (tbnl:get-parameter "subnet"))
                                      (ipaddress:make-ipv4-subnet (tbnl:get-parameter "subnet"))
                                      (ipaddress:make-ipv6-subnet (tbnl:get-parameter "subnet"))))))
           ;; Did we find one?
           (if (or (null result)
                   (equal result ""))
             ;; Not found
             (progn
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-not-found+)
               "No such subnet")
             ;; Found it!
             (progn
               (setf (tbnl:content-type*) "application/json")
               (setf (tbnl:return-code*) tbnl:+http-ok+)
               (let ((output (format-subnet-path
                               (tbnl:get-parameter "org")
                               (tbnl:get-parameter "vrf")
                               result)))
                 (restagraph:log-message :debug "Retrieved subnet path ~A" output)
                 ;; Actually return it to the appserver
                 output))))
         ;; Attempted violation of db integrity
         (restagraph:integrity-error (e) (restagraph::return-integrity-error (restagraph:message e)))
         ;; Generic client errors
         (restagraph:client-error (e) (restagraph::return-client-error (restagraph:message e)))))
      ;;
      ;; Delete a subnet
      ((and (equal (tbnl:request-method*) :DELETE)
            (tbnl:post-parameter "subnet")
            (tbnl:post-parameter "org"))
       (restagraph:log-message :debug
                               (format nil "Dispatching DELETE request for URI ~A"
                                       (tbnl:request-uri*)))
       (delete-subnet (restagraph::datastore *syscat-acceptor*)
                      (tbnl:post-parameter "org")
                      (or (tbnl:post-parameter "vrf") "")
                      (if (ipaddress:ipv4-subnet-p (tbnl:post-parameter "subnet"))
                        (ipaddress:make-ipv4-subnet (tbnl:post-parameter "subnet"))
                        (ipaddress:make-ipv6-subnet (tbnl:post-parameter "subnet"))))
       (setf (tbnl:content-type*) "text/plain")
       (setf (tbnl:return-code*) tbnl:+http-no-content+)
       "")
      ;; Methods we don't support.
      ;; Take the whitelist approach
      ((not (member (tbnl:request-method*) '(:POST :GET :DELETE)))
       (restagraph::method-not-allowed))
      ;;
      ;; Handle all other cases
      (t
        (restagraph::return-client-error "This wasn't a valid request")))
    ;; Handle general errors
    ;;
    ;; Generic client errors
    (restagraph:client-error (e) (restagraph::return-client-error (restagraph:message e)))
    (neo4cl:client-error (e) (restagraph::return-client-error (neo4cl:message e)))
    ;; Transient error
    (neo4cl:transient-error (e) (restagraph::return-transient-error e))
    ;; Database error
    (neo4cl:database-error (e) (restagraph::return-database-error e))))

(defun format-address-path (org vrf address-path)
  "Generate a URI for the path to an address."
  (format nil
          (if (= 6 (ipaddress:ip-version (car address-path)))
              "/organisations/~A~A~{/Subnets/ipv6Subnets/~A~}/Addresses/ipv6Addresses/~A"
              "/organisations/~A~A~{/Subnets/ipv4Subnets/~A~}/Addresses/ipv4Addresses/~A")
          org
          (if (and vrf (not (equal vrf "")))
              (format nil "/VrfGroups/vrfGroups/~A" vrf)
              "")
          (mapcar #'make-subnet-uid (butlast address-path))
          (car (last address-path))))

(defun address-dispatcher-v1 ()
  "Hunchentoot dispatch function for the IPAM-specific REST API, version 1. Address subset."
  (handler-case
    (cond
      ;;
      ;; Create an address
      ((and (equal (tbnl:request-method*) :POST)
            (tbnl:post-parameter "address")
            (tbnl:post-parameter "org"))
       (restagraph:log-message
         :debug (format nil "Dispatching POST request for URI ~A" (tbnl:request-uri*)))
       (insert-ipaddress (restagraph::datastore *syscat-acceptor*)
                         (make-instance (if (ipaddress:ipv4-address-p (tbnl:post-parameter "address"))
                                          'ipaddress:ipv4-address
                                          'ipaddress:ipv6-address)
                                        :address (tbnl:post-parameter "address"))
                         (tbnl:post-parameter "org")
                         (or (tbnl:post-parameter "vrf") ""))
       ;; Return it to the client for confirmation
       (restagraph:log-message
         :debug
         (format nil "Stored address ~A. Now retrieving it for positive confirmation."
                 (tbnl:post-parameter "address")))
       (setf (tbnl:content-type*) "application/json")
       (setf (tbnl:return-code*) tbnl:+http-created+)
       (format-address-path (tbnl:post-parameter "org")
                            (or (tbnl:post-parameter "vrf") "")
                            (find-ipaddress (restagraph::datastore *syscat-acceptor*)
                                            (make-instance (if (ipaddress:ipv4-address-p (tbnl:post-parameter "address"))
                                                             'ipaddress:ipv4-address
                                                             'ipaddress:ipv6-address)
                                                           :address (tbnl:post-parameter "address"))
                                            (tbnl:post-parameter "org")
                                            (or (tbnl:post-parameter "vrf") ""))))
      ;;
      ;; Search for an address
      ((and (equal (tbnl:request-method*) :GET)
            (tbnl:get-parameter "address")
            (tbnl:get-parameter "org"))
       (restagraph:log-message
         :debug (format nil "Dispatching GET request for URI ~A" (tbnl:request-uri*)))
       ;; Go look for it
       (handler-case
         (let ((result (find-ipaddress
                         (restagraph::datastore *syscat-acceptor*)
                         (make-instance (if (ipaddress:ipv4-address-p (tbnl:get-parameter "address"))
                                            'ipaddress:ipv4-address
                                            'ipaddress:ipv6-address)
                                        :address (tbnl:get-parameter "address"))
                         (tbnl:get-parameter "org")
                         (or (tbnl:get-parameter "vrf") ""))))
           ;; Did we find one?
           (if (or (null result)
                   (equal result ""))
               ;; Not found
               (progn
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-not-found+)
                 "No such address")
               ;; Found it!
               (progn
                 (setf (tbnl:content-type*) "application/json")
                 (setf (tbnl:return-code*) tbnl:+http-ok+)
                 (format-address-path (tbnl:get-parameter "org")
                                      (or (tbnl:get-parameter "vrf") "")
                                      result))))
         ;; Attempted violation of db integrity
         (restagraph:integrity-error (e) (restagraph::return-integrity-error (restagraph:message e)))
         ;; Generic client errors
         (restagraph:client-error (e) (restagraph::return-client-error (restagraph:message e)))))
      ;;
      ;; Delete an address
      ((and (equal (tbnl:request-method*) :DELETE)
            (tbnl:post-parameter "address")
            (tbnl:post-parameter "org"))
       (restagraph:log-message :debug
                               (format nil "Dispatching DELETE request for URI ~A"
                                       (tbnl:request-uri*)))
       (delete-ipaddress (restagraph::datastore *syscat-acceptor*)
                           (make-instance (if (ipaddress:ipv4-address-p (tbnl:post-parameter "address"))
                                            'ipaddress:ipv4-address
                                            'ipaddress:ipv6-address)
                                        :address (tbnl:post-parameter "address"))
                           (tbnl:post-parameter "org")
                           (or (tbnl:post-parameter "vrf") ""))
       (setf (tbnl:content-type*) "text/plain")
       (setf (tbnl:return-code*) tbnl:+http-no-content+)
       "")
      ;; Reject any methods we don't support.
      ;; Take the whitelist approach
      ((not (member (tbnl:request-method*) '(:POST :GET :DELETE)))
       (restagraph::method-not-allowed))
      ;;
      ;; Handle all other cases
      (t
        (restagraph::return-client-error "This wasn't a valid request")))
    ;; Handle general errors
    ;;
    ;; Generic client errors
    (neo4cl:client-error (e) (restagraph::return-client-error (neo4cl:message e)))
    ;; Transient error
    (neo4cl:transient-error (e) (restagraph::return-transient-error e))
    ;; Database error
    (neo4cl:database-error (e) (restagraph::return-database-error e))
    ;; Service errors, e.g. connection refused
    (neo4cl:service-error (e) (restagraph::return-service-error (neo4cl:message e)))))

(defun make-syscat-acceptor ()
  (make-instance
    'restagraph::restagraph-acceptor
    :address (or (sb-ext:posix-getenv "SYSCAT_LISTEN_ADDR")
                 (getf restagraph::*config-vars* :listen-address))
    :port (or (when (sb-ext:posix-getenv "SYSCAT_LISTEN_PORT")
                (parse-integer (sb-ext:posix-getenv "SYSCAT_LISTEN_PORT")))
              (getf restagraph::*config-vars* :listen-port))
    ;; Send all logs to STDOUT, and let Docker sort 'em out
    :access-log-destination (make-synonym-stream 'cl:*standard-output*)
    :message-log-destination (make-synonym-stream 'cl:*standard-output*)
    ;; Datastore object - for specialising all the db methods on
    :datastore (make-instance
                 'neo4cl:neo4j-rest-server
                 :hostname (or (sb-ext:posix-getenv "SYSCAT_NEO4J_HOSTNAME")
                               (getf restagraph::*config-vars* :dbhostname))
                 :port (or (sb-ext:posix-getenv "SYSCAT_NEO4J_PORT")
                               (getf restagraph::*config-vars* :dbport))
                 :dbname (or (sb-ext:posix-getenv "SYSCAT_NEO4J_DBNAME")
                               (getf restagraph::*config-vars* :dbname))
                 :dbpasswd (or (sb-ext:posix-getenv "SYSCAT_NEO4J_PASSWORD")
                               (getf restagraph::*config-vars* :dbpasswd))
                 :dbuser (or (sb-ext:posix-getenv "SYSCAT_NEO4J_USER")
                             (getf restagraph::*config-vars* :dbusername)))))

(defun startup (&key docker schemapath)
  ;; This needs to be a dynamic variable for the dispatcher to grab
  (defparameter *syscat-acceptor*
    (make-syscat-acceptor))
  ;; Having set the dynamic variable, invoke the appserver
  (restagraph:startup
    :acceptor *syscat-acceptor*
    :dispatchers (list
                   (tbnl:create-prefix-dispatcher "/ipam/v1/subnets" 'subnet-dispatcher-v1)
                   (tbnl:create-prefix-dispatcher "/ipam/v1/addresses" 'address-dispatcher-v1))
    :docker docker
    :schemapath (cond
                  ;; Were we passed one explicitly?
                  (schemapath
                    schemapath)
                  ;; Is one set via an environment variable?
                  ((sb-ext:posix-getenv "SCHEMAPATH")
                   (sb-ext:posix-getenv "SCHEMAPATH"))
                  ;; Default case
                  (t
                    nil))))

(defun dockerstart (&key schemapath)
  (if schemapath
    (startup :docker t :schemapath schemapath)
    (startup :docker t)))

(defun shutdown ()
  (restagraph:shutdown))
