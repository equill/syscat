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
              (format nil "/vrfGroups/~A" vrf)
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
       ;; Insert it
       (let ((result
               (insert-subnet (restagraph::datastore *syscat-acceptor*)
                              (tbnl:post-parameter "org")
                              (or (tbnl:post-parameter "vrf") "")
                              (if (ipaddress:ipv4-subnet-p (tbnl:post-parameter "subnet"))
                                  (ipaddress:make-ipv4-subnet (tbnl:post-parameter "subnet"))
                                  (ipaddress:make-ipv6-subnet (tbnl:post-parameter "subnet"))))))
         ;; Return it to the client for confirmation
         (if result
             (restagraph:log-message
               :debug
               (format nil "Stored subnet ~A. Now retrieving it for positive confirmation."
                       (tbnl:post-parameter "subnet")))
             (restagraph:log-message
               :debug
               (format nil "Subnet ~A was already present. Retrieving it for positive confirmation."
                       (tbnl:post-parameter "subnet"))))
         (setf (tbnl:content-type*) "application/json")
         (setf (tbnl:return-code*)
               (if result
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
                            (ipaddress:make-ipv6-subnet (tbnl:post-parameter "subnet")))))))
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
              (format nil "/vrfGroups/~A" vrf)
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
       (cl-json:encode-json-to-string
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
    (neo4cl:database-error (e) (restagraph::return-database-error e))))

(defun startup (&key docker)
  (if *syscat-acceptor*
      ;; There's an acceptor already in play; bail out.
      (restagraph:log-message :warn "Acceptor already exists; refusing to create a new one.")
      ;; No existing acceptor; we're good to go.
      (progn
        ;; Create the acceptor
        (defparameter *syscat-acceptor*
          (make-instance
            'restagraph::restagraph-acceptor
            :address (or (sb-ext:posix-getenv "SYSCAT_LISTEN_ADDR")
                         (getf restagraph::*config-vars* :listen-address))
            :port (or (when(sb-ext:posix-getenv "SYSCAT_LISTEN_PORT")
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
                         :dbpasswd (or (sb-ext:posix-getenv "SYSCAT_NEO4J_PASSWORD")
                                       (getf restagraph::*config-vars* :dbpasswd))
                         :dbuser (or (sb-ext:posix-getenv "SYSCAT_NEO4J_USER")
                                     (getf restagraph::*config-vars* :dbusername)))))
        (restagraph:log-message :info "Configuring the dispatch table")
        ;; Set the dispatch table
        (setf tbnl:*dispatch-table*
              (list
                (tbnl:create-prefix-dispatcher "/ipam/v1/subnets" 'subnet-dispatcher-v1)
                (tbnl:create-prefix-dispatcher "/ipam/v1/addresses" 'address-dispatcher-v1)
                (tbnl:create-prefix-dispatcher
                  (getf restagraph::*config-vars* :api-uri-base)
                  'restagraph::api-dispatcher-v1)
                (tbnl:create-prefix-dispatcher
                  (getf restagraph::*config-vars* :schema-uri-base)
                  'restagraph::schema-dispatcher-v1)
                (tbnl:create-prefix-dispatcher "/" 'restagraph::four-oh-four)))
        ;; Start up the server
        (restagraph:log-message
          :info
          (format nil "Starting up Hunchentoot to serve HTTP requests on ~A:~D"
                  (tbnl:acceptor-address *syscat-acceptor*)
                  (tbnl:acceptor-port *syscat-acceptor*)))
        (handler-case
          (tbnl:start *syscat-acceptor*)
          (usocket:address-in-use-error
            () (restagraph:log-message
                 :error
                 (format nil "Socket already in use"))))
        (when docker
          (sb-thread:join-thread
            (find-if
                    (lambda (th)
                      (string= (sb-thread:thread-name th)
                               (format nil "hunchentoot-listener-~A:~A"
                                       (tbnl:acceptor-address *syscat-acceptor*)
                                       (tbnl:acceptor-port *syscat-acceptor*))))
                    (sb-thread:list-all-threads)))))))

(defun dockerstart ()
  (startup :docker t))

(defun save-image (&optional (path "/tmp/syscat"))
  (sb-ext:save-lisp-and-die path :executable t :toplevel 'syscat::dockerstart))

(defun shutdown ()
  ;; Check whether there's something to shut down
  (if *syscat-acceptor*
      ;; There is; go ahead
      (progn
        (restagraph:log-message
          :info
          (format nil "Shutting down the restagraph application server"))
        (handler-case
          ;; Perform a soft shutdown: finish serving any requests in flight
          (tbnl:stop *syscat-acceptor* :soft t)
          ;; Catch the case where it's already shut down
          (tbnl::unbound-slot
            ()
            (restagraph:log-message :info "Attempting to shut down Hunchentoot, but it's not running.")))
        ;; Nuke the acceptor
        (setf *syscat-acceptor* nil))
      ;; No acceptor. Note the fact and do nothing.
      (restagraph:log-message :warn "No acceptor present, so nothing to shut down.")))
