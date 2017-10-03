(in-package #:syscat)

(defvar *syscat-acceptor*
  (make-instance 'restagraph::restagraph-acceptor
                 :address (getf restagraph::*config-vars* :listen-address)
                 :port (getf restagraph::*config-vars* :listen-port)
                 :url-base (getf restagraph::*config-vars* ::url-base)
                 ;; Send all logs to STDOUT, and let Docker sort 'em out
                 :access-log-destination (make-synonym-stream 'cl:*standard-output*)
                 :message-log-destination (make-synonym-stream 'cl:*standard-output*)
                 ;; Datastore object - for specialising all the db methods on
                 :datastore (make-instance 'neo4cl:neo4j-rest-server
                                           :hostname (getf restagraph::*config-vars* :dbhostname)
                                           :dbpasswd (getf restagraph::*config-vars* :dbpasswd)
                                           :dbuser (getf restagraph::*config-vars* :dbusername))))

;;; Define a logging method
(defmethod tbnl:acceptor-log-message ((acceptor restagraph::restagraph-acceptor)
                                      log-level
                                      format-string
                                      &rest format-arguments)
  (restagraph:log-message log-level (append (list format-string) format-arguments)))

(defun format-subnet (asn vrf subnet-list)
  (format nil "/asn/~A~A~{/Subnets/ipv4Subnets/~A~}"
          asn
          (if vrf (format nil "/~A" vrf) "")
          subnet-list))

(defun subnet-dispatcher-v1 ()
  "Hunchentoot dispatch function for the IPAM-specific REST API, version 1. Subnet subset."
  (handler-case
    (cond
      ;; Methods we don't support.
      ;; Take the whitelist approach
      ((not (member (tbnl:request-method*) '(:POST :GET :DELETE)))
       (restagraph::method-not-allowed))
      ;;
      ;; Create a subnet
      ((and (equal (tbnl:request-method*) :POST)
            (tbnl:post-parameter "subnet")
            (tbnl:post-parameter "asn"))
       (restagraph:log-message :debug
                               (format nil "Dispatching POST request for URI ~A"
                                       (tbnl:request-uri*)))
       ;; Insert it
       (insert-subnet (restagraph::datastore *syscat-acceptor*)
                      (tbnl:post-parameter "asn")
                      (or (tbnl:post-parameter "vrf") "")
                      (tbnl:post-parameter "subnet"))
       ;; Return it to the client for confirmation
       (restagraph:log-message
         :debug
         (format nil "Stored subnet ~A. Now retrieving it for positive confirmation."
                 (tbnl:post-parameter "subnet")))
       (setf (tbnl:content-type*) "application/json")
       (setf (tbnl:return-code*) tbnl:+http-created+)
       (format-subnet
         (tbnl:post-parameter "asn")
         (tbnl:post-parameter "vrf")
         (find-subnet (restagraph::datastore *syscat-acceptor*)
                      (tbnl:post-parameter "asn")
                      (or (tbnl:post-parameter "vrf") "")
                      (tbnl:post-parameter "subnet"))))
      ;;
      ;; Search for a subnet
      ((and (equal (tbnl:request-method*) :GET)
            (tbnl:get-parameter "subnet")
            (tbnl:get-parameter "asn"))
       (restagraph:log-message :debug
                               (format nil "Dispatching GET request for URI ~A"
                                       (tbnl:request-uri*)))
       ;; Go look for it
       (handler-case
         (let ((result (find-subnet (restagraph::datastore *syscat-acceptor*)
                                    (tbnl:get-parameter "asn")
                                    (or (tbnl:get-parameter "vrf") "")
                                    (tbnl:get-parameter "subnet"))))
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
                 (format-subnet
                   (tbnl:post-parameter "asn")
                   (tbnl:post-parameter "vrf")
                   result))))
         ;; Attempted violation of db integrity
         (restagraph:integrity-error (e) (restagraph::return-integrity-error (restagraph:message e)))
         ;; Generic client errors
         (restagraph:client-error (e) (restagraph::return-client-error (restagraph:message e)))))
      ;;
      ;; Delete a subnet
      ((and (equal (tbnl:request-method*) :DELETE)
            (tbnl:post-parameter "subnet")
            (tbnl:post-parameter "asn"))
       (restagraph:log-message :debug
                               (format nil "Dispatching DELETE request for URI ~A"
                                       (tbnl:request-uri*)))
       (delete-subnet (restagraph::datastore *syscat-acceptor*)
                      (tbnl:post-parameter "asn")
                      (or (tbnl:post-parameter "vrf") "")
                      (tbnl:post-parameter "subnet"))
       (setf (tbnl:content-type*) "text/plain")
       (setf (tbnl:return-code*) tbnl:+http-no-content+)
       "")
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

(defun address-dispatcher-v1 ()
  "Hunchentoot dispatch function for the IPAM-specific REST API, version 1. Address subset."
  (handler-case
    (cond
      ;; Methods we don't support.
      ;; Take the whitelist approach
      ((not (member (tbnl:request-method*) '(:POST :GET :DELETE)))
       (restagraph::method-not-allowed))
      ;;
      ;; Create an address
      ((and (equal (tbnl:request-method*) :POST)
            (tbnl:post-parameter "address")
            (tbnl:post-parameter "asn"))
       (restagraph:log-message
         :debug (format nil "Dispatching POST request for URI ~A" (tbnl:request-uri*)))
       ;; Insert it
       (insert-ipv4address (restagraph::datastore *syscat-acceptor*)
                           (tbnl:post-parameter "address")
                           (tbnl:post-parameter "asn")
                           (or (tbnl:post-parameter "vrf") ""))
       ;; Return it to the client for confirmation
       (restagraph:log-message
         :debug
         (format nil "Stored address ~A. Now retrieving it for positive confirmation."
                 (tbnl:post-parameter "address")))
       (setf (tbnl:content-type*) "application/json")
       (setf (tbnl:return-code*) tbnl:+http-created+)
       (cl-json:encode-json-to-string
         (find-ipv4address (restagraph::datastore *syscat-acceptor*)
                           (tbnl:post-parameter "address")
                           (tbnl:post-parameter "asn")
                           (or (tbnl:post-parameter "vrf") ""))))
      ;;
      ;; Search for an address
      ((and (equal (tbnl:request-method*) :GET)
            (tbnl:get-parameter "address")
            (tbnl:get-parameter "asn"))
       (restagraph:log-message
         :debug (format nil "Dispatching GET request for URI ~A" (tbnl:request-uri*)))
       ;; Go look for it
       (handler-case
         (let ((result (find-ipv4address (restagraph::datastore *syscat-acceptor*)
                                         (tbnl:get-parameter "address")
                                         (tbnl:get-parameter "asn")
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
                 (cl-json:encode-json-to-string result))))
         ;; Attempted violation of db integrity
         (restagraph:integrity-error (e) (restagraph::return-integrity-error (restagraph:message e)))
         ;; Generic client errors
         (restagraph:client-error (e) (restagraph::return-client-error (restagraph:message e)))))
      ;;
      ;; Delete an address
      ((and (equal (tbnl:request-method*) :DELETE)
            (tbnl:post-parameter "address")
            (tbnl:post-parameter "asn"))
       (restagraph:log-message :debug
                               (format nil "Dispatching DELETE request for URI ~A"
                                       (tbnl:request-uri*)))
       (delete-ipv4address (restagraph::datastore *syscat-acceptor*)
                           (tbnl:post-parameter "address")
                           (tbnl:post-parameter "asn")
                           (or (tbnl:post-parameter "vrf") ""))
       (setf (tbnl:content-type*) "text/plain")
       (setf (tbnl:return-code*) tbnl:+http-no-content+)
       "")
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
  (restagraph:log-message :info "Starting up the restagraph application server")
  ;; Set the dispatch table
  (setf tbnl:*dispatch-table*
        (list
          (tbnl:create-prefix-dispatcher "/ipam/v1/subnets" 'subnet-dispatcher-v1)
          (tbnl:create-prefix-dispatcher "/ipam/v1/addresses" 'address-dispatcher-v1)
          (tbnl:create-prefix-dispatcher (getf restagraph::*config-vars* :uri-base) 'restagraph::api-dispatcher-v1)
          (tbnl:create-prefix-dispatcher "/" 'restagraph::four-oh-four)))
  ;; Start up the server
  (restagraph:log-message :info "Starting up Hunchentoot to serve HTTP requests")
  (handler-case
    (tbnl:start *syscat-acceptor*)
    (usocket:address-in-use-error
      () (restagraph:log-message
           :error
           (format nil "Attempted to start an already-running instance!"))))
  (when docker
    (sb-thread:join-thread
      (find-if
        (lambda (th)
          (string= (sb-thread:thread-name th)
                   (format nil "hunchentoot-listener-localhost:~A"
                           (getf restagraph::*config-vars* :listen-port))))
        (sb-thread:list-all-threads)))))

(defun dockerstart ()
  (startup :docker t))

(defun save-image (&optional (path "/tmp/syscat"))
  (sb-ext:save-lisp-and-die path :executable t :toplevel 'syscat::dockerstart))

(defun shutdown ()
  (restagraph:log-message
    :info
    (format nil "Shutting down the restagraph application server"))
  (handler-case
    (tbnl:stop *syscat-acceptor*)
    ;; Catch the case where it's already shut down
    (tbnl::unbound-slot
      ()
      (restagraph:log-message :info "Attempting to shut down Hunchentoot, but it's not running."))))
