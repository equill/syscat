(in-package #:syscat)

(defvar *syscat-acceptor*
  (make-instance 'restagraph::restagraph-acceptor
                 :address (getf *config-vars* :listen-address)
                 :port (getf *config-vars* :listen-port)
                 :url-base (getf *config-vars* ::url-base)
                 ;; Send all logs to STDOUT, and let Docker sort 'em out
                 :access-log-destination (make-synonym-stream 'cl:*standard-output*)
                 :message-log-destination (make-synonym-stream 'cl:*standard-output*)
                 ;; Datastore object - for specialising all the db methods on
                 :datastore (make-instance 'neo4cl:neo4j-rest-server
                                           :hostname (getf *config-vars* :dbhostname)
                                           :dbpasswd (getf *config-vars* :dbpasswd)
                                           :dbuser (getf *config-vars* :dbusername))))

;;; Define a logging method
(defmethod tbnl:acceptor-log-message ((acceptor restagraph::restagraph-acceptor)
                                      log-level
                                      format-string
                                      &rest format-arguments)
  (restagraph::log-message log-level (append (list format-string) format-arguments)))

(defun startup ()
  (restagraph::log-message :info "Starting up the restagraph application server")
  ;; Enforce the schema
  (restagraph::enforce-db-schema (restagraph::datastore *syscat-acceptor*))
  ;; Set the dispatch table
  (setf tbnl:*dispatch-table*
        (list
          (tbnl:create-prefix-dispatcher (getf *config-vars* :uri-base) 'restagraph::api-dispatcher-v1)
          (tbnl:create-prefix-dispatcher "/" 'four-oh-four)))
  ;; Start up the server
  (restagraph::log-message :info "Starting up Hunchentoot to serve HTTP requests")
  (handler-case
    (tbnl:start *syscat-acceptor*)
    (usocket:address-in-use-error
      () (restagraph::log-message
           :error
           (format nil "Attempted to start an already-running instance!")))))

(defun shutdown ()
  (restagraph::log-message
    :info
    (format nil "Shutting down the restagraph application server"))
  (handler-case
    (tbnl:stop *syscat-acceptor*)
    ;; Catch the case where it's already shut down
    (tbnl::unbound-slot
      ()
      (restagraph::log-message :info "Attempting to shut down Hunchentoot, but it's not running."))))
