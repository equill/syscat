;;;; The REST API server application

(in-package #:syscat)

(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 4949
    :datastore ,(make-instance 'neo4cl:neo4j-rest-server
                               :dbpasswd "wallaby")))

;;; Customised Hunchentoot acceptor.
;;; Carries information about the datastore being used.
(defclass syscat-acceptor (tbnl:easy-acceptor)
  ((datastore :initarg :datastore
              :reader datastore
              :initform (error "Datastore object must be supplied.")
              :documentation "An object representing the datastore, on which the generic functions will be dispatched.")
   (url-base :initarg :url-base
             :reader url-base
             :initform "localhost"))
  (:default-initargs :address "127.0.0.1")
  (:documentation "vhost object, subclassed from tbnl:easy-acceptor"))

;;; We can't directly check whether this acceptor is running,
;;; so we're using the existence of its special variable as a proxy.
(defparameter *syscat-acceptor*
  (make-instance 'syscat-acceptor
                 :address (getf *config-vars* :listen-address)
                 :port (getf *config-vars* :listen-port)
                 :url-base (getf *config-vars* ::url-base)
                 :datastore (getf *config-vars* :datastore)))

;;; Tell Hunchentoot to extract POST parameters for PUT and DELETE requests
(push :PUT tbnl:*methods-for-post-parameters*)
(push :DELETE tbnl:*methods-for-post-parameters*)

(defun startup ()
  (format t "Starting up the Syscat application server")
  (handler-case
    (tbnl:start *syscat-acceptor*)
    (usocket:address-in-use-error
      () (format t "Attempted to start an already-running instance!"))))

(defun shutdown ()
  (format t "Shutting down the Syscat application server")
  (tbnl:stop *syscat-acceptor*))
