;;;; Functions exposed by the REST API server application

(in-package #:syscat)

(defun four-oh-four ()
  "Fallthrough handler, for anything we haven't already defined."
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-not-found+)
  "This is not a valid URI")

(defun ipv4-address ()
  (cond
    ;; GET = return an address' details
    ((equal (tbnl:request-method*) :GET)
     ;; Check whether the client supplied an address
     (let ((address  (fourth (cl-ppcre:split "/" (tbnl:request-uri*)))))
       ;; If they did, try to retrieve it
       (log-message :debug (format nil "Request received for IP address '~A'" address))
       (if address
         (let ((result (get-ipv4-address (datastore tbnl:*acceptor*) address)))
           (if result
             ;; It exists; return the details
             (progn
               (setf (tbnl:content-type*) "application/json")
               result)
             ;; It's not there. Inform the client of the sad news.
             (progn
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-not-found+)
               (format nil "Address ~A not found" address))))
         ;; If they didn't, complain
         (progn
           (setf (tbnl:content-type*) "text/plain")
           (setf (tbnl:return-code*) tbnl:+http-bad-request+)
           (format nil "An address must be specified")))))
    ;; POST = create an address
    ((equal (tbnl:request-method*) :POST)
     ;; If the client supplied an address, try to create it
     (if (tbnl:post-parameter "address")
       (progn
         (log-message :debug
                      (format nil
                              "Creating IPv4 address ~A"
                              (tbnl:post-parameter "address")))
         (let ((result (syscat:store-ipv4-address
                         (datastore tbnl:*acceptor*)
                         (tbnl:post-parameter "address"))))
           (if result
             (format nil
                     "http://~A:~A~A~A"
                     (tbnl:acceptor-address *syscat-acceptor*)
                     (tbnl:acceptor-port *syscat-acceptor*)
                     (tbnl:request-uri*) (tbnl:post-parameter "address")))))
       ;; If they didn't, complain
       (progn
         (log-message :debug (format nil "No address was supplied"))
         (setf (tbnl:content-type*) "text/plain")
         (setf (tbnl:return-code*) tbnl:+http-not-found+)
         (format nil "Parameter 'address' must be supplied"))))
    ;; DELETE = remove an address
    ((equal (tbnl:request-method*) :DELETE)
     ;; Check whether the client supplied an IP address
     (let ((address  (fourth (cl-ppcre:split "/" (tbnl:request-uri*)))))
       ;; If they did, try to delete it
       (if address
         (progn
           (log-message :info (format nil "Deleting IP address ~A" address))
           (delete-ipv4-address (datastore tbnl:*acceptor*) address)
           (setf (tbnl:content-type*) "text/plain")
           (format nil "Success"))
         ;; If they didn't, complain
         (progn
           (setf (tbnl:content-type*) "text/plain")
           (setf (tbnl:return-code*) tbnl:+http-not-found+)
           (format nil "An address must be specified")))))
    ;; Default case
    (t
      (setf (tbnl:return-code*) tbnl:+http-method-not-allowed+)
      "Method not supported")))

(defun device ()
  (cond
    ;; GET = return a device's details
    ((equal (tbnl:request-method*) :GET)
     ;; We need a hostname to work from
     (let ((hostname  (fourth (cl-ppcre:split "/" (tbnl:request-uri*)))))
       (log-message :debug (format nil "Request received for hostname '~A'" hostname))
       (if hostname
         (progn
           (log-message :info "Requested device: ~A" hostname)
           (let ((devicedetails (get-device (datastore tbnl:*acceptor*) hostname)))
             (if devicedetails
               (progn
                 (setf (tbnl:content-type*) "application/json")
                 devicedetails)
               (progn
                 (setf (tbnl:content-type*) "text/plain")
                 (setf (tbnl:return-code*) tbnl:+http-not-found+)
                 "No device found by that name."))))
         ;; No hostname, no help
         (progn
           (log-message :info "No hostname specified")
           "'hostname' is a required argument"))))
    ;; POST = create a device
    ((equal (tbnl:request-method*) :POST)
     (let ((hostname (cdr (assoc "hostname" (tbnl:post-parameters*) :test #'equal))))
       (if hostname
         (progn
           (log-message :info "Received request to create device: ~A" hostname)
           (progn
             (setf (tbnl:content-type*) "application/json")
             (let ((devicedetails (store-device (datastore tbnl:*acceptor*) hostname)))
               (when devicedetails
                 (format nil "http://~A:~A~A~A"
                         (tbnl:acceptor-address *syscat-acceptor*)
                         (tbnl:acceptor-port *syscat-acceptor*)
                         (tbnl:request-uri*)
                         (hostname))))))
         (progn
           (log-message :info "No hostname specified")
           "'hostname' is a required argument"))))
    ;; DELETE = delete a device
    ((equal (tbnl:request-method*) :DELETE)
     (let ((hostname (cdr (assoc "hostname" (tbnl:post-parameters*) :test #'equal))))
       (if hostname
         (progn
           (log-message :info "Received request to delete device: ~A" hostname)
           (progn
             (setf (tbnl:content-type*) "application/json")
             (let ((devicedetails (delete-device (datastore tbnl:*acceptor*) hostname)))
               devicedetails)))
         (progn
           (log-message :info "No hostname specified")
           "'hostname' is a required argument"))))
    (t
      (setf (tbnl:return-code*) tbnl:+http-method-not-allowed+)
      "Method not supported")))

;; Dispatch them
(setf tbnl:*dispatch-table*
      (list (tbnl:create-prefix-dispatcher "/v1/ipv4-addresses/" 'ipv4-address)
            (tbnl:create-prefix-dispatcher "/v1/devices/" 'device)
            ;; Fallback.
            ;; This must be last, because they're inspected in order,
            ;; first match wins.
            (tbnl:create-prefix-dispatcher "/" 'four-oh-four)))
