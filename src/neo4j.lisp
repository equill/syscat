;;;; Classes and methods specific to neo4j

(in-package #:syscat)


;;; Utilities

(defun extract-data-from-get-request (response)
  "Reach into the copious data returned by a get-x request,
   and return only the actual content."
  ;; The row data, ignoring the metadata
  (car
    (cdr
      (assoc :row
             (car
               ;; Only the data section, ignoring the columns list
               (cdr
                 (assoc :data
                        (car
                          ;; Just the results section
                          (cdr (assoc :results
                                      ;; The actual query
                                      response))))))))))


;;; Methods - implementing the generics

;;; IPAM

(defmethod store-ipv4-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  ;; First, validate the address
  ;; Then convert it to a 32-bit integer and store it
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . "CREATE (i:ipv4Addr {properties}) RETURN i")
         (:PARAMETERS .
                      ((:properties .
                                    ((:address . ,(format nil "~A" (cl-cidr-notation:parse-ip address))))))))))))


(defmethod get-ipv4-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  ;; Assert which path we want to take in the event of an error
  (handler-bind
    ;; For now, I just want to see what went wrong
    ((neo4cl:neo4j-transaction-error #'(lambda (e) (invoke-restart 'report-error e))))
    ;; The thing we actually wanted to do
    ;; Extract just the data, ignoring everything else
    (let* ((response-data
             (extract-data-from-get-request
               (neo4cl:neo4j-transaction
                 endpoint
                 `((:STATEMENTS
                     ((:STATEMENT . ,(format nil "MATCH (i:ipv4Addr {address: '~A'}) RETURN i"
                                             (cl-cidr-notation:parse-ip address)))))))))
           (address (cdr (assoc :address response-data))))
      ;; Convert it back to dotted-quad notation before returning it.
      (cl-cidr-notation:ip-string (parse-integer address)))))
