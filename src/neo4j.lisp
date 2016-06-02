;;;; Classes and methods specific to neo4j

(in-package #:syscat)


;;; Utilities

(defun extract-data-from-get-request (response)
  "Reach into the copious data returned by a get-x request,
   and return only the actual content."
  ;; Extract the data portion of the response
  (let ((data (cdr
                (assoc :data
                       (car
                         ;; Just the results section
                         (cdr (assoc :results
                                     ;; The actual query
                                     response)))))))
    ;; If there actually is data to be inspected, extract just the row portion.
    ;; If there isn't, return NIL.
    (when data
      (car
        (cdr
          (assoc :row
                 (car data)))))))


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
  ;; Extract just the data, ignoring everything else
  (let* ((response-data
           (extract-data-from-get-request
             (neo4cl:neo4j-transaction
               endpoint
               `((:STATEMENTS
                   ((:STATEMENT . ,(format nil "MATCH (i:ipv4Addr {address: '~A'}) RETURN i"
                                           (cl-cidr-notation:parse-ip address)))))))))
         ;; Extract the address from the response, but only if
         ;; there's something to inspect
         (address (when response-data
                    (cdr (assoc :address response-data)))))
    ;; If an address was returned, convert it back to dotted-quad notation
    (when address
      (cl-cidr-notation:ip-string (parse-integer address)))))

(defmethod delete-ipv4-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . ,(format nil "MATCH (i:ipv4Addr { address: '~A' }) DELETE i"
                                (cl-cidr-notation:parse-ip address))))))))
