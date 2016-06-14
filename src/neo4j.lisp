;;;; Classes and methods specific to neo4j

(in-package #:syscat)

;;; IPAM

;; IPv4 addresses are stored in canonicalised format, to ensure predictability
;; once they're in the system.

(defmethod store-ipv4-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  ;; First, validate the address
  ;; Then convert it to a 32-bit integer and store it
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . "CREATE (i:ipv4Addr {properties}) RETURN i")
         (:PARAMETERS .
          ((:properties .
            ((:address . ,(format nil "~A"
                                  (canonicalise-ipv4-addr address))))))))))))

(defmethod get-ipv4-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  ;; Extract just the data, ignoring everything else
  (let* ((response-data
           (neo4cl:extract-data-from-get-request
             (neo4cl:neo4j-transaction
               endpoint
               `((:STATEMENTS
                   ((:STATEMENT . ,(format nil "MATCH (i:ipv4Addr {address: '~A'}) RETURN i"
                                           (canonicalise-ipv4-addr address)))))))))
         ;; Extract the address from the response, but only if
         ;; there's something to inspect
         (returned-address (when response-data
                    (cdr (assoc :address response-data)))))
    ;; If an address was returned, convert it back to dotted-quad notation
    (when returned-address (canonicalise-ipv4-addr returned-address))))

(defmethod delete-ipv4-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . ,(format nil "MATCH (i:ipv4Addr { address: '~A' }) DELETE i"
                                (canonicalise-ipv4-addr address))))))))
