;;;; Classes and methods specific to neo4j

(in-package #:syscat)

;;; IPAM


;;; For IPv4 and IPv6 addresses, I'm using the sb-bsd-sockets conversions
;;; to vectors of unsigned bytes, largely because it's already written by
;;; somebody smarter than me, and it's close enough to a format useful for
;;; comparisons.
;;; Portability would be nice, but a)I'm writing this for myself, and SBCL is my
;;; platform of choice, and b)the nature of graph databases is such that
;;; migrating to another format is feasible if required later.

(defmethod store-ipv4-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  ;; First, validate the address
  ;; Then convert it to a 32-bit integer and store it
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . "CREATE (i:ipv4Addr {properties}) RETURN i")
         (:PARAMETERS .
          ((:properties .
            ((:address . ,(format nil "~A" (sb-bsd-sockets:make-inet-address address))))))))))))

(defmethod get-ipv4-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  ;; Extract just the data, ignoring everything else
  (let* ((response-data
           (neo4cl:extract-data-from-get-request
             (neo4cl:neo4j-transaction
               endpoint
               `((:STATEMENTS
                   ((:STATEMENT . ,(format nil "MATCH (i:ipv4Addr {address: '~A'}) RETURN i"
                                           (sb-bsd-sockets:make-inet-address address)))))))))
         ;; Extract the address from the response, but only if
         ;; there's something to inspect
         (address (when response-data
                    (read (cdr (assoc :address response-data))))))
    ;; If an address was returned, convert it back to dotted-quad notation
    (when address
      (vector-to-dotted-quad (parse-integer address)))))

(defmethod delete-ipv4-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . ,(format nil "MATCH (i:ipv4Addr { address: '~A' }) DELETE i"
                                (sb-bsd-sockets:make-inet-address address))))))))
