;;;; Classes and methods specific to neo4j

(in-package #:syscat)

;;; IPAM

;; IPv4 addresses are stored in canonicalised format, to ensure predictability
;; once they're in the system.

(defmethod store-ipv4-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . "CREATE (i:ipv4Addr {properties}) RETURN i")
         (:PARAMETERS .
          ((:properties .
            ((:address . ,(format nil "~A"
                                  (canonicalise-ipv4-addr address))))))))))))

(defmethod get-ipv4-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  (neo4cl:extract-data-from-get-request
    (neo4cl:neo4j-transaction
      endpoint
      `((:STATEMENTS
          ((:STATEMENT . ,(format nil "MATCH (i:ipv4Addr {address: '~A'}) RETURN i"
                                  (canonicalise-ipv4-addr address)))))))))

(defmethod delete-ipv4-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . ,(format nil "MATCH (i:ipv4Addr { address: '~A' }) DELETE i"
                                (canonicalise-ipv4-addr address))))))))

(defmethod store-ipv6-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . "CREATE (i:ipv6Addr {properties}) RETURN i")
         (:PARAMETERS .
          ((:properties .
            ((:address . ,(format nil "~A"
                                  (canonicalise-ipv6-addr address))))))))))))

(defmethod get-ipv6-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  (neo4cl:extract-data-from-get-request
    (neo4cl:neo4j-transaction
      endpoint
      `((:STATEMENTS
          ((:STATEMENT . ,(format nil "MATCH (i:ipv6Addr {address: '~A'}) RETURN i"
                                  (canonicalise-ipv6-addr address)))))))))

(defmethod delete-ipv6-address ((endpoint neo4cl:neo4j-rest-server) (address string))
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . ,(format nil "MATCH (i:ipv6Addr { address: '~A' }) DELETE i"
                                (canonicalise-ipv6-addr address))))))))

(defmethod store-device ((endpoint neo4cl:neo4j-rest-server) (hostname string))
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . "CREATE (d:device {properties}) RETURN d")
         (:PARAMETERS .
          ((:properties .
            ((:hostname . ,hostname))))))))))

(defmethod get-device ((endpoint neo4cl:neo4j-rest-server) (hostname string))
  (neo4cl:extract-data-from-get-request
    (neo4cl:neo4j-transaction
      endpoint
      `((:STATEMENTS
          ((:STATEMENT . ,(format nil "MATCH (d:device { hostname: '~A' }) RETURN d" hostname))))))))

(defmethod delete-device ((endpoint neo4cl:neo4j-rest-server) (hostname string))
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . ,(format nil "MATCH (d:device { hostname: '~A' }) DELETE d" hostname)))))))

(defmethod add-interface-to-device ((endpoint neo4cl:neo4j-rest-server) (hostname string) (interface string))
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT .
          ,(format nil
                   "MATCH (d:device { hostname: '~A' }) CREATE (d)-[:hasInterface]->(i:interface {properties})"
                   hostname))
         (:PARAMETERS .
          ((:properties .
            ((:name . ,interface))))))))))

(defmethod get-device-interface ((endpoint neo4cl:neo4j-rest-server) (hostname string) (interface string))
  (neo4cl:extract-data-from-get-request
    (neo4cl:neo4j-transaction
      endpoint
      `((:STATEMENTS
          ((:STATEMENT . ,(format
                            nil
                            "MATCH (:device { hostname: '~A' })-[:hasInterface]->(i:interface { name: '~A'}) RETURN i"
                            hostname interface))))))))

(defmethod list-device-interfaces ((endpoint neo4cl:neo4j-rest-server) (hostname string))
  (neo4cl:extract-rows-from-get-request
    (neo4cl:neo4j-transaction
      endpoint
      `((:STATEMENTS
          ((:STATEMENT . ,(format
                            nil
                            "MATCH (:device { hostname: '~A' })-[:hasInterface]->(i:interface) RETURN i.name"
                            hostname ))))))))

(defmethod delete-interface-from-device ((endpoint neo4cl:neo4j-rest-server) (hostname string) (interface string))
  (neo4cl:neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT .
          ,(format
             nil
             "MATCH (:device { hostname: '~A' })-[r:hasInterface]->(i:interface { name: '~A' }) DELETE r, i"
             hostname interface)))))))
