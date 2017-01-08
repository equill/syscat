(in-package #:syscat)

(defmethod find-ip-address
  ((db neo4cl:neo4j-rest-server)
   (raw-address string)
   &key (asn 0))
  (let ((address (if (cl-ppcre:all-matches "[0-9]\.[0-9]\.[0-9]\.[0-9]"
                                           raw-address)
                   (sb-bsd-sockets:make-inet-address raw-address)
                   (sb-bsd-sockets:make-inet6-address raw-address))))
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH p=(a:asn {uid: '~A'})-[]->(i:~A {uid: '~A'}) RETURN p"
                     asn
                     (if (equal (length address) 4)
                       "ipv4Address"
                       "ipv6Address")
                     address))))))))
