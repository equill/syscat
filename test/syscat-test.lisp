(in-package #:syscat-test)

(defparameter *server*
  (make-instance 'neo4cl:neo4j-rest-server
                 :dbpasswd "wallaby"))

;;;; The actual test suite
(fiveam:def-suite neo4j)
(fiveam:in-suite neo4j)

(fiveam:test
  ipam
  "Functions relating to IP addresses"
  ;; Store an IPv4 address
  (multiple-value-bind (results status message)
    (syscat:store-ipv4-address *server* "127.0.0.1")
    (fiveam:is (equal 200 status))
    (fiveam:is (equal "OK" message))
    (fiveam:is (equal '((:ADDRESS . "127.0.0.1"))
                      (neo4cl:extract-data-from-get-request results))))
  ;; Retrieve the IPv4 address we just stored
  (fiveam:is (equal '((:ADDRESS . "127.0.0.1"))
                    (syscat:get-ipv4-address *server* "127.0.0.1")))
  ;; Delete it
  (multiple-value-bind (results status message)
    (syscat:delete-ipv4-address *server* "127.0.0.1")
    (fiveam:is (equal 200 status))
    (fiveam:is (equal "OK" message))
    (fiveam:is (equal '((:RESULTS ((:COLUMNS) (:DATA))) (:ERRORS))
                      results)))
  ;; But is it really gone?
  (fiveam:is (null (syscat:get-ipv4-address *server* "127.0.0.1")))

  ;; Store an IPv6 address
  (multiple-value-bind (results status message)
    (syscat:store-ipv6-address *server* "::1")
    (fiveam:is (equal 200 status))
    (fiveam:is (equal "OK" message))
    (fiveam:is (equal '((:ADDRESS . "0:0:0:0:0:0:0:1"))
                      (neo4cl:extract-data-from-get-request results))))
  ;; Retrieve the IPv6 address we just stored
  (fiveam:is (equal '((:ADDRESS . "0:0:0:0:0:0:0:1"))
                    (syscat:get-ipv6-address *server* "::1")))
  ;; Delete it
  (multiple-value-bind (results status message)
    (syscat:delete-ipv6-address *server* "::1")
    (fiveam:is (equal 200 status))
    (fiveam:is (equal "OK" message))
    (fiveam:is (equal '((:RESULTS ((:COLUMNS) (:DATA))) (:ERRORS))
                      results)))
  ;; But is it really gone?
  (fiveam:is (null (syscat:get-ipv6-address *server* "::1"))))
