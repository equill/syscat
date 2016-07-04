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

(fiveam:test
  devices-basic
  "Functions relating to devices and physical things attached to them"
  ;; Store a device
  (let ((test-hostname "gandalf.onfire.onice"))
    (multiple-value-bind (results status message)
      (syscat:store-device *server* test-hostname)
      (fiveam:is (equal 200 status))
      (fiveam:is (equal "OK" message))
      (fiveam:is (equal `((:HOSTNAME . ,test-hostname))
                        (neo4cl:extract-data-from-get-request results))))
    ;; Retrieve the device we just stored
    (fiveam:is (equal `((:HOSTNAME . ,test-hostname))
                      (syscat:get-device *server* test-hostname)))
    ;; Delete it
    (multiple-value-bind (results status message)
      (syscat:delete-device *server* test-hostname)
      (fiveam:is (equal 200 status))
      (fiveam:is (equal "OK" message))
      (fiveam:is (equal '((:RESULTS ((:COLUMNS) (:DATA))) (:ERRORS))
                        results)))
    ;; Confirm that it's gone
    (fiveam:is (null (syscat:get-device *server* test-hostname)))))

(fiveam:test
  (interfaces-basic :depends-on devices-basic)
  ;; Now test interface-related functions
  (let ((test-hostname "bilbo.onfire.onice")
        (test-iface "eth0"))
    ;; Create a new device for testing interfaces
    (multiple-value-bind (results status message)
      (syscat:store-device *server* test-hostname)
      (declare (ignore results message))
      (fiveam:is (equal 200 status)))
    ;; Add an interface to the device
    (multiple-value-bind (results status message)
      (syscat:add-interface-to-device *server* test-hostname test-iface)
      (fiveam:is (equal 200 status))
      (fiveam:is (equal "OK" message))
      (fiveam:is (equal '((:RESULTS ((:COLUMNS) (:DATA))) (:ERRORS))
                        results)))
    ;; List the device's interfaces
    (fiveam:is (equal `((,test-iface))
                      (syscat:list-device-interfaces *server* test-hostname)))
    ;; Get the interface's details
    (fiveam:is (equal `((:name . ,test-iface))
                      (syscat:get-device-interface *server* test-hostname test-iface)))
    ;; Delete the interface
    (multiple-value-bind (results status message)
      (syscat:delete-interface-from-device *server* test-hostname test-iface)
      (fiveam:is (equal 200 status))
      (fiveam:is (equal "OK" message))
      (fiveam:is (equal '((:RESULTS ((:COLUMNS) (:DATA))) (:ERRORS))
                        results)))
    ;; Delete the test device
    (multiple-value-bind (results status message)
      (syscat:delete-device *server* test-hostname)
      (declare (ignore results ))
      (fiveam:is (equal 200 status))
      (fiveam:is (equal "OK" message)))))

(fiveam:test
  (interfaces-multiple :depends-on devices-basic)
  (let ((test-hostname "bilbo.onfire.onice")
        (test-iface "eth0")
        (test-iface2 "eth1"))
    ;; Create a new device for testing interfaces
    (multiple-value-bind (results status message)
      (syscat:store-device *server* test-hostname)
      (declare (ignore results message))
      (fiveam:is (equal 200 status)))
    ;; Add an interface to the device
    (multiple-value-bind (results status message)
      (syscat:add-interface-to-device *server* test-hostname test-iface)
      (declare (ignore results message))
      (fiveam:is (equal 200 status)))
    ;; Add a second interface
    (multiple-value-bind (results status message)
      (syscat:add-interface-to-device *server* test-hostname test-iface2)
      (fiveam:is (equal 200 status))
      (fiveam:is (equal "OK" message))
      (fiveam:is (equal `()
                        (neo4cl:extract-data-from-get-request results))))
    ;; List the interfaces, and confirm we're told about both of them
    (fiveam:is (equal `((,test-iface2) (,test-iface))
                      (syscat:list-device-interfaces *server* test-hostname)))
    ;; Get the second interface's details
    (fiveam:is (equal `((:name . ,test-iface2))
                      (syscat:get-device-interface *server* test-hostname test-iface2)))
    ;; Delete the first interface
    (multiple-value-bind (results status message)
      (syscat:delete-interface-from-device *server* test-hostname test-iface)
      (fiveam:is (equal 200 status))
      (fiveam:is (equal "OK" message))
      (fiveam:is (equal '((:RESULTS ((:COLUMNS) (:DATA))) (:ERRORS))
                        results)))
    ;; Confirm only the second one is still there
    (fiveam:is (equal `((,test-iface2))
                      (syscat:list-device-interfaces *server* test-hostname)))
    ;; Delete the second interface
    (multiple-value-bind (results status message)
      (syscat:delete-interface-from-device *server* test-hostname test-iface2)
      (fiveam:is (equal 200 status))
      (fiveam:is (equal "OK" message))
      (fiveam:is (equal '((:RESULTS ((:COLUMNS) (:DATA))) (:ERRORS))
                        results)))
    ;; Confirm there are now no interfaces
    (fiveam:is (equal `()
                      (syscat:list-device-interfaces *server* test-hostname)))
    ;; Delete the test device
    (multiple-value-bind (results status message)
      (syscat:delete-device *server* test-hostname)
      (declare (ignore results ))
      (fiveam:is (equal 200 status))
      (fiveam:is (equal "OK" message)))))
