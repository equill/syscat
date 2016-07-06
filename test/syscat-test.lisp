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
  (fiveam:is (syscat:store-ipv4-address *server* "127.0.0.1"))
  ;; Retrieve the IPv4 address we just stored
  (fiveam:is (equal '((:ADDRESS . "127.0.0.1"))
                    (syscat:get-ipv4-address *server* "127.0.0.1")))
  ;; Delete it
  (fiveam:is (syscat:delete-ipv4-address *server* "127.0.0.1"))
  ;; But is it really gone?
  (fiveam:is (null (syscat:get-ipv4-address *server* "127.0.0.1")))
  ;; Store an IPv6 address
  (fiveam:is (syscat:store-ipv6-address *server* "::1"))
  ;; Retrieve the IPv6 address we just stored
  (fiveam:is (equal '((:ADDRESS . "0:0:0:0:0:0:0:1"))
                    (syscat:get-ipv6-address *server* "::1")))
  ;; Delete it
  (fiveam:is (syscat:delete-ipv6-address *server* "::1"))
  ;; But is it really gone?
  (fiveam:is (null (syscat:get-ipv6-address *server* "::1"))))

(fiveam:test
  devices-basic
  "Functions relating to devices and physical things attached to them"
  ;; Store a device
  (let ((test-hostname "gandalf.onfire.onice"))
    (fiveam:is (syscat:store-device *server* test-hostname))
    ;; Retrieve the device we just stored
    (fiveam:is (equal `((:HOSTNAME . ,test-hostname))
                      (syscat:get-device *server* test-hostname)))
    ;; Delete it
    (fiveam:is (syscat:delete-device *server* test-hostname))
    ;; Confirm that it's gone
    (fiveam:is (null (syscat:get-device *server* test-hostname)))))

(fiveam:test
  (interfaces-basic :depends-on devices-basic)
  ;; Now test interface-related functions
  (let ((test-hostname "bilbo.onfire.onice")
        (test-iface "eth0"))
    ;; Create a new device for testing interfaces
    (fiveam:is (syscat:store-device *server* test-hostname))
    ;; Add an interface to the device
    (fiveam:is (syscat:add-interface-to-device *server* test-hostname test-iface))
    ;; List the device's interfaces
    (fiveam:is (equal `((,test-iface))
                      (syscat:list-device-interfaces *server* test-hostname)))
    ;; Get the interface's details
    (fiveam:is (equal `((:name . ,test-iface))
                      (syscat:get-device-interface *server* test-hostname test-iface)))
    ;; Delete the interface
    (fiveam:is (syscat:delete-interface-from-device *server* test-hostname test-iface))
    ;; Delete the test device
    (fiveam:is (syscat:delete-device *server* test-hostname))))

(fiveam:test
  (interfaces-multiple :depends-on devices-basic)
  (let ((test-hostname "bilbo.onfire.onice")
        (test-iface "eth0")
        (test-iface2 "eth1"))
    ;; Create a new device for testing interfaces
    (fiveam:is (syscat:store-device *server* test-hostname))
    ;; Add an interface to the device
    (fiveam:is (syscat:add-interface-to-device *server* test-hostname test-iface))
    ;; Add a second interface
    (fiveam:is (syscat:add-interface-to-device *server* test-hostname test-iface2))
    ;; List the interfaces, and confirm we're told about both of them
    (fiveam:is (equal `((,test-iface2) (,test-iface))
                      (syscat:list-device-interfaces *server* test-hostname)))
    ;; Get the second interface's details
    (fiveam:is (equal `((:name . ,test-iface2))
                      (syscat:get-device-interface *server* test-hostname test-iface2)))
    ;; Delete the first interface
    (fiveam:is (syscat:delete-interface-from-device *server* test-hostname test-iface))
    ;; Confirm only the second one is still there
    (fiveam:is (equal `((,test-iface2))
                      (syscat:list-device-interfaces *server* test-hostname)))
    ;; Delete the second interface
    (fiveam:is (syscat:delete-interface-from-device *server* test-hostname test-iface2))
    ;; Confirm there are now no interfaces
    (fiveam:is (equal `()
                      (syscat:list-device-interfaces *server* test-hostname)))
    ;; Delete the test device
    (fiveam:is (syscat:delete-device *server* test-hostname))))

(fiveam:test
  (delete-device-with-interfaces :depends-on interfaces-multiple)
  (let ((test-hostname "frodo.onfire.onice")
        (test-iface "eth0")
        (test-iface2 "eth1"))
    ;; Create a new device for testing interfaces
    (fiveam:is (syscat:store-device *server* test-hostname))
    ;; Add an interface to the device
    (fiveam:is (syscat:add-interface-to-device *server* test-hostname test-iface))
    ;; Delete the device; its interface should have gone with it.
    (fiveam:is (syscat:delete-device *server* test-hostname))
    ;; Make sure it's really gone
    (fiveam:is (null (syscat:get-device *server* test-hostname)))
    ;; Do it all again, but with 2 interfaces this time
    (fiveam:is (syscat:store-device *server* test-hostname))
    (fiveam:is (syscat:add-interface-to-device *server* test-hostname test-iface))
    (fiveam:is (syscat:add-interface-to-device *server* test-hostname test-iface2))
    (fiveam:is (syscat:delete-device *server* test-hostname))
    (fiveam:is (null (syscat:get-device *server* test-hostname)))))
