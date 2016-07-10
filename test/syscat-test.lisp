;;;; Test suite for all of syscat
;;;;
;;;; Beware: it currently only tests _expected_ cases,
;;;; and does not test edge-cases or wrong input.

(in-package #:syscat-test)

(defparameter *server*
  (syscat::datastore syscat::*syscat-acceptor*))

(fiveam:def-suite neo4j)
(fiveam:in-suite neo4j)

(fiveam:test
  ip-addrs
  "Cursory test of IP address formatting functions"
  (fiveam:is (equal "127.0.0.1"
                    (syscat::canonicalise-ipv4-addr "127.00.00.1")))
  (fiveam:is (equal "0:0:0:0:0:0:0:1"
                    (syscat::canonicalise-ipv6-addr "::1"))))

(fiveam:test
  ipam
  "Functions relating to IP addresses"
  ;; Store an IPv4 address
  (fiveam:is (equal
               (syscat:store-ipv4-address *server* "127.0.0.1")
               "{\"address\":\"127.0.0.1\"}"))
  ;; Retrieve the IPv4 address we just stored
  (fiveam:is (equal "{\"address\":\"127.0.0.1\"}"
                    (syscat:get-ipv4-address *server* "127.0.0.1")))
  ;; Delete it
  (fiveam:is (equal "{}" (syscat:delete-ipv4-address *server* "127.0.0.1")))
  ;; But is it really gone?
  (fiveam:is (equal "{}" (syscat:get-ipv4-address *server* "127.0.0.1")))
  ;; Store an IPv6 address
  (fiveam:is (equal "{\"address\":\"0:0:0:0:0:0:0:1\"}"
                    (syscat:store-ipv6-address *server* "::1")))
  ;; Retrieve the IPv6 address we just stored
  (fiveam:is (equal "{\"address\":\"0:0:0:0:0:0:0:1\"}"
                    (syscat:get-ipv6-address *server* "::1")))
  ;; Delete it
  (fiveam:is (equal "{}" (syscat:delete-ipv6-address *server* "::1")))
  ;; But is it really gone?
  (fiveam:is (equal "{}" (syscat:get-ipv6-address *server* "::1"))))

(fiveam:test
  devices-basic
  "Functions relating to devices and physical things attached to them"
  ;; Store a device
  (let ((test-hostname "gandalf.onfire.onice"))
    (fiveam:is (equal (format nil "{\"hostname\":\"~A\"}" test-hostname)
                      (syscat:store-device *server* test-hostname)))
    ;; Retrieve the device we just stored
    (fiveam:is (equal (format nil "{\"hostname\":\"~A\"}" test-hostname)
                      (syscat:get-device *server* test-hostname)))
    ;; Delete it
    (fiveam:is (equal "{}" (syscat:delete-device *server* test-hostname)))
    ;; Confirm that it's gone
    (fiveam:is (equal "{}" (syscat:get-device *server* test-hostname)))))

(fiveam:test
  (interfaces-basic :depends-on devices-basic)
  ;; Now test interface-related functions
  (let ((test-hostname "bilbo.onfire.onice")
        (test-iface "eth0"))
    ;; Create a new device for testing interfaces
    (fiveam:is (equal (format nil "{\"hostname\":\"~A\"}" test-hostname)
                      (syscat:store-device *server* test-hostname)))
    ;; Confirm that it has no interfaces
    (fiveam:is (equal "{}"
                      (syscat:list-device-interfaces *server*
                                                     test-hostname)))
    ;; Add an interface to the device
    (fiveam:is (equal (format nil "{\"~A\":null}" test-iface)
                      (syscat:add-interface-to-device *server*
                                                      test-hostname
                                                      test-iface)))
    ;; Confirm the interface is listed for that device
    (fiveam:is (equal (format nil "{\"~A\":null}" test-iface)
                      (syscat:list-device-interfaces *server* test-hostname)))
    ;; Get the interface's details
    (fiveam:is (equal (format nil "{\"name\":\"~A\"}" test-iface)
                      (syscat:get-device-interface *server*
                                                   test-hostname test-iface)))
    ;; Delete the interface
    (fiveam:is (equal "{}"
                      (syscat:delete-interface-from-device *server*
                                                           test-hostname
                                                           test-iface)))
    ;; Confirm the interface has gone
    (fiveam:is (equal "{}"
                      (syscat:list-device-interfaces *server*
                                                     test-hostname)))
    ;; Delete the test device
    (fiveam:is (equal "{}"
                      (syscat:delete-device *server* test-hostname)))))

(fiveam:test
  (interfaces-multiple :depends-on devices-basic)
  (let ((test-hostname "bilbo.onfire.onice")
        (test-iface "eth0")
        (test-iface2 "eth1"))
    ;; Create a new device for testing interfaces
    (fiveam:is (equal (format nil "{\"hostname\":\"~A\"}" test-hostname)
                      (syscat:store-device *server* test-hostname)))
    ;; Add an interface to the device
    (fiveam:is (equal (format nil "{\"~A\":null}" test-iface)
                      (syscat:add-interface-to-device *server*
                                                      test-hostname
                                                      test-iface)))
    ;; Add a second interface
    (fiveam:is (equal (format nil "{\"~A\":null,\"~A\":null}"
                              test-iface test-iface2)
                      (syscat:add-interface-to-device *server* test-hostname test-iface2)))
    ;; List the interfaces, and confirm we're told about both of them
    (fiveam:is (equal (format nil "{\"~A\":null,\"~A\":null}"
                              test-iface test-iface2)
                      (syscat:list-device-interfaces *server* test-hostname)))
    ;; Get the second interface's details
    (fiveam:is (equal (format nil "{\"name\":\"~A\"}" test-iface2)
                      (syscat:get-device-interface *server* test-hostname test-iface2)))
    ;; Delete the first interface
    (fiveam:is (equal (format nil "{\"~A\":null}" test-iface2)
                      (syscat:delete-interface-from-device *server* test-hostname test-iface)))
    ;; Confirm only the second one is still there
    (fiveam:is (equal (format nil "{\"~A\":null}" test-iface2)
                      (syscat:list-device-interfaces *server* test-hostname)))
    ;; Delete the second interface
    (fiveam:is (equal "{}" (syscat:delete-interface-from-device *server* test-hostname test-iface2)))
    ;; Confirm there are now no interfaces
    (fiveam:is (equal "{}"
                      (syscat:list-device-interfaces *server* test-hostname)))
    ;; Delete the test device
    (fiveam:is (equal "{}" (syscat:delete-device *server* test-hostname)))))

(fiveam:test
  (delete-device-with-interfaces :depends-on interfaces-multiple)
  (let ((test-hostname "frodo.onfire.onice")
        (test-iface "eth0")
        (test-iface2 "eth1"))
    ;; Create a new device for testing interfaces
    (fiveam:is (equal (format nil "{\"hostname\":\"~A\"}" test-hostname)
                      (syscat:store-device *server* test-hostname)))
    ;; Confirm that it has no interfaces
    (fiveam:is (equal "{}"
                      (syscat:list-device-interfaces *server*
                                                     test-hostname)))
    ;; Add an interface to the device
    (fiveam:is (equal (format nil "{\"~A\":null}" test-iface)
                      (syscat:add-interface-to-device *server*
                                                      test-hostname
                                                      test-iface)))
    ;; Delete the device; its interface should have gone with it.
    (fiveam:is (equal "{}" (syscat:delete-device *server* test-hostname)))
    ;; Make sure it's really gone
    (fiveam:is (equal "{}" (syscat:get-device *server* test-hostname)))
    ;; Do it all again, but with 2 interfaces this time
    (fiveam:is (equal (format nil "{\"hostname\":\"~A\"}" test-hostname)
                      (syscat:store-device *server* test-hostname)))
    (fiveam:is (equal (format nil "{\"~A\":null}" test-iface)
                      (syscat:add-interface-to-device *server*
                                                      test-hostname
                                                      test-iface)))
    (fiveam:is (equal (format nil "{\"~A\":null,\"~A\":null}"
                              test-iface test-iface2)
                      (syscat:add-interface-to-device *server*
                                                      test-hostname
                                                      test-iface2)))
    (fiveam:is (equal "{}" (syscat:delete-device *server* test-hostname)))
    (fiveam:is (equal "{}" (syscat:get-device *server* test-hostname)))))
