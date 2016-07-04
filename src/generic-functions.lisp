;;;; Generic functions for the syscat library

(in-package #:syscat)

(defgeneric store-ipv4-address (endpoint address)
  (:documentation "Store an IPv4 address in the database"))

(defgeneric get-ipv4-address (endpoint address)
  (:documentation "Retrieve the details of an IPv4 address"))

(defgeneric delete-ipv4-address (endpoint address)
  (:documentation "Delete an IPv4 address from the database"))

(defgeneric store-ipv6-address (endpoint address)
  (:documentation "Store an IPv6 address in the database"))

(defgeneric get-ipv6-address (endpoint address)
  (:documentation "Retrieve the details of an IPv6 address"))

(defgeneric delete-ipv6-address (endpoint address)
  (:documentation "Delete an IPv6 address from the database"))

(defgeneric store-device (endpoint hostname)
  (:documentation "Store a device in the database."))

(defgeneric get-device (endpoint hostname)
  (:documentation "Return the details of a device"))

(defgeneric delete-device (endpoint hostname)
  (:documentation "Delete a device from the database, including its interfaces"))

(defgeneric add-interface-to-device (endpoint hostname interface)
  (:documentation "Add an interface to a device"))

(defgeneric delete-interface-from-device (endpoint hostname interface)
  (:documentation "Remove an interface from a device"))

(defgeneric list-device-interfaces (endpoint hostname)
  (:documentation "Enumerate the interfaces on a device"))

(defgeneric get-device-interface (endpoint hostname interface)
  (:documentation "Get the details of a specific device"))
