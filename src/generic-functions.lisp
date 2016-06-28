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
