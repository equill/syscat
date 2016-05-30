;;;; Generic functions for the syscat library

(in-package #:syscat)

(defgeneric discover-rest-api (endpoint)
  (:documentation "Query the REST API to discover its capabilities"))

(defgeneric store-ipv4-address (endpoint address))

(defgeneric get-ipv4-address (endpoint address))

(defgeneric delete-ipv4-address (endpoint address))
