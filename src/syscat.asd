(asdf:defsystem #:syscat
  :serial t
  :license "MIT license"
  :author "James Fleming <james@electronic-quill.net>"
  :description "System catalogue"
  :depends-on (#:cl-ppcre      ; Check UUID formatting and identify error strings
               #:drakma        ; Send requests to the neo4j server
               #:cl-json       ; Encode/decode json requests
               #:babel         ; Convert neo4j's octet response into a string
               #:ironclad      ; Password hashing
               #:flexi-streams ; Convert between strings and octets
               #:uuid          ; Universally Unique IDentifiers
               #:cl-base64     ; base64 encoding/decoding
               #:neo4cl        ; Actually connect to Neo4J
               )
  :components ((:file "generic-functions")
               (:file "ip-addrs")
               (:file "neo4j")))

(defpackage #:syscat
  (:use
    #:cl)
  (:export
    neo4j-rest-server
    store-ipv4-address
    get-ipv4-address
    delete-ipv4-address
    ))
