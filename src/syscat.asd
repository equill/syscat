(asdf:defsystem #:syscat
  :serial t
  :license "MIT license"
  :author "James Fleming <james@electronic-quill.net>"
  :description "System catalogue"
  :depends-on (#:neo4cl)
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
