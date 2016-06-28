(asdf:defsystem #:syscat
  :serial t
  :license "MIT license"
  :author "James Fleming <james@electronic-quill.net>"
  :description "System catalogue"
  :depends-on (#:neo4cl)
  :components ((:file "package")
               (:file "generic-functions")
               (:file "ip-addrs")
               (:file "neo4j")))
