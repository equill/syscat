(asdf:defsystem #:syscat
  :serial t
  :license "MIT license"
  :author "James Fleming <james@electronic-quill.net>"
  :description "System Catalogue"
  :depends-on (#:restagraph
               #:cl-cidr-notation
               #:ipaddress)
  :components ((:file "package")
               (:file "config")
               (:file "generics")
               (:file "neo4j")
               (:file "hunchentoot")))
