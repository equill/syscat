(asdf:defsystem #:syscat
  :serial t
  :license "MIT license"
  :author "James Fleming <james@electronic-quill.net>"
  :description "System Catalogue"
  :depends-on (#:restagraph)
  :components ((:file "package")
               (:file "config")
               (:file "syscat")))
