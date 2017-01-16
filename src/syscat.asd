(asdf:defsystem #:syscat
  :serial t
  :license "MIT license"
  :author "James Fleming <james@electronic-quill.net>"
  :description "System Catalogue"
  :depends-on (#:restagraph
               #:cl-cidr-notation)
  :components ((:file "package")
               (:file "config")
               (:file "utilities")
               (:file "hunchentoot")))
