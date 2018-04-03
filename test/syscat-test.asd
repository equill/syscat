(asdf:defsystem #:syscat-test
                :serial t
                :license "MIT license"
                :author "James Fleming <james@electronic-quill.net>"
                :description "Test suite for Syscat"
                :depends-on (#:syscat
                             #:fiveam
                             #:ipaddress)
                :components ((:file "package")
                             (:file "syscat-test")))
