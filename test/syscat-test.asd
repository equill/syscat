(asdf:defsystem #:syscat-test
  :serial t
  :description "Test suite for syscat"
  :author "James Fleming <james@electronic-quill.net>"
  :license "As yet unlicensed"
  :depends-on (#:syscat
               #:fiveam)
  :components ((:file "syscat-test")))

(defpackage #:syscat-test
  (:use #:cl
        #:syscat)
  (:export neo4j))
