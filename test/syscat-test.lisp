(in-package #:syscat-test)

(defparameter *server*
  (make-instance 'neo4cl:neo4j-rest-server
                 :dbpasswd "wallaby"))

;;;; The actual test suite
(fiveam:def-suite neo4j)
(fiveam:in-suite neo4j)


