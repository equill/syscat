(in-package #:syscat-test)

(defparameter *server*
  (make-instance 'syscat::neo4j-rest-server
                 :base-url "http://localhost:7474"
                 :dbuser "neo4j"
                 :dbpasswd "wallaby"))


;;;; The actual test suite
(fiveam:def-suite neo4j)
(fiveam:in-suite neo4j)

(fiveam:test
    neo4j-base-api
  "Test the lowest-level methods for interacting with Neo4j"
  ;; Can we authenticate?
  (multiple-value-bind (body numeric verbal headers)
      (syscat::get-user-status *server*)
    (declare (ignore body)
             (ignore headers))
    (fiveam:is (equal numeric 200))
    (fiveam:is (equal verbal "OK")))
  ;; method: discover-rest-api
  (let ((result (syscat:discover-rest-api *server*)))
    (fiveam:is (listp result))
    (fiveam:is (equalp (car result) '(:extensions))))
  ;; Store a node
  (fiveam:is (listp (syscat::neo4j-cypher-post-request
                     *server*
                     '((:query . "CREATE (n:Person { name : {name} }) RETURN n")
                       (:params (:name . "Andre"))))))
  ;; Retrieve a node
  (fiveam:is (equal
              (car
               (second (assoc :data
                              (syscat::neo4j-cypher-post-request
                               *server*
                               '((:query . "MATCH (x:Person {name: 'Andre'}) RETURN x.name")
                                 (:params (:name . "Andre")))))))
              "Andre"))
  ;; Delete a node
  (let ((result (syscat::neo4j-cypher-post-request
                 *server*
                 '((:query . "MATCH (x:Person {name: 'Andre'}) DELETE x")
                   (:params (:name . "Andre"))))))
    (fiveam:is (listp result))
    (fiveam:is (equal (car (first result)) :columns))
    (fiveam:is (equal (car (second result)) :data))))

(fiveam:test
    uuid
  "Ensure the UUIDs work the way we expect"
  (fiveam:is (syscat:uuid-string-p (syscat:make-uuid))))
