;;;; Classes and methods specific to neo4j

(in-package #:syscat)


;;; Classes

(defclass neo4j-rest-server ()
  ((base-url :initarg :base-url
             :initform "http://localhost:7474"
             :reader base-url
             :documentation "Omit any trailing slashes; they're added as necessary. Include the protocol and port number as appropriate, however, e.g. http://localhost:7474")
   (dbuser :initarg :dbuser
           :initform "neo4j"
           :reader dbuser
           :documentation "Username with which we authenticate to the database.")
   (dbpasswd :initarg :dbpasswd
             :initform "neo4j"
             :reader dbpasswd
             :documentation "Password with which we authenticate to the database."))
  (:documentation "Neo4j REST API endpoint"))


;;; Utilities

(defun decode-neo4j-json (json)
  "Parse the JSON returned by Neo4J into a CL structure"
  ;; Neo4j sends a stream of octets. Convert this into a string.
  (let ((json-string (babel:octets-to-string json)))
    ;; If an empty string was returned, pass an empty string back.
    (if (equal json-string "")
        ""
        ;; If we received actual content, on the other hand, decode it.
        (cl-json:decode-json-from-string json-string))))

(define-condition neo4j-transaction-error (error)
  ((code :initarg :code :reader code)
   (message :initarg :message :reader message)))

(define-condition neo4j-invalid-format-error (neo4j-transaction-error)
  ((code :initarg :code :reader code)
   (message :initarg :message :reader message)))

(define-condition neo4j-integrity-error (neo4j-transaction-error)
  ((code :initarg :code :reader code)
   (message :initarg :message :reader message)))

(defun neo4j-transaction (endpoint statements)
  "Execute one or more Cypher statements, wrapped in a transaction.
  For now, we're simply issuing all statements in a batch and then committing, instead of building it up over several HTTP requests.
  Sample input:
  '((:statements
      ((:statement . \"MATCH (n:User {properties} ) RETURN n\")
       (:parameters (:properties (:name . \"bob\"))))))"
  (multiple-value-bind (reply-content code headers uri stream ignore reason)
    (drakma:http-request (concatenate 'string (base-url endpoint) "/db/data/transaction/commit")
                         :method :post
                         :accept "application/json; charset=UTF-8"
                         :content-type "application/json"
                         :additional-headers `(("authorization"
                                                ,(concatenate 'string "Basic "
                                                              (cl-base64:string-to-base64-string
                                                                (format nil "~A:~A"
                                                                        (dbuser endpoint)
                                                                        (dbpasswd endpoint))))))
                         :content (cl-json:encode-json-alist-to-string
                                    statements))
    ;; We only bound these values to make m-v-b work properly.
    (declare (ignore headers)
             (ignore uri)
             (ignore stream)
             (ignore ignore))
    ;; Prepare to bind some common restart cases
    (restart-case
      ;; Process the response we got, returning either the content or an error
      (let* ((response (decode-neo4j-json reply-content))
             (errors (second (second response))))
        (if
          ;; If an error was returned, throw it
          errors
          (let ((error-code (cdr (assoc :code errors))))
            (error
              (cond
                ;; Schema violation.
                ;; Usually because we attempted to create a user that already exists
                ((equal error-code "Neo.ClientError.Schema.ConstraintViolation")
                 'neo4j-integrity-error)
                ;; Usually means we goofed when generating the JSON for the query
                ((equal error-code "Neo.ClientError.Request.InvalidFormat")
                 'neo4j-invalid-format-error)
                ;; Anything else.
                (t
                  'neo4j-transaction-error))
              :code (cdr (assoc :code errors))
              :message (cdr (assoc :message errors))))
          ;; If everything's OK, return the values we received along with the status codes
          (values response code reason)))
      ;;; Restart cases start here
      ;; The ultimate cop-out: FIDO
      (return-nil () nil)
      ;; Just tell us what went wrong
      (report-error (e) (format nil "Error code: '~A' Error message: '~A'" (code e) (message e))))))

;;; Methods - implementing the generics

;;; IPAM

(defmethod store-ipv4-address ((endpoint neo4j-rest-server) (address string))
  ;; First, validate the address
  ;; Then convert it to a 32-bit integer and store it
  (neo4j-transaction
    endpoint
    `((:STATEMENTS
        ((:STATEMENT . "CREATE (i:ipv4Addr {properties}) RETURN i")
         (:PARAMETERS .
                      ((:properties .
                                    ((:address . ,(format nil "~A" (cl-cidr-notation:parse-ip address))))))))))))

(defun extract-data-from-get-request (response)
  "Reach into the copious data returned by a get-x request,
   and return only the actual content."
  ;; The row data, ignoring the metadata
  (car
    (cdr
      (assoc :row
             (car
               ;; Only the data section, ignoring the columns list
               (cdr
                 (assoc :data
                        (car
                          ;; Just the results section
                          (cdr (assoc :results
                                      ;; The actual query
                                      response))))))))))

(defmethod get-ipv4-address ((endpoint neo4j-rest-server) (address string))
  ;; Assert which path we want to take in the event of an error
  (handler-bind
    ;; For now, I just want to see what went wrong
    ((neo4j-transaction-error #'(lambda (e) (invoke-restart 'report-error e))))
    ;; The thing we actually wanted to do
    ;; Extract just the data, ignoring everything else
    (let* ((response-data
             (extract-data-from-get-request
               (neo4j-transaction
                 endpoint
                 `((:STATEMENTS
                     ((:STATEMENT . ,(format nil "MATCH (i:ipv4Addr {address: '~A'}) RETURN i"
                                             (cl-cidr-notation:parse-ip address)))))))))
           (address (cdr (assoc :address response-data))))
      ;; Convert it back to dotted-quad notation before returning it.
      (cl-cidr-notation:ip-string (parse-integer address)))))
