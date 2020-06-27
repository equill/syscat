;;;; Configs for the server to use

(in-package #:restagraph)

;; Forcibly override the existing variables
(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 4950
    :dbhostname "localhost"
    :dbport 7474
    :dbname "neo4j"
    :dbusername "neo4j"
    :dbpasswd "wallaby"
    :api-uri-base "/raw/v1"
    :schema-uri-base "/schema/v1"))

(setf *loglevel* :info)
