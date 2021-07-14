;   Copyright 2021 James Fleming <james@electronic-quill.net>

;;;; Configs for the server to use

(in-package #:restagraph)

;; Forcibly override the existing variables
(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 4950
    :dbhostname "10.255.0.1"
    :dbport 7474
    :dbname "neo4j"
    :dbusername "neo4j"
    :dbpasswd "wallaby"
    :api-uri-base "/raw/v1"
    :schema-uri-base "/schema/v1"
    :files-uri-base "/files/v1"
    :files-temp-location "/tmp/restagraph-files-tmp/"
    :files-location "/tmp/restagraph-files"))

(setf *loglevel* :info)
