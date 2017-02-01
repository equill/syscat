;;;; Configs for the server to use

(in-package #:restagraph)

(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 4950
    :dbhostname "localhost"
    :dbport 7474
    :dbusername "neo4j"
    :dbpasswd "wallaby"
    :uri-base "/raw/v1"))
