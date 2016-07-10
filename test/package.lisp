(defpackage #:syscat-test
  (:use #:cl
        #:syscat)
  (:export neo4j
           ip-addrs
           ipam
           devices-basic
           interfaces-basic
           interfaces-multiple
           delete-device-with-interfaces
           ))
