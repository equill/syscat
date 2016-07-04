(defpackage #:syscat
  (:use
    #:cl)
  (:export
    neo4j-rest-server
    store-ipv4-address
    get-ipv4-address
    delete-ipv4-address
    store-ipv6-address
    get-ipv6-address
    delete-ipv6-address
    store-device
    get-device
    delete-device
    add-interface-to-device
    list-device-interfaces
    get-device-interface
    delete-interface-from-device
    ))
