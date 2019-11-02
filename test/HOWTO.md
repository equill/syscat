How to run the test suite
=========================

# Foreword

Yes, really: a foreword to a HOWTO. How very!

This test only covers the IPAM functionality of Syscat, and thus only involves the Expected instance (and thus, in turn, the `common` and `expected` schemas).

This is because the bulk of Syscat is Restagraph with a domain-specific schema, and only the Expected instance actually layers code on top of it. Beyond that, it´s up to the Restagraph test suite to cover everything.


# Preparation

* Start up a default Neo4j container. Assuming you´re already in this directory: `./run_test_neo4j`
    * this will start a container called `syscattest`, listening on TCP/7474 and TCP/7687
* ensure this repo is in SBCL´s search path
* copy the combined `common` and `expected` schemas to a convenient directory:
```
cd ../src/schemas
mkdir /tmp/schemas
cp schemas/common/* /tmp/schemas/
cp schemas/expected/* /tmp/schemas/
```


# Run the actual test
1. Start an interactive SBCL session
1. If you haven´t already, load the required libraries. Zach Beane´s excellent Quicklisp is recommended: `(ql:quickload :syscat-test)`
1. Load the module itself: `(asdf:load-system :syscat-test)`
1. Instantiate the schema: `(syscat:startup :schemapath "/tmp/schemas/")`
1. Run the test: `(fiveam:run! 'syscat-test:main)`
1. Shut down the appserver: `(syscat:shutdown)`
