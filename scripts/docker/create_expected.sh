sbcl --userinit sbclrc-docker \
    --eval "(asdf:load-system :syscat)" \
    --eval "(sb-ext:save-lisp-and-die \"syscat_expected\" :executable t :toplevel #'(lambda () (syscat::dockerstart :schemapath \"/schemas\")))"