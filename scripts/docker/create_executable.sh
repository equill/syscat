sbcl --userinit sbclrc-docker \
    --eval "(asdf:load-system :syscat)" \
    --eval "(sb-ext:save-lisp-and-die \"syscat\" :executable t :toplevel #'syscat::dockerstart)"
