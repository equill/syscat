sbcl --userinit sbclrc-docker \
    --eval "(asdf:load-system :restagraph)" \
    --eval "(sb-ext:save-lisp-and-die \"syscat_discovered\" :executable t :toplevel #'(lambda () (restagraph::dockerstart :schemapath \"/schemas\")))"
