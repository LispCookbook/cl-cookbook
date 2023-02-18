run:
        bundle exe jekyll serve --incremental

epub:
	sbcl --load make-cookbook.lisp --eval '(uiop:quit)'
