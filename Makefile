.PHONY = run clean epub epub+pdf

SED_CMD ?= sed
export SED_CMD

run:
	bundle exe jekyll serve --incremental

clean:
	rm -f full.md common-lisp-cookbook.epub common-lisp-cookbook.pdf

epub: clean
	sbcl --load make-cookbook.lisp --eval '(generate)' --eval '(to-epub)' --eval '(uiop:quit)'

epub+pdf: epub
	sbcl --load make-cookbook.lisp --eval '(to-pdf)' --eval '(uiop:quit)'
