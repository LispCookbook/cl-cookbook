;; Enter "M-x shell" to bring up a shell window in the current
;; directory. Then run "etags *.lisp" to create a TAG file containing
;; all symbols in lisp files in the current directory. Then run "etags
;; -a subdir/*.lisp" to add to the TAG file symbols from all lisp
;; files in another directory. Locate the definition of the "aa"
;; variable is the s13.lisp file by putting the cursor on the "aa" in
;; the following form and pressing "M-.".
(setq x aa)