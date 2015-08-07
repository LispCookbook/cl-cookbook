;; Indentation is automatic for Lisp forms. Example: Put the cursor
;; after the first addition form and press Enter:
(progn
  (+ 3 3)
  (- 2 2))

;; Pressing TAB will indent incorrectly indented code. Example: Put
;; the cursor at the beginning of the "(+ 3 3)" form and press TAB:
(progn
(+ 3 3))

;; CL indentation rules are different from Emacs Lisp indentation
;; rules. Make certain you have the following code in a lisp mode hook
;; in your .emacs file:
(set (make-local-variable lisp-indent-function)
		 'common-lisp-indent-function)
