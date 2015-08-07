;; Put the cursor on the following sexp and press "M-;" to get a
;; code line comment (right-hand comment):
(setq x 1)

;; Highlight the 2nd & 3rd lines and press "M-;" to comment out those
;; lines (highlighting them a 2nd time and pressing "M-;" removes the
;; comment):
(+ 1 1)
(+ 2 2)
(+ 3 3)

;; Using Paul Foley's comment functions allows you to selectively
;; comment out embedded sexps. Example: Put the cursor on the "(+ 4
;; 4)" sexp and press "C-c ;" to comment out that sexp. Pressing "C-c
;; ;" comments out the enclosing sexp (and on upwards). Pressing "C-c
;; :" removes the comment:
(+ 1 (+ 2 (+ 3 (+ 4 4))))

;; Emacs knows how to wrap comment text intelligently. For example, this comment line spans
;; muliple lines but is not aligned consitently
;; with the rest of the comments/code in the file (everything else
;; ends at
;; column 68. Pressing "M-q" adjusts the comments appropriately.