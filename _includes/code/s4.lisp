;; Placing the cursor on a "(" or after a ")" highlights the matching
;; parens:
(progn (+ 3 3) (- 2 2))

;; A mismatched parens is highlighted in a different color (put cursor
;; after last parens and enter a ")" to see this:
(- 2 2)

;; You can also type "M-x check-parens" to locate mismatched parens in
;; a buffer

;; Press "M-(" and you will get:
() 

;; with the cursor placed on the closing parens, ready to enter the
;; function name. 

;; Put the cursor on the open parens of the "(+ 2 2)" sexp below and
;; press "C-u 2 M-(" to enclose the next 2 sexps with parens - then
;; type "+ 1" to add "1" to the result of the following 2 sexps:
(progn (+ 2 2) (+ 3 3))

;; To delete the enclosing "progn" below, put the cursor on the open
;; parens of the "(+ 1 1)" and press the following sequence of keys:
;; "C-M-k C-M-k C-M-k C-M-u C-M-k C-y M-y C-M-a C-M-q":
(defun a ()
  (progn 
    (+ 1 1)
    (+ 2 2)
    (+ 3 3)))
