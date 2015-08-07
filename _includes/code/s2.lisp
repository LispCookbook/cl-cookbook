;; Put the cursor on the open parens of "(progn .." and press "C-M-k"
;; to delete it. Then press "C-M-backspace" to delete the sexp before
;; the cursor:
(defun d ()
  (if t  
      (+ 3 3)
    (progn
      (+ 1 1)
      (if t
	  (+ 2 2)
	(+ 3 3)))
    (+ 4 4)))