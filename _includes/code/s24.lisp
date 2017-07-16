;; With the cursor on the "let", press "C-x C-e" (Slime) to
;; evaluate a lisp form by sending it to the REPL.

(let ((n 20))
  (loop for i from 0 below n
     do (print i)))
