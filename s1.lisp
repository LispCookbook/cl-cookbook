;; Put the cursor on the open parens of "(defvar.." and press "C-M-f"
;; and "C-M-b" a few times to see how you move in units of sexps. Put
;; the cursor on the second additon in the "(progn" statement and
;; press "C-M-t" to swap the first addition sexp and the second
;; one. Put the cursor on the open parens of "(+ x" in defun c and
;; press "C-M-@" to highlight the entire sexp. Then press "C-M-u" to
;; expand the selection "upwards" to the enclosing "(let". Pressing
;; "C-M-d" moves to the next enclosed sexp or (if you are at the
;; beginning of a line) to the enclosed sexp on the line: 
(defvar a "a variable")

(defun b ()
  "a function"
  (+ 2 2))

(defun c ()
  "another function"
  (let ((x 42))
    (+ x
       (+ 2 2)
       (+ 3 3)
       (+ 4 4))))

(progn
  (+ 1 1)
  (+ 2 2)
  (+ 3 3))