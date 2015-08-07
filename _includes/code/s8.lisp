;; Highlight the middle "(if ..." block and press "C-x n n" to hide
;; everything but that block ("C-x n w" restores the other code):
(if a 
    (+ 1 1))
(if b
    (+ 2 2))
(if c
    (+ 3 3))

;; Put the cursor on "(defun b ..." and press "C-x n d" to narrow to
;; just defun b (("C-x n w" restores the other code):
(defun a ()
  (+ 1 1))

(defun b ()
  (+ 2 2))

(defun c ()
  (+ 3 3))