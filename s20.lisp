;; Enter and evaluate the following definitions, then put the cursor
;; on "(xx)" and press "C-c C-q C-d" (ILISP) to get a description of
;; the function.
(defun xx ()
  "A do-nothing function"
  nil)

(setf (documentation 'xx 'function) "A function that does nil.")

(xx)