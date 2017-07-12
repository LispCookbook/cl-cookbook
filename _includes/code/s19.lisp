;; Enter and evaluate the following definitions, then put the cursor
;; on "(xx)" and press "C-c C-d d" to get the function documentation.

(defun xx ()
  "A do-nothing function"
  nil)

(setf (documentation 'xx 'function) "A function that does nil.")

(xx)
