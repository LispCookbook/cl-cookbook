;; Enter and evaluate the following definitions, then put the cursor
;; on "(xx)" and press "C-c C-f" (ELI) or "C-c C-q C-o" (ILISP) to get
;; the function documentation.
(defun xx ()
  "A do-nothing function"
  nil)

(setf (documentation 'xx 'function) "A function that does nil.")

(xx)