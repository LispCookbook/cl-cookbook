;; Enter and evaluate the following definitions, then put the cursor
;; on "(xx)" and press "C-c I" (Slime) to execute "inspect" on
;; the "xx" function. Entering "h" gives a list of help commands and
;; "q" exits the inspector.

(defun xx ()
  "A do-nothing function"
  nil)

(setf (documentation 'xx 'function) "A function that does nil.")

(xx)
