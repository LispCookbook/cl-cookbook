;; Enter and evaluate the following definitions, then put the cursor
;; on "(xx)" and press "C-c C-q C-i" (ILISP) to execute "inspect" on
;; the "xx" function. Entering ":h" gives a list of help commands and
;; ":q" exits "inspect". 
(defun xx ()
  "A do-nothing function"
  nil)

(setf (documentation 'xx 'function) "A function that does nil.")

(xx)