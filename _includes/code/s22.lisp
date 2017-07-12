;; Enter the following function definition, then put the cursor on the
;; open parens of "(defun ..." and call "M-x slime-macro-expand-1" to
;; get a macroexpand-1. Then press "C-c M-m" to get a recursive
;; macroexpansion. See also the menu.

(defun test (n)
  (loop for i from 0 below n
     do (print i)))
