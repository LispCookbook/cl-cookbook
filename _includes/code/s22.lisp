;; Enter the following function definition, then put the cursor on the
;; open parens of "(defun ..." and press "C-c RET" (ELI) or "C-c C-b
;; k" (ILISP) to get a macroexpand-1. Then press "C-c (" (ELI) or "C-c
;; C-b C-k" (ILISP) to get a recursive macroexpansion.
(defun test (n)
  (loop for i from 0 below n
	do (print i)))
