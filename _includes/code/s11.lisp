;; Compile the entire buffer by pressing "C-c C-b" (ELI) or "C-c C-k
;; C-b" (ILISP).

;; Compile a region by selecting the first 2 forms in test-all and
;; pressing "C-c C-r" (ELI) or "C-c C-k C-r" (ILISP).

;; Compile a defun by putting the cursor inside the "test-format"
;; defun and pressing "C-c C-x" (ELI) or "C-c C-k C-d" (ILISP).

;; Compile the sexp before the point by putting the cursor after the
;; closing paren of "(test-format)" and pressing "C-c C-s" (ELI). 

;; As a general rule, to evaluate rather than compile, press "C-u"
;; before the ELI command (e.g. -- "C-u C-c C-s" to evaluate the sexp
;; before the point instead of "C-c C-s" to compile it) or enter the
;; ILISP key sequence with a "C-c C-j" prefix rather than a "C-c C-k"
;; prefix (e.g. -- use "C-c C-j C-d" to evaluate a defun instead of
;; "C-c C-k C-d" to compile the defun)

;; The "Do What I Mean" evaluation/compilation functions work on the
;; following basis: If a region is selected, process the region.  If
;; the cursor is on or immediately after a ')', process the last sexp.
;; If the cursor is on or immediately before a '(', process the next
;; sexp. If the cursor is inside a defun, process the defun. If the
;; cursor is inside a top-level sexp, process the top-level
;; sexp. Tests are done in the order specified, so (if there is any
;; ambiguity), make certain that the cursor is either on a parenthesis
;; (for the last/next commands or not directly before/after/on a
;; parenthesis for the defun/top-level commands.  Press "C-c d" (ELI)
;; or "C-c C-j C-j" (ILISP).

(defun test (n)
  (loop for i from 0 below n
	do (print i)))

(defun test-format ()
  (format t "This is a test.~%"))

(defun test-format-loop (n)
  (loop for i from 0 below n
	do (test-format)
	(sleep 1)))

(defun test-all ()
  (test 5)
  (test-format)
  (test-format-loop 5))