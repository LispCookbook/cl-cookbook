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

