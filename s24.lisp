;; With the cursor on the "let", press "C-c x" (ELI or ILISP) to
;; evaluate a lisp form by copying it into the listener.
(let ((n 20))
  (loop for i from 0 below n
      do (print i)))

;; In ILISP, most of the eval & compile functions have an "and go"
;; equivalent that transfers the focus to the listener after
;; execution. Their key bindings are the same as the normal
;; eval/compile functions except that the final key does not have a
;; "Ctrl" prefix. For example, put the cursor at the open paren of the
;; "(let ..." sexp above and press "C-c C-j C-n" to evaluate in the
;; source buffer. Then, press "C-c C-j n" to evaluate "and go" to the
;; listener.
