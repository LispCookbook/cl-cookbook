;; Use the Emacs "occur" function to find all occurances of a string
;; (or regexp) in a buffer. Example: Enter "M-x occur" and enter the
;; string "defun" to get a list of all the occurances of the
;; characters "defun" in the current buffer.

(defvar aa "a" "a variable")

(defun b ()
  "a function"
  (+ 2 2))

(defun c ()
  "another function"
  (+ 3 3))

(defmacro d (x)
  "a macro"
  `(list ,x))

;; Use the Emacs "grep" function to find all occurances of a string
;; (or regexp) in a multiple files. Example: Enter "M-x grep" and
;; enter the string "defun *.lisp" to get a list of all the function
;; definitions in lisp files in the current directory.