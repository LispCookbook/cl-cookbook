;; $Id$

(in-package "EDITOR")

;;                            PRESENT.LISP
;;           Nick Levine, Ravenbrook Limited, 2002-09-27
;; 
;; Copyright (c) 2002 Nick Levine.
;; 
;; Experimental package for presenting a file of lisp forms, in order,
;; in a lisp listener when in "presentation mode". PowerPoint for
;; LispWorks.
;;
;; F12 to scroll to the next form, Control-F12 to scroll up.
;; F1 through F10 represent digits 1 ... 0. Use these to build up a
;; decimal number and F11 to jump to that position in the list of
;; forms. If you jump by more than 1 position, your old position is
;; pushed onto a stack. Use Control-F11 to pop the stack.
;; (file-to-forms <file>) to create the list of forms in the first
;; place.
;;
;; This code, originally written to support an SQL tutorial (ILC 2002)
;; attempts to work alongside the [...] SQL notation.
;;
;; The pretty printer doesn't do a brilliant job. Let me know how you
;; improved this and I'll gladly incorporate your changes (and
;; acknowledge your help - fame indeed).
;;
;; Note (of course) that working in a system package - such as EDITOR
;; - is strongly discouraged by the system developers. As is using
;; editor internals.
;;
;; This document is provided "as is", without any express or implied
;; warranty.  In no event will the author be held liable for any
;; damages arising from the use of this document.  You may make and
;; distribute verbatim copies of this document provided that you do
;; not charge a fee for this document or for its distribution.

(defparameter *eval-defstructs* t)

(defvar *forms* nil)

(defvar *current* -1)

(defvar *stack* nil)

(defvar *building-n* nil)

(defun reset () 
  (setf *stack* () *building-n* nil *current* -1))

(defun pop-unnecessary-form (n)
  (when (and (car *stack*)
             (<= (abs (- (car *stack*) n))
                 1))
     (pop *stack*)))

(defun push-form (n)
  (pop-unnecessary-form n)
  (let ((form (nth n *forms*)))
    (push n *stack*)
    form))

(defun insert-nth-form (n)
  (let ((form (push-form n)))
    (insert-given-form form)))

(defun clear-after-prompt ()
  (when-let* ((stream (variable-value 'rubber-stream :current nil))
              (start (editor-region-stream-start stream))
              (end (editor-region-stream-end stream)))
    (delete-points-and-save start end)))
    
(defun insert-given-form (form)
  (clear-after-prompt)
  (let ((*print-case* :downcase)
        (*print-length* nil)
        (*print-level* nil))
    (setf *building-n* nil)
    (end-of-buffer-command nil)
    (insert-form-at-point (current-point)       ; pretty prints
                          (reintern-symbols form))
    (end-of-buffer-command nil)
    (pop-mark-command nil)
    (stats)))

(defun reintern-symbols (form)
  (typecase form
    (cons (cons (reintern-symbols (car form))
                (reintern-symbols (cdr form))))
    (symbol (if (symbol-package form)
                form
              (intern (symbol-name form))))
    (otherwise form)))      

(defun insert-prior ()
  (when (plusp *current*)
    (insert-nth-form (decf *current*))))

(defun insert-this ()
  (when *building-n*
    (insert-nth-form (setf *current* *building-n*))))

(defun insert-next ()
  (when (< *current* (1- (length *forms*)))
    (insert-nth-form (incf *current*))))

(defun insert-pop-stack ()
  (pop *stack*)
  (when *stack*
    (insert-nth-form (setf *current* (car *stack*)))))

(defmacro def-key-command (name key &body body)
  `(progn
     (defcommand ,name (p) "" "" (declare (ignorable p)) ,@body)
     (bind-key ,name ,key :mode "Execute")))

(setup-indent "def-key-command" 1 2 8)

(def-key-command "Insert Next" #\F12
  (insert-next))

(def-key-command "Insert Prior" #\Control-F12
   (insert-prior))

(def-key-command "Insert This" #\F11
  (insert-this))

(def-key-command "Insert Pop Stack"  #\Control-F11
  (insert-pop-stack))

(defun add-to-build (n)
  (setf *building-n*
        (+ n (* 10 (or *building-n* 0)))))

(defmacro def-building-key (n key)
  (let ((command-name (format nil "Build Key ~d" n)))
    `(def-key-command ,command-name ,key
       (add-to-build ,n))))

(def-building-key 0 #\F10)
(def-building-key 1 #\F1)
(def-building-key 2 #\F2)
(def-building-key 3 #\F3)
(def-building-key 4 #\F4)
(def-building-key 5 #\F5)
(def-building-key 6 #\F6)
(def-building-key 7 #\F7)
(def-building-key 8 #\F8)
(def-building-key 9 #\F9)

(defun stats ()
  (message "Current = ~d / Stack = ~a" *current* *stack*))

(defun editor-right-margin ()
  (when-let* ((buffer (current-buffer))
              (window (car (buffer-windows buffer))))
    (slot-value window 'width)))    

;; SQL character handling

(set-syntax-from-char #\] #\))

(defstruct sql-expr
  things)

(defun pprint-sql-expr (*standard-output* sql-expr)
  (let ((things (sql-expr-things sql-expr)))
    (pprint-logical-block (*standard-output* things :prefix "[" :suffix "]")
      (pprint-exit-if-list-exhausted)
      (loop (write (pprint-pop))
            (pprint-exit-if-list-exhausted)
            (write-char #\Space)
            (pprint-newline :fill)))))

(set-pprint-dispatch 'sql-expr 'pprint-sql-expr)

(defun read-sql-form (stream ignore)
  (declare (ignore ignore))
  (let ((things (read-delimited-list #\] stream)))
    (make-sql-expr :things things)))

(defun file-to-forms (file)
  (reset)
  (multiple-value-bind (previous-macro-character non-terminating-p)
      (get-macro-character #\[)
    (let ((forms nil)
	  (eof (cons nil nil)))
      (unwind-protect
	  (with-open-file (istream file)
	    (set-macro-character #\[ 'read-sql-form)
	    (loop (let ((next (read istream nil eof)))
		    (when (eq next eof) 
		      (return))
                    (when (and (consp next) (eq (car next) 'defstruct) *eval-defstructs*)
                      (eval next))
		    (push next forms))))
	(set-macro-character #\[
			     previous-macro-character
			     (or (null previous-macro-character)
				 non-terminating-p)))
      (setf *forms* (reverse forms))
      file)))

(defvar *examples* (current-pathname "examples" "lisp"))

(defun cl-user::prepare ()
  ;; Uncomment the following to enable SQL character handling...
  ;; (require "odbc")
  ;; (use-package :sql)
  (file-to-forms *examples*))


