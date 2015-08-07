;; Put the cursor on the open parens of "(defun ..." and press "C-M-q"
;; to indent the code:
(defun e ()
"A badly indented function."
(let ((x 20))
(loop for i from 0 to x 
do (loop for j from 0 below 10 
do (print j)) 
(if (< i 10)
(let ((z nil) )
(setq z (format t "x=~d" i))
(print z))))))

;; This is the result:
(defun e ()
  "A badly indented function."
  (let ((x 20))
    (loop for i from 0 to x 
	do (loop for j from 0 below 10 
	       do (print j)) 
	   (if (< i 10)
	       (let ((z nil) )
		 (setq z (format t "x=~d" i))
		 (print z))))))
