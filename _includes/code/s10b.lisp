(defun test (n)
  (loop for i from 0 below n
      do (print i)))

(test 100)
(test 200)

(defun test-format ()
  (format t "This is a test.~%")
  (format t "This is another test.~%"))

(test-format)

(defun test-format-loop (n)
  (loop for i from 0 below (1+ n)
   do (test-format)
      (sleep 1)))

(test-format-loop 5)

(time (test-format-loop 5))

(test-format-loop 100)

(time (test-format-loop 100))