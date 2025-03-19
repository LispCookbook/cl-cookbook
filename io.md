---
title: Input/Output
---

Let's see some useful patterns for input/output.

## Asking for user input: `read`, `read-line`

The [`read`](https://cl-community-spec.github.io/pages/read.html)
function, when called with no other argument, stops the world and
waits for user input:

~~~lisp
CL-USER> (read)
|               ;; <---- point waiting for input.
~~~

Type in `(1+ 2)` and you will see this result:

```lisp
(1+ 2)
```

Did you expect to see 3?

Let's try again:

~~~lisp
(read)
hello  ;; our input
HELLO  ;; return value: a symbol, hence capitalized

(read)
(list a b c)   ;; our input
(LIST A B C)   ;; return value: some s-expression.
~~~

The results were *not* evaluated.

The `read` function reads source code. Not strings! It returns an
s-expression.

If you want a string, write it inside quotes (or see the next section).

It doesn't evaluate the code yet. For this, we have `eval`:

~~~lisp
(read)
(1+ 2)  ;; input
(1+ 2)  ;; return value

(eval *)
;; 3
~~~

This is how we can build the most primite REPL: a Read-Eval-Print-Loop.

~~~lisp
CL-USER> (loop (print (eval (read))))
(1+ 1)  ;; input

2 (1+ 2)  ;; result + my next input

3
~~~

The above line will loop forever and you have no way to escape it,
appart `(quit)` which quits your top-level REPL too. Here's a very
simple loop that quits when seeing the `q` symbol (not a string):

~~~lisp
(loop for input = (read)
      while (not (equal input 'q))
      do (print (eval input)))
~~~


### Read input as string: `read-line`

To read the input as a *string*, use [`read-line`](https://cl-community-spec.github.io/pages/read_002dline.html):

~~~lisp
CL-USER> (defparameter *input* (read-line))
This is a longer input.
*INPUT*

CL-USER> *input*
"This is a longer input."
~~~

It doesn't read multiple lines.


## Redirecting the standard output of your program

You do it like this:

~~~lisp
(let ((*standard-output* <some form generating a stream>))
  ...)
~~~

Because
[`*standard-output*`](http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm)
is a dynamic variable, all references to it during execution of the body of the
`LET` form refer to the stream that you bound it to. After exiting the `LET`
form, the old value of `*STANDARD-OUTPUT*` is restored, no matter if the exit
was by normal execution, a `RETURN-FROM` leaving the whole function, an
exception, or what-have-you. (This is, incidentally, why global variables lose
much of their brokenness in Common Lisp compared to other languages: since they
can be bound for the execution of a specific form without the risk of losing
their former value after the form has finished, their use is quite safe; they
act much like additional parameters that are passed to every function.)

If the output of the program should go to a file, you can do the following:

~~~lisp
(with-open-file (*standard-output* "somefile.dat"
                                   :direction :output
                                   :if-exists :supersede)
  ...)
~~~

[`with-open-file`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_open.htm)
opens the file - creating it if necessary - binds `*standard-output*`, executes
its body, closes the file, and restores `*standard-output*` to its former
value. It doesn't get more comfortable than this!<a name="faith"></a>

## Faithful output with character streams

By _faithful output_ I mean that characters with codes between 0 and 255 will be
written out as is. It means, that I can `(princ (code-char 0..255) s)` to a
stream and expect 8-bit bytes to be written out, which is not obvious in the
times of Unicode and 16 or 32 bit character representations. It does _not_
require that the characters ä, ß, or þ must have their
[`char-code`](http://www.lispworks.com/documentation/HyperSpec/Body/f_char_c.htm)
in the range 0..255 - the implementation is free to use any code. But it does
require that no `#\Newline` to CRLF translation takes place, among others.

Common Lisp has a long tradition of distinguishing character from byte (binary)
I/O,
e.g. [`read-byte`](http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_by.htm)
and
[`read-char`](http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_cha.htm)
are in the standard. Some implementations let both functions be called
interchangeably. Others allow either one or the other. (The
[simple stream proposal](https://www.cliki.net/simple-stream) defines the
notion of a _bivalent stream_ where both are possible.)

Varying element-types are useful as some protocols rely on the ability to send
8-Bit output on a channel. E.g. with HTTP, the header is normally ASCII and
ought to use CRLF as line terminators, whereas the body can have the MIME type
application/octet-stream, where CRLF translation would destroy the data. (This
is how the Netscape browser on MS-Windows destroys data sent by incorrectly
configured Webservers which declare unknown files as having MIME type
text/plain - the default in most Apache configurations).

 What follows is a list of implementation dependent choices and behaviours and some code to experiment.

### SBCL

To load arbitrary bytes into a string, use the `:iso-8859-1` external format. For example:

~~~lisp
(uiop:read-file-string "/path/to/file" :external-format :iso-8859-1)
~~~

### CLISP

On CLISP, faithful output is possible using

~~~lisp
:external-format
(ext:make-encoding :charset 'charset:iso-8859-1
                   :line-terminator :unix)
~~~

You can also use `(SETF (STREAM-ELEMENT-TYPE F) '(UNSIGNED-BYTE 8))`, where the
ability to `SETF` is a CLISP-specific extension. Using `:EXTERNAL-FORMAT :UNIX`
will cause portability problems, since the default character set on MS-Windows
is `CHARSET:CP1252`. `CHARSET:CP1252` doesn't allow output of e.g. `(CODE-CHAR
#x81)`:

~~~
;*** - Character #\u0080 cannot be represented in the character set CHARSET:CP1252
~~~

Characters with code > 127 cannot be represented in ASCII:

~~~
;*** - Character #\u0080 cannot be represented in the character set CHARSET:ASCII
~~~

### AllegroCL

`#+(AND ALLEGRO UNIX) :DEFAULT` (untested) - seems enough on UNIX, but would not
work on the MS-Windows port of AllegroCL.

### LispWorks

`:EXTERNAL-FORMAT '(:LATIN-1 :EOL-STYLE :LF)` (confirmed by Marc Battyani)

### Example

Here's some sample code to play with:

~~~lisp
(defvar *unicode-test-file* "faithtest-out.txt")

(defun generate-256 (&key (filename *unicode-test-file*)
			  #+CLISP (charset 'charset:iso-8859-1)
                          external-format)
  (let ((e (or external-format
	       #+CLISP (ext:make-encoding :charset charset
                           :line-terminator :unix))))
    (describe e)
    (with-open-file (f filename :direction :output
		     :external-format e)
      (write-sequence
        (loop with s = (make-string 256)
	      for i from 0 to 255
	      do (setf (char s i) (code-char i))
	      finally (return s))
       f)
      (file-position f))))

;(generate-256 :external-format :default)
;#+CLISP (generate-256 :external-format :unix)
;#+CLISP (generate-256 :external-format 'charset:ascii)
;(generate-256)

(defun check-256 (&optional (filename *unicode-test-file*))
  (with-open-file (f filename :direction :input
		     :element-type '(unsigned-byte 8))
    (loop for i from 0
	  for c = (read-byte f nil nil)
	  while c
	  unless (= c i)
	  do (format t "~&Position ~D found ~D(#x~X)." i c c)
	  when (and (= i 33) (= c 32))
	  do (let ((c (read-byte f)))
	       (format t "~&Resync back 1 byte ~D(#x~X) - cause CRLF?." c c) ))
    (file-length f)))

#| CLISP
(check-256 *unicode-test-file*)
(progn (generate-256 :external-format :unix) (check-256))
; uses UTF-8 -> 385 bytes

(progn (generate-256 :charset 'charset:iso-8859-1) (check-256))

(progn (generate-256 :external-format :default) (check-256))
; uses UTF-8 + CRLF(on MS-Windows) -> 387 bytes

(progn (generate-256 :external-format
  (ext:make-encoding :charset 'charset:iso-8859-1 :line-terminator :mac)) (check-256))
(progn (generate-256 :external-format
  (ext:make-encoding :charset 'charset:iso-8859-1 :line-terminator :dos)) (check-256))
|#
~~~

<a name="bulk"></a>

## Fast bulk I/O

If you need to copy a lot of data and the source and destination are both
streams (of the same
[element type](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_e.htm#element_type)),
it's very fast to use
[`read-sequence`](http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_seq.htm)
and
[`write-sequence`](http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_seq.htm):

~~~lisp
(let ((buf (make-array 4096 :element-type (stream-element-type input-stream))))
  (loop for pos = (read-sequence buf input-stream)
        while (plusp pos)
        do (write-sequence buf output-stream :end pos)))
~~~
