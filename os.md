---
title: Interfacing with your OS
---


The ANSI Common Lisp standard doesn't mention this topic. (Keep in mind that it was written at a time where [Lisp Machines](http://kogs-www.informatik.uni-hamburg.de/~moeller/symbolics-info/symbolics.html) were at their peak. On these boxes Lisp _was_ your operating system!) So almost everything that can be said here depends on your OS and your implementation.


<a name="env"></a>

## Accessing Environment variables

Here's a function that'll allow you to look at Unix/Linux environment variables on a lot of different CL implementations:

~~~lisp
* (defun my-getenv (name &optional default)
    #+CMU
    (let ((x (assoc name ext:*environment-list*
                    :test #'string=)))
      (if x (cdr x) default))
    #-CMU
    (or
    #+Allegro (sys:getenv name)
    #+CLISP (ext:getenv name)
    #+ECL (si:getenv name)
    #+SBCL (sb-unix::posix-getenv name)
    #+LISPWORKS (lispworks:environment-variable name)
    default))
MY-GETENV
* (my-getenv "HOME")
"/home/edi"
* (my-getenv "HOM")
NIL
* (my-getenv "HOM" "huh?")
"huh?"
~~~

You should also note that some of these implementations also provide the ability to _set_ these variables. These include ECL (`si:setenv`) and AllegroCL, LispWorks, and CLISP where you can use the functions from above together with [`setf`](http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm). This feature might be important if you want to start subprocesses from your Lisp environment.


<a name="accessing-command-line"></a>

## Accessing the command line arguments

Accessing command line arguments is implementation-specific but it appears most implementations have a way of getting at them. [SBCL](http://www.sbcl.org) has the special variable `*posix-argv*`

~~~lisp
$ sbcl my-command-line-arg
~~~

....

~~~lisp
* *posix-argv*

("sbcl" "my-command-line-arg")
*
~~~

More on using this to write standalone Lisp scripts can be found in the [SBCL Manual](http://www.sbcl.org/manual/Unix_002dstyle-Command-Line-Protocol.html)

[LispWorks](http://www.lispworks.com) has `system:*line-arguments-list*`

~~~lisp
CL-USER> system:*line-arguments-list*
("/Users/cbrown/Projects/lisptty/tty-lispworks" "-init" "/Users/cbrown/Desktop/lisp/lispworks-init.lisp")
~~~

[CMUCL](http://www.cons.org/cmucl/) has interesting extensions for [manipulating the arguments](http://common-lisp.net/project/cmucl/doc/cmu-user/unix.html)

Here's a quick function to return the argument strings list across multiple implementations:

~~~lisp
(defun my-command-line ()
  (or
   #+SBCL *posix-argv*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))
~~~


<a name="fork-cmucl"></a>

## Forking with CMUCL

Here's a function by Martin Cracauer that'll allow you to compile a couple of files in parallel with [CMUCL](http://www.cons.org/cmucl/). It demonstrates how to use the UNIX [`fork`](http://www.freebsd.org/cgi/man.cgi?query=fork&apropos=0&sektion=0&manpath=FreeBSD+4.5-RELEASE&format=html) system call with this CL implementation.

~~~lisp
(defparameter *sigchld* 0)

(defparameter *compile-files-debug* 2)

(defun sigchld-handler (p1 p2 p3)
  (when (> 0 *compile-files-debug*)
    (print (list "returned" p1 p2 p3))
    (force-output))
  (decf *sigchld*))

(defun compile-files (files &key (load nil))
  (setq *sigchld* 0)
  (system:enable-interrupt unix:sigchld #'sigchld-handler)
  (do ((f files (cdr f)))
      ((not f))
    (format t "~&process ~d diving for ~a" (unix:unix-getpid)
            `(compile-file ,(car f)))
    (force-output)
    (let ((pid (unix:unix-fork)))
      (if (/= 0 pid)
          ;; parent
          (incf *sigchld*)
          ;; child
          (progn
            (compile-file (car f) :verbose nil :print nil)
            (unix:unix-exit 0)))))
  (do () ((= 0 *sigchld*))
    (sleep 1)
    (when (> 0 *compile-files-debug*)
      (format t "~&process ~d still waiting for ~d childs"
              (unix:unix-getpid) *sigchld*)))
  (when (> 0 *compile-files-debug*)
    (format t "~&finished"))
  (when load
    (do ((f files (cdr f)))
        ((not f))
      (load (compile-file-pathname (car f))))))
~~~
