---
title: Interfacing with your OS
---


The ANSI Common Lisp standard doesn't mention this topic. (Keep in mind that it was written at a time where [Lisp Machines](http://kogs-www.informatik.uni-hamburg.de/~moeller/symbolics-info/symbolics.html) were at their peak. On these boxes Lisp _was_ your operating system!) So almost everything that can be said here depends on your OS and your implementation.
There are, however, some widely used libraries, which either come with your Common Lisp implementation, or are easily
available through [Quicklisp](https://www.quicklisp.org/beta/). These include:

* ASDF3, which is included with almost all Common Lisp implementations,
  includes [Utilities for Implementation- and OS- Portability (UIOP)](https://common-lisp.net/project/asdf/uiop.html).
* [osicat](https://common-lisp.net/project/osicat/)
* [unix-opts](http://quickdocs.org/unix-opts/) is a command-line argument parser, similar to Python's `argparse`.


<a name="env"></a>

## Accessing Environment variables

UIOP comes with a function that'll allow you to look at Unix/Linux environment variables on a lot of different CL implementations:

~~~lisp
* (uiop:getenv "HOME")
  "/home/edi"
~~~

Below is an example implementation:

~~~lisp
* (defun my-getenv (name &optional default)
    "Obtains the current value of the POSIX environment variable NAME."
    (declare (type (or string symbol) name))
    (let ((name (string name)))
      (or #+abcl (ext:getenv name)
         #+ccl (ccl:getenv name)
         #+clisp (ext:getenv name)
         #+cmu (unix:unix-getenv name) ; since CMUCL 20b
         #+ecl (si:getenv name)
         #+gcl (si:getenv name)
         #+mkcl (mkcl:getenv name)
         #+sbcl (sb-ext:posix-getenv name)
         default)))
MY-GETENV
* (my-getenv "HOME")
"/home/edi"
* (my-getenv "HOM")
NIL
* (my-getenv "HOM" "huh?")
"huh?"
~~~

You should also note that some of these implementations also provide the ability to _set_ these variables. These include ECL (`si:setenv`) and AllegroCL, LispWorks, and CLISP where you can use the functions from above together with [`setf`](http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm). This feature might be important if you want to start subprocesses from your Lisp environment.

Also note that the
[Osicat](https://www.common-lisp.net/project/osicat/manual/osicat.html#Environment)
library has the method `(environment-variable "name")`, on POSIX-like
systems including Windows. It is also `fset`-able.

<a name="accessing-command-line"></a>

## Accessing the command line arguments

### Basics

Accessing command line arguments is implementation-specific but it
appears most implementations have a way of getting at
them. [Roswell](https://github.com/roswell/roswell/wiki) or external
libraries (see next section) make it portable.

[SBCL](http://www.sbcl.org) stores the arguments list in the special variable `sb-ext:*posix-argv*`

~~~lisp
$ sbcl my-command-line-arg
~~~

....

~~~lisp
* sb-ext:*posix-argv*

("sbcl" "my-command-line-arg")
*
~~~

More on using this to write standalone Lisp scripts can be found in the [SBCL Manual](http://www.sbcl.org/manual/index.html#Command_002dline-arguments)

[LispWorks](http://www.lispworks.com) has `system:*line-arguments-list*`

~~~lisp
* system:*line-arguments-list*
("/Users/cbrown/Projects/lisptty/tty-lispworks" "-init" "/Users/cbrown/Desktop/lisp/lispworks-init.lisp")
~~~

[CMUCL](http://www.cons.org/cmucl/) has interesting extensions for [manipulating the arguments](https://common-lisp.net/project/cmucl/docs/cmu-user/html/UNIX-Interface.html)

Here's a quick function to return the argument strings list across multiple implementations:

~~~lisp
(defun my-command-line ()
  (or
   #+SBCL *posix-argv*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))
~~~

Now it would be handy to access them in a portable way and to parse
them according to a schema definition.

### Parsing command line arguments

We have a look at the
[Awesome CL list#scripting](https://github.com/CodyReichert/awesome-cl#scripting)
section and we'll show how to use [unix-opts](https://github.com/mrkkrp/unix-opts).

~~~lisp
(ql:quickload "unix-opts")
~~~

We can now refer to it with its `opts` nickname.

We first declare our arguments with `opts:define-opts`, for example

~~~lisp
(opts:define-opts
    (:name :help
           :description "print this help text"
           :short #\h
           :long "help")
    (:name :level
           :description "The level of something (integer)."
           :short #\l
           :long "level"
           :arg-parser #'parse-integer))
~~~

Everything should be self-explanatory. Note that `#'parse-integer` is
a built-in CL function.

Now we can parse and get them with `opts:get-opts`, which returns two
values: the first is the list of valid options and the second the
remaining free arguments. We then must use multiple-value-bind to
catch everything:

~~~lisp
(multiple-value-bind (options free-args)
    ;; There is no error handling yet (specially for options not having their argument).
    (opts:get-opts)
~~~

We can explore this by giving a list of strings (as options) to
`get-opts`:


~~~lisp
(multiple-value-bind (options free-args)
                   (opts:get-opts '("hello" "-h" "-l" "1"))
                 (format t "Options: ~a~&" options)
                 (format t "free args: ~a~&" free-args))
Options: (HELP T LEVEL 1)
free args: (hello)
NIL
~~~

If we put an unknown option, we get into the debugger. We refer you to
unix-opts' documentation and code sample to deal with erroneous
options and other errors.

We can access the arguments stored in `options` with `getf` (it is a
property list), and we can exit (in a portable way) with
`opts:exit`. So, for example:

~~~lisp
(multiple-value-bind (options free-args)
    ;; No error handling.
    (opts:get-opts)

  (if (getf options :help)
      (progn
        (opts:describe
         :prefix "My app. Usage:"
         :args "[keywords]")
        (exit))) ;; <= exit takes an optional return status.
    ...
~~~

And that's it for now, you know the essential. See the documentation
for a complete example, and the Awesome CL list for useful packages to
use in the terminal (ansi colors, printing tables and progress bars,
interfaces to readline,…).


## Running external programs

**uiop** has us covered, and is probably included in your Common Lisp
implementation.

### Synchronously

[`uiop:run-program`](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fRUN_002dPROGRAM) either takes a string as argument, denoting the
name of the executable to run, or a list of strings, for the program and its arguments:

~~~lisp
(uiop:run-program "firefox")
~~~

or

~~~lisp
(uiop:run-program (list "firefox" "http:url"))
~~~

This will process the program output as specified and return the
processing results when the program and its output processing are
complete.

Use `:output t` to print to standard output.

This function has the following optional arguments:

~~~lisp
run-program (command &rest keys &key
                         ignore-error-status
                         (force-shell nil force-shell-suppliedp)
                         input
                         (if-input-does-not-exist :error)
                         output
                         (if-output-exists :supersede)
                         error-output
                         (if-error-output-exists :supersede)
                         (element-type #-clozure *default-stream-element-type* #+clozure 'character)
                         (external-format *utf-8-external-format*)
                       &allow-other-keys)
~~~

It will always call a shell (rather than directly executing the command when possible)
if `force-shell` is specified.  Similarly, it will never call a shell if `force-shell` is
specified to be `nil`.

Signal a continuable `subprocess-error` if the process wasn't successful (exit-code 0),
unless `ignore-error-status` is specified.

If `output` is a pathname, a string designating a pathname, or `nil` (the default)
designating the null device, the file at that path is used as output.
If it's `:interactive`, output is inherited from the current process;
beware that this may be different from your `*standard-output*`,
and under `slime` will be on your `*inferior-lisp*` buffer.
If it's `t`, output goes to your current `*standard-output*` stream.
Otherwise, `output` should be a value that is a suitable first argument to
`slurp-input-stream` (qv.), or a list of such a value and keyword arguments.
In this case, `run-program` will create a temporary stream for the program output;
the program output, in that stream, will be processed by a call to `slurp-input-stream`,
using `output` as the first argument (or the first element of `output`, and the rest as keywords).
The primary value resulting from that call (or `nil` if no call was needed)
will be the first value returned by `run-program.`
E.g., using `:output :string` will have it return the entire output stream as a string.
And using `:output '(:string :stripped t`) will have it return the same string
stripped of any ending newline.

`if-output-exists`, which is only meaningful if `output` is a string or a
pathname, can take the values `:error`, `:append`, and `:supersede` (the
default). The meaning of these values and their effect on the case
where `output` does not exist, is analogous to the `if-exists` parameter
to `open` with `:direction` `:output`.

`error-output` is similar to `output`, except that the resulting value is returned
as the second value of `run-program`. t designates the `*error-output*`.
Also `:output` means redirecting the error output to the output stream,
in which case `nil` is returned.

`if-error-output-exists` is similar to `if-output-exist`, except that it
affects `error-output` rather than `output`.

`input` is similar to `output`, except that `vomit-output-stream` is used,
no value is returned, and T designates the `*standard-input*`.

`if-input-does-not-exist`, which is only meaningful if `input` is a string
or a pathname, can take the values `:create` and `:error` (the
default). The meaning of these values is analogous to the
`if-does-not-exist` parameter to `open` with `:direction :input`.

`element-type` and `external-format` are passed on
to your Lisp implementation, when applicable, for creation of the output stream.

One and only one of the stream slurping or vomiting may or may not happen
in parallel in parallel with the subprocess,
depending on options and implementation,
and with priority being given to output processing.
Other streams are completely produced or consumed
before or after the subprocess is spawned, using temporary files.

`run-program` returns 3 values:

* the result of the `output` slurping if any, or `nil`
* the result of the `error-output` slurping if any, or `nil`
* either 0 if the subprocess exited with success status, or an
  indication of failure via the `exit-code` of the process


### Asynchronously

With [`uiop:launch-program`](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fLAUNCH_002dPROGRAM).

Its signature is the following:

~~~lisp
launch-program (command &rest keys
                         &key
                           input
                           (if-input-does-not-exist :error)
                           output
                           (if-output-exists :supersede)
                           error-output
                           (if-error-output-exists :supersede)
                           (element-type #-clozure *default-stream-element-type*
                                         #+clozure 'character)
                           (external-format *utf-8-external-format*)
                           directory
                           #+allegro separate-streams
                           &allow-other-keys)
~~~

Output (stdout) from the launched program is set using the `output`
keyword:

 - If `output` is a pathname, a string designating a pathname, or
   `nil` (the default) designating the null device, the file at that
   path is used as output.
 - If it's `:interactive`, output is inherited from the current process;
   beware that this may be different from your `*standard-output*`, and
   under Slime will be on your `*inferior-lisp*` buffer.
 - If it's `T`, output goes to your current `*standard-output*` stream.
 - If it's `:stream`, a new stream will be made available that can be accessed via
   `process-info-output` and read from.
 - Otherwise, `output` should be a value that the underlying lisp
   implementation knows how to handle. 

`if-output-exists`, which is only meaningful if `output` is a string or a
pathname, can take the values `:error`, `:append`, and `:supersede` (the
default). The meaning of these values and their effect on the case
where `output` does not exist, is analogous to the `if-exists` parameter
to `open` with `:DIRECTION :output`.

`error-output` is similar to `output`. T designates the `*error-output*`,
`:output` means redirecting the error output to the output stream,
and `:stream` causes a stream to be made available via
`process-info-error-output`.

`launch-program` returns a `process-info` object, which look like the following ([source](https://gitlab.common-lisp.net/asdf/asdf/blob/master/uiop/launch-program.lisp#L205)):


~~~lisp
(defclass process-info ()
    (
     ;; The advantage of dealing with streams instead of PID is the
     ;; availability of functions like `sys:pipe-kill-process`.
     (process :initform nil)
     (input-stream :initform nil)
     (output-stream :initform nil)
     (bidir-stream :initform nil)
     (error-output-stream :initform nil)
     ;; For backward-compatibility, to maintain the property (zerop
     ;; exit-code) <-> success, an exit in response to a signal is
     ;; encoded as 128+signum.
     (exit-code :initform nil)
     ;; If the platform allows it, distinguish exiting with a code
     ;; >128 from exiting in response to a signal by setting this code
     (signal-code :initform nil)))
~~~

See the [docstrings](https://gitlab.common-lisp.net/asdf/asdf/blob/master/uiop/launch-program.lisp#L508).

#### Test if a subprocess is alive

`uiop:process-alive-p` tests if a process is still alive, given a
`process-info` object returned by `launch-program`:

~~~lisp
* (defparameter *shell* (uiop:launch-program "bash" :input :stream :output :stream))

;; inferior shell process now running
* (uiop:process-alive-p *shell*)
T

;; Close input and output streams
* (uiop:close-streams *shell*)
* (uiop:process-alive-p *shell*)
NIL
~~~

#### Get the exit code

We can use `uiop:wait-process`. If the process is finished, it returns
immediately, and returns the exit code. If not, it waits for the
process to terminate.

~~~lisp
(uiop:process-alive-p *process*)
NIL
(uiop:wait-process *process*)
0
~~~

An exit code to 0 means success (use `zerop`).

The exit code is also stored in the `exit-code` slot of our
`process-info` object. We see from the class definition above that it
has no accessor, so we'll use `slot-value`. It has an `initform` to
nil, so we don't have to check if the slot is bound.  We can do:

~~~lisp
(slot-value *my-process* 'uiop/launch-program::exit-code)
0
~~~

The trick is that we *must* run `wait-process` beforehand, otherwise
the result will be `nil`.

Since `wait-process` is blocking, we can do it on a new thread:

~~~lisp
(bt:make-thread
  (lambda ()
    (let ((exit-code (uiop:wait-process
                       (uiop:launch-program (list "of" "commands"))))
      (if (zerop exit-code)
          (print :success)
          (print :failure)))))
  :name "Waiting for <program>")
~~~


Note that `run-program` returns the exit code as the third value.


### Input and output from subprocess

If the `input` keyword is set to `:stream`, then a stream is created
and can be written to in the same way as a file. The stream can be
accessed using `uiop:process-info-input`:

~~~lisp
;; Start the inferior shell, with input and output streams
* (defparameter *shell* (uiop:launch-program "bash" :input :stream :output :stream))
;; Write a line to the shell
* (write-line "find . -name '*.md'" (uiop:process-info-input *shell*))
;; Flush stream
* (force-output (uiop:process-info-input *shell*))
~~~

where [write-line](http://clhs.lisp.se/Body/f_wr_stg.htm) writes the
string to the given stream, adding a newline at the end. The
[force-output](http://clhs.lisp.se/Body/f_finish.htm) call attempts to
flush the stream, but does not wait for completion.

Reading from the output stream is similar, with
`uiop:process-info-output` returning the output stream:

~~~lisp
* (read-line (uiop:process-info-output *shell*))
~~~

In some cases the amount of data to be read is known, or there are
delimiters to determine when to stop reading. If this is not the case,
then calls to [read-line](http://clhs.lisp.se/Body/f_rd_lin.htm) can
hang while waiting for data. To avoid this,
[listen](http://clhs.lisp.se/Body/f_listen.htm) can be used to test if
a character is available:

~~~lisp
* (let ((stream (uiop:process-info-output *shell*)))
     (loop while (listen stream) do
         ;; Characters are immediately available
         (princ (read-line stream))
         (terpri)))
~~~

There is also
[read-char-no-hang](http://clhs.lisp.se/Body/f_rd_c_1.htm) which reads
a single character, or returns `nil` if no character is available.
Note that due to issues like buffering, and the timing of when the
other process is executed, there is no guarantee that all data sent
will be received before `listen` or `read-char-no-hang` return `nil`.

## Piping

Here's an example to do the equivalent of `ls | sort`. Note that "ls"
uses `launch-program` (async) and outputs to a stream, where "sort",
the last command of the pipe, uses `run-program` and outputs to a
string.

~~~lisp
(uiop:run-program "sort"
                   :input
                   (uiop:process-info-output
                    (uiop:launch-program "ls"
                                         :output :stream))
                   :output :string)
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
