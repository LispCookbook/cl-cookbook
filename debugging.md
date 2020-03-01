---
title: Debugging
---

You entered this new world of Lisp and now wonder: how can we debug
what's going on? How is it more interactive than other platforms?
What does the interactive debugger bring, apart from stack traces?

## Print debugging

Well of course we can use the famous technique of "print
debugging". Let's just recap a few print functions.

`print` works, it prints a `read`able representation of its argument,
which means what is `print`ed can be `read` back in by the Lisp
reader.

`princ` focuses on an *aesthetic* representation.

`format t "~a" …)`, with the *aesthetic* directive, prints a string (in `t`, the standard output
stream) and returns nil, whereas `format nil …` doesn't print anything
and returns a string. With many format controls we can print several
variables at once.

## Logging

Logging is a good evolution from print debugging ;)

[log4cl](https://github.com/sharplispers/log4cl/) is the popular,
de-facto logging library but it isn't the only one. Download it:

~~~lisp
(ql:quickload :log4cl)
~~~

and let's have a dummy variable:

~~~lisp
(defvar *foo* '(:a :b :c))
~~~

We can use log4cl with its `log` nickname, then it is as simple to use as:

~~~lisp
(log:info *foo*)
;; <INFO> [13:36:49] cl-user () - *FOO*: (:A :B :C)
~~~

We can interleave strings and expressions, with or without `format`
control strings:

~~~lisp
(log:info "foo is " *foo*)
;; <INFO> [13:37:22] cl-user () - foo is *FOO*: (:A :B :C)
(log:info "foo is ~{~a~}" *foo*)
;; <INFO> [13:39:05] cl-user () - foo is ABC
~~~

With its companion library `log4slime`, we can interactively change
the log level:

- globally
- per package
- per function
- and by CLOS methods and CLOS hierarchy (before and after methods)

It is very handy, when we have a lot of output, to turn off the
logging of functions or packages we know to work, and thus narrowing
our search to the right area. We can even save this configuration and
re-use it in another image, be it on another machine.

We can do all this through commands, keyboard shortcuts and also through a
menu or mouse clicks.

!["changing the log level with log4slime"](https://github.com/sharplispers/log4cl/raw/master/images/screenshot-15.png)

We invite you to read log4cl's readme.

## Using the powerful REPL

Part of the joy of Lisp is the excellent REPL. Its existence usually
delays the need to use other debugging tools, if it doesn't annihilate
them for the usual routine.

As soon as we define a function, we can try it in the REPL. In Slime,
compile a function with `C-c C-c` (the whole buffer with `C-c C-k`),
switch to the REPL with `C-c C-z` and try it. Eventually enter the
package you are working on with `(in-package :your-package)`.

The feedback is immediate. There is no need to recompile everything,
nor to restart any process, nor to create a main function and define
command line arguments for use in the shell (we can do this later on
when needed).

We usually need to create some data to test our function(s). This is a
subsequent art of the REPL existence and it may be a new discipline
for newcomers. A trick is to write the test data alongside your
functions but inside a `#+nil` declaration so that only you can
manually compile them:

~~~lisp
#+nil
(progn
   (defvar *test-data* nil)
   (setf *test-data* (make-instance 'foo …)))
~~~

When you load this file, `*test-data*` won't exist, but you can
manually create it with `C-c C-c`.

We can define tests functions like this.

Some do similarly inside `#| … |#` comments.

All that being said, keep in mind to write unit tests when time comes ;)


## Inspect and describe

These two commands share the same goal, printing a description of an
object, `inspect` being the interactive one.

~~~
(inspect *foo*)

The object is a proper list of length 3.
0. 0: :A
1. 1: :B

2. 2: :C
> q
~~~

We can also, in editors that support it, right-click on any object in
the REPL and `inspect` them. We are presented a screen where we can
dive deep inside the data structure and even change it.

Let's have a quick look with a more interesting structure, an object:

~~~lisp
(defclass foo ()
    ((a :accessor foo-a :initform '(:a :b :c))
     (b :accessor foo-b :initform :b)))
;; #<STANDARD-CLASS FOO>
(make-instance 'foo)
;; #<FOO {100F2B6183}>
~~~

We right-click on the `#<FOO` object and choose "inspect". We are
presented an interactive pane (in Slime):

```
#<FOO {100F2B6183}>
--------------------
Class: #<STANDARD-CLASS FOO>
--------------------
 Group slots by inheritance [ ]
 Sort slots alphabetically  [X]

All Slots:
[ ]  A = (:A :B :C)
[ ]  B = :B

[set value]  [make unbound]
```

When we click or press enter on the line of slot A, we inspect it further:

```
#<CONS {100F5E2A07}>
--------------------
A proper list:
0: :A
1: :B
2: :C
```

## The interactive debugger

Whenever an exceptional situation happens (see
[error handling](error_handling.html)), the interactive debugger pops
up.

It presents the error message, available actions (*restarts*),
and the backtrace. A few remarks:

- the restarts are programmable, we can create our own
- in Slime, press `v` on a stack trace frame to view the corresponding
  source file location
- hit enter on a frame for more details
- we can explore the functionality with the menu that should appear
  in our editor. See the "break" section below for a few
  more commands (eval in frame, etc).

Usually your compiler will optimize things out and this will reduce
the amount of information available to the debugger. For example
sometimes we can't see intermediate variables of computations. We can
change the optimization choices with:

~~~lisp
(declaim (optimize (speed 0) (space 0) (debug 3)))
~~~

and recompile our code.


## Trace

[trace](http://www.xach.com/clhs?q=trace) allows us to see when a
function was called, what arguments it received, and the value it
returned.

~~~lisp
(defun factorial (n)
  (if (plusp n)
    (* n (factorial (1- n)))
    1))
~~~

~~~lisp
(trace factorial)

(factorial 2)
  0: (FACTORIAL 3)
    1: (FACTORIAL 2)
      2: (FACTORIAL 1)
        3: (FACTORIAL 0)
        3: FACTORIAL returned 1
      2: FACTORIAL returned 1
    1: FACTORIAL returned 2
  0: FACTORIAL returned 6
6

(untrace factorial)
~~~

To untrace all functions, just evaluate `(untrace)`.

In Slime we also have the shortcut `C-c M-t` to trace or untrace a
function.

If you don't see recursive calls, that may be because of the
compiler's optimizations. Try this before defining the function to be
traced:

~~~lisp
(declaim (optimize (debug 3)))
~~~

The output is printed to `*trace-output*` (see the CLHS).

In Slime, we also have an interactive trace dialog with ``M-x
slime-trace-dialog`` bound to `C-c T`.


### Tracing method invocation

In SBCL, we can use `(trace foo :methods t)` to trace the execution order of method combination (before, after, around methods). For example:

~~~lisp
(trace foo :methods t)

(foo 2.0d0)
  0: (FOO 2.0d0)
    1: ((SB-PCL::COMBINED-METHOD FOO) 2.0d0)
      2: ((METHOD FOO (FLOAT)) 2.0d0)
        3: ((METHOD FOO (T)) 2.0d0)
        3: (METHOD FOO (T)) returned 3
      2: (METHOD FOO (FLOAT)) returned 9
      2: ((METHOD FOO :AFTER (DOUBLE-FLOAT)) 2.0d0)
      2: (METHOD FOO :AFTER (DOUBLE-FLOAT)) returned DOUBLE
    1: (SB-PCL::COMBINED-METHOD FOO) returned 9
  0: FOO returned 9
9
~~~

See the [CLOS](clos.html) section for a tad more information.

## Step

[step](http://www.xach.com/clhs?q=step) is an interactive command with
similar scope than `trace`. This:

~~~lisp
(step (factorial 2))
~~~

gives an interactive pane with the available restarts:

```
Evaluating call:
  (FACTORIAL 2)
With arguments:
  2
   [Condition of type SB-EXT:STEP-FORM-CONDITION]

Restarts:
 0: [STEP-CONTINUE] Resume normal execution
 1: [STEP-OUT] Resume stepping after returning from this function
 2: [STEP-NEXT] Step over call
 3: [STEP-INTO] Step into call
 4: [RETRY] Retry SLIME REPL evaluation request.
 5: [*ABORT] Return to SLIME's top level.
 --more--

Backtrace:
  0: ((LAMBDA ()))
  1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (LET ((SB-IMPL::*STEP-OUT* :MAYBE)) (UNWIND-PROTECT (SB-IMPL::WITH-STEPPING-ENABLED #))) #S(SB-KERNEL:LEXENV :FUNS NIL :VARS NIL :BLOCKS NIL :TAGS NIL :TYPE-RESTRICTIONS ..
  2: (SB-INT:SIMPLE-EVAL-IN-LEXENV (STEP (FACTORIAL 2)) #<NULL-LEXENV>)
  3: (EVAL (STEP (FACTORIAL 2)))
```

Stepping is useful, however it may be a sign that you need to simplify your function.

## Break

A call to [break](http://www.xach.com/clhs?q=break) makes the program
enter the debugger, from which we can inspect the call stack.


### Breakpoints in Slime

Look at the `SLDB` menu, it shows navigation keys and available
actions. Of which:

- `e` (*sldb-eval-in-frame*) prompts for an expression and evaluates
  it in the selected frame. This is how we can explore our
  intermediate variables
- `d` is similar with the addition of pretty printing the result

Once we are in a frame and detect a suspicious behavior, we can even
re-compile a function at runtime and resume the program execution from
where it stopped (using the "step-continue" restart).


## Advise and watch

*advise* and
[watch](http://www.xach.com/clhs?q=watch) are available in some
implementations, like CCL
([advise](https://ccl.clozure.com/manual/chapter4.3.html#Advising) and
[watch](https://ccl.clozure.com/manual/chapter4.12.html#watched-objects))
and [LispWorks](http://www.lispworks.com/). They do exist in
SBCL but are not exported. `advise` allows to modify a function without changing its
source, or to do something before or after its execution, similar
to CLOS method combination (before, after, around methods).

`watch` will signal a condition when a thread attempts to write to an
object being watched. It can be coupled with the display of the
watched objects in a GUI.

There is a [cl-advice](https://bitbucket.org/budden/budden-tools/src/default/cl-advice/?at=default) non-published library defining a portability layer.

## Unit tests

Last but not least, automatic testing of functions in isolation might
be what you're looking for! See the [testing](testing.html) section and a list of
[test frameworks and libraries](https://github.com/CodyReichert/awesome-cl#unit-testing).


## Remote debugging

Here's how to debug a running application on another machine.

The steps involved are to start a Swank server on the remote machine, create an
ssh tunnel, and connect to the Swank server from our editor. Then we
can browse and evaluate code of the running instance transparently.

Let's define a function that prints forever.

If needed, import the dependencies first:

~~~lisp
(ql:quickload '(:swank :bordeaux-threads))
~~~


~~~lisp
;; a little common lisp swank demo
;; while this program is running, you can connect to it from another terminal or machine
;; and change the definition of doprint to print something else out!

(require :swank)
(require :bordeaux-threads)

(defparameter *counter* 0)

(defun dostuff ()
  (format t "hello world ~a!~%" *counter*))

(defun runner ()
  (bt:make-thread (lambda ()
                    (swank:create-server :port 4006)))
  (format t "we are past go!~%")
  (loop while t do
       (sleep 5)
       (dostuff)
       (incf *counter*)))

(runner)
~~~

On the server, we can run it with

    sbcl --load demo.lisp

we do port forwarding on our development machine:

    ssh -L4006:127.0.0.1:4006 username@example.com

this will securely forward port 4006 on the server at example.com to
our local computer's port 4006 (swanks only accepts connections from
localhost).

We connect to the running swank with `M-x slime-connect`, typing in
port 4006.

We can write new code:

~~~lisp
(defun dostuff ()
  (format t "goodbye world ~a!~%" *counter*))
(setf *counter* 0)
~~~

and eval it as usual with `C-c C-c` or `M-x slime-eval-region` for instance. The output should change.

That's how Ron Garret debugged the Deep Space 1 spacecraft from the earth
in 1999:

> we were able to debug and fix a race condition that had not shown up during ground testing. (Debugging a program running on a $100M piece of hardware that is 100 million miles away is an interesting experience. Having a read-eval-print loop running on the spacecraft proved invaluable in finding and fixing the problem.


## References

- ["How to understand and use Common Lisp"](https://successful-lisp.blogspot.com/p/httpsdrive.html), chap. 30, David Lamkins (book download from author's site)
- [Malisper: debugging Lisp series](https://malisper.me/debugging-lisp-part-1-recompilation/)
- [Two Wrongs: debugging Common Lisp in Slime](https://two-wrongs.com/debugging-common-lisp-in-slime.html)
- [Slime documentation: connecting to a remote Lisp](https://common-lisp.net/project/slime/doc/html/Connecting-to-a-remote-lisp.html#Connecting-to-a-remote-lisp)
- [cvberrycom: remotely modifying a running Lisp program using Swank](http://cvberry.com/tech_writings/howtos/remotely_modifying_a_running_program_using_swank.html)
- [Ron Garret: Lisping at the JPL](http://www.flownet.com/gat/jpl-lisp.html#1994-1999%20-%20Remote%20Agent)
- [the Remote Agent experiment: debugging code from 60 million miles away (youtube)](https://www.youtube.com/watch?v=_gZK0tW8EhQ&feature=youtu.be&t=4175) (["AMA" on reddit](https://www.reddit.com/r/lisp/comments/a7156w/lisp_and_the_remote_agent/))
