---
title: Defining Systems
---

A **system** is a collection of Lisp files that together constitute an application or a library, and that should therefore be managed as a whole. A **system definition** describes which source files make up the system, what the dependencies among them are, and the order they should be compiled and loaded in.


## ASDF

[ASDF](https://gitlab.common-lisp.net/asdf/asdf) is the standard build
system for Common Lisp. It is shipped in most Common Lisp
implementations. It includes
[UIOP](https://gitlab.common-lisp.net/asdf/asdf/blob/master/uiop/README.md),
_"the Utilities for Implementation- and OS- Portability"_. You can read
[its manual](https://common-lisp.net/project/asdf/asdf.html) and the
[tutorial and best practices](https://gitlab.common-lisp.net/asdf/asdf/blob/master/doc/best_practices.md).

<a name="example"></a>

## Simple examples

### Loading a System

The most trivial use of ASDF is by calling `(asdf:make :foobar)` (or `load-system`)
to load your library.
Then you can use it.
For instance, if it exports a function `some-fun` in its package `foobar`,
then you will be able to call it with `(foobar:some-fun ...)` or with:

~~~lisp
(in-package :foobar)
(some-fun ...)
~~~

You can also use Quicklisp:

~~~lisp
(ql:quickload :foobar)
~~~

### Testing a System

To run the tests for a system, you may use:

~~~lisp
(asdf:test-system :foobar)
~~~

The convention is that an error SHOULD be signalled if tests are unsuccessful.

### Designating a system

The proper way to designate a system in a program is with lower-case
strings, not symbols, as in:

~~~lisp
(asdf:make "foobar")
(asdf:test-system "foobar")
~~~

### Trivial System Definition

A trivial system would have a single Lisp file called `foobar.lisp`.
That file would depend on some existing libraries,
say `alexandria` for general purpose utilities,
and `trivia` for pattern-matching.
To make this system buildable using ASDF,
you create a system definition file called `foobar.asd`,
with the following contents:

~~~lisp
(defsystem "foobar"
  :depends-on ("alexandria" "trivia")
  :components ((:file "foobar")))
~~~


Note how the type `lisp` of `foobar.lisp`
is implicit in the name of the file above.
As for contents of that file, they would look like this:

~~~lisp
(defpackage :foobar
  (:use :common-lisp :alexandria :trivia)
  (:export
   #:some-function
   #:another-function
   #:call-with-foobar
   #:with-foobar))

(in-package :foobar)

(defun some-function (...)
    ...)
...
~~~

Instead of `using` multiple complete packages, you might want to just import parts of them:

~~~lisp
(defpackage :foobar
  (:use #:common-lisp)
  (:import-from #:alexandria
                #:some-function
                #:another-function))
  (:import-from #:trivia
                #:some-function
                #:another-function))
...)                
~~~


#### Using the system you defined

Assuming your system is installed under `~/common-lisp/`,
`~/quicklisp/local-projects/` or some other filesystem hierarchy
already configured for ASDF, you can load it with: `(asdf:make "foobar")`.

If your Lisp was already started when you created that file,
you may have to `(asdf:clear-configuration)` to re-process the configuration.


### Trivial Testing Definition

Even the most trivial of systems needs some tests,
if only because it will have to be modified eventually,
and you want to make sure those modifications don't break client code.
Tests are also a good way to document expected behavior.

The simplest way to write tests is to have a file `foobar-tests.lisp`
and modify the above `foobar.asd` as follows:

~~~lisp
(defsystem "foobar"
    :depends-on ("alexandria" "trivia")
    :components ((:file "foobar"))
    :in-order-to ((test-op (test-op "foobar/tests"))))

(defsystem "foobar/tests"
    :depends-on ("foobar" "fiveam")
    :components ((:file "foobar-tests"))
    :perform (test-op (o c) (symbol-call :fiveam '#:run! :foobar)))
~~~

The `:in-order-to` clause in the first system
allows you to use `(asdf:test-system :foobar)`
which will chain into `foobar/tests`.
The `:perform` clause in the second system does the testing itself.

In the test system, `fiveam` is the name of a popular test library,
and the content of the `perform` method is how to invoke this library
to run the test suite `:foobar`.
Obvious YMMV if you use a different library.

## Create a project skeleton

[cl-project](https://github.com/fukamachi/cl-project) can be used to
generate a project skeleton. It will create a default ASDF definition,
it generates a system for unit testing, etc.

Install with

    (ql:quickload :cl-project)

Create a project:

~~~lisp
(cl-project:make-project #p"lib/cl-sample/"
:author "Eitaro Fukamachi"
:email "e.arrows@gmail.com"
:license "LLGPL"
:depends-on '(:clack :cl-annot))
;-> writing /Users/fukamachi/Programs/lib/cl-sample/.gitignore
;   writing /Users/fukamachi/Programs/lib/cl-sample/README.markdown
;   writing /Users/fukamachi/Programs/lib/cl-sample/cl-sample-test.asd
;   writing /Users/fukamachi/Programs/lib/cl-sample/cl-sample.asd
;   writing /Users/fukamachi/Programs/lib/cl-sample/src/hogehoge.lisp
;   writing /Users/fukamachi/Programs/lib/cl-sample/t/hogehoge.lisp
;=> T
~~~
