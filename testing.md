---
title: Testing
---

So you want to easily test the code you're writing ? The following
recipes cover how to write automated tests and see their code
coverage. We also give pointers to plug those in modern continuous
integration services like Travis CI and Coveralls.

We will use an established and well-designed regression testing
framework called [Prove](https://github.com/fukamachi/prove). It is
not the only possibility though,
[FiveAM](http://quickdocs.org/fiveam/api) is a popular one. We prefer
`Prove` for its doc and its **extensible reporters** (it has different
report styles and we can extend them).


<a name="install"></a>

## Testing with Prove

### Install and load

`Prove` is in Quicklisp:

~~~lisp
(ql:quickload :prove)
~~~

This command installs `prove` if necessary, and loads it.

<a name="writetest"></a>

### Write a test file

~~~lisp
(in-package :cl-user)
(defpackage my-test
  (:use :cl
        :prove))
(in-package :my-test)

(subtest "Showing off Prove"
  (ok (not (find 4 '(1 2 3))))
  (is 4 4)
  (isnt 1 #\1))

~~~

Prove's API contains the following testing functions: `ok`, `is`,
`isnt`, `is-values`, `is-type`, `like` (for regexps), `is-print`
(checks the standard output), `is-error`, `is-expand`, `pass`, `fail`,
`skip`, `subtest`.


### Run a test file

~~~lisp
(prove:run #P"myapp/tests/my-test.lisp")
(prove:run #P"myapp/tests/my-test.lisp" :reporter :list)
~~~

We get an output like:

<img src="assets/prove-report.png"
     style="max-width: 800px"/>


### More about Prove

`Prove` can also:

* be run on **Travis CI**,
* **colorize** the output,
* report **tests duration**,
* change the default test function,
* set a treshold for slow tests,
* invoke the **CL debugger** whenever getting an error during running tests,
* integrate with **ASDF** so than we can execute `(asdf:test-system)` or
  `(prove:run)` in the REPL (such configuration is provided by
  [cl-project](https://github.com/fukamachi/cl-project), by the same
  author).

See [Prove's documentation](https://github.com/fukamachi/prove) !

## Code coverage

A code coverage tool produces a visual output that allows to see what
parts of our code were tested or not:


![](assets/coverage.png "source: https://www.snellman.net/blog/archive/2007-05-03-code-coverage-tool-for-sbcl.html")


### Generating an html test coverage output

SBCL comes with a built-in module to do code coverage analysis:
[sb-cover](http://www.sbcl.org/manual/index.html#sb_002dcover).

Coverage reports are only generated for code compiled using
`compile-file` with the value of the `sb-cover:store-coverage-data`
optimization quality set to 3.

~~~lisp
;;; Load SB-COVER
(require :sb-cover)

;;; Turn on generation of code coverage instrumentation in the compiler
(declaim (optimize sb-cover:store-coverage-data))

;;; Load some code, ensuring that it's recompiled with the new optimization
;;; policy.
(asdf:oos 'asdf:load-op :cl-ppcre-test :force t)

;;; Run the test suite.
(cl-ppcre-test:test)
~~~

Produce a coverage report, set the output directory:

~~~lisp
(sb-cover:report "/tmp/report/")
~~~

Finally, turn off instrumentation:

~~~lisp
(declaim (optimize (sb-cover:store-coverage-data 0)))
~~~

This produces something like the capture above or
[this code coverage of cl-ppcre](https://www.snellman.net/sbcl/cover/cl-ppcre-report-3/cover-index.html).


### Continuous testing and test coverage with Travis CI and Coveralls

[Travis](https://travis-ci.org/) is a service for running unit tests
in the cloud and [Coveralls](https://coveralls.io/) shows you the
evolution of coverage over time, and also tells you what a pull
request will do to coverage.

Thanks to `cl-travis` we can easily test our program against one or many
Lisp implementations (ABCL, Allegro CL, SBCL, CMUCL, CCL and
ECL). `cl-coveralls` helps to post our coverage to the service. It
supports SBCL and Clozure CL with Travis CI and Circle CI.

We refer you to the lengthy and illustrated explanations of the
["continuous integration" page on lisp-lang.org](http://lisp-lang.org/learn/continuous-integration).

You'll find many example projects using them in the links above, but
if you want a quick overview of what it looks like:

- Lucerne on [Coveralls](https://coveralls.io/github/eudoxia0/lucerne)
  and [Travis](https://travis-ci.org/eudoxia0/lucerne).
