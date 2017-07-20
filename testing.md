---
title: Testing
---

So you want to easily test the code you're writing? The following
recipes cover how to write automated tests. We will use an established
and well-designed regression testing framework called
[Prove](https://github.com/fukamachi/prove). It is not the only
possibility though, [FiveAM](http://quickdocs.org/fiveam/api) is a
popular one. We prefer `Prove` for its doc and its **extensible
reporters** (it has different report styles and we can extend
them).


<a name="install"></a>

## Load

`Prove` is in Quicklisp:

~~~lisp
(ql:quickload :prove)
~~~

This command installs `prove` if necessary, and loads it.

<a name="writetest"></a>

## Write a test file

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


## Run a test file

~~~lisp
(prove:run #P"myapp/tests/my-test.lisp")
(prove:run #P"myapp/tests/my-test.lisp" :reporter :list)
~~~

We get an output like:

<img src="assets/prove-report.png"
     style="max-width: 800px"/>


## More

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
