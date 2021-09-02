---
title: Testing
---

So you want to easily test the code you're writing? The following
recipes cover how to write automated tests and see their code
coverage. We also give pointers to plug those in modern continuous
integration services like Travis CI and Coveralls.

[Prove](https://github.com/fukamachi/prove) used to be a widely liked testing framework but its repository was later archived. Its successor [Rove](https://github.com/fukamachi/rove) is not stable enough, so we will be using a mature testing framework called [FiveAM](https://github.com/lispci/fiveam). There are also some [other testing frameworks](https://github.com/CodyReichert/awesome-cl#unit-testing) to explore.

FiveAM does not have many documentation resources. However, it has really good docstrings built-in. Most of the time, they would provide sufficient information that answers your questions. 

## Testing with FiveAM

FiveAM has 3 levels of abstraction: check, test and suite. As you may have guessed:
1. A check is a single assertion that asserts a boolean value. 
2. A test is the smallest runnable unit. A test case that may contain multiple checks. Any check failure leads to failure of the whole test.
3. A suite is a collection of tests. When a suite is run, all tests inside would be performed. Suite allows paternity, which means that running a suite will run all the tests defined in it and in its children suites.

A simplest code sample containing 3 basic blocks mentioned above can shown as below:

~~~lisp
(def-suite* my-suite)

(test my-test
  (is (= 2 (+ 1 1))))
~~~

It is totally up to the user to decide the hierarchy of tests and suites. Here we mainly focus on the usage of FiveAM.

Suppose we have built a rather complex system and the following functions is part of it:

~~~lisp
(define-condition file-not-existing-error (error)
  ((filename :type string :initarg :filename :reader filename)))

(defun read-file-as-string (filename &key (error-if-not-exists t))
  "Read file content as string. FILENAME specifies the path of file. 
  
Keyword ERROR-IF-NOT-EXISTS specifies the operation to perform when the file 
is not found. T (by default) means an error will be signaled. When given NIL, 
the function will return NIL in that case."
  (cond
    ((uiop:file-exists-p filename)
     (uiop:read-file-string filename))
    (error-if-not-exists
     (error 'file-not-existing-error :filename filename))
    (t nil)))
~~~

### Install and load

`FiveAM` is in Quicklisp and can be loaded with the following command:

~~~lisp
(ql:quickload "fiveam")
~~~

The package is named `fiveam` with a nickname `5am`. For the sake of simplicity, we will ignore the package prefix in the following code samples.

### Defining suites

Testing in FiveAM usually starts by defining a suite. A suite helps separating tests to smaller collections that makes them more organized. It is highly recommended to define a single *root* suite for the sake of ASDF integration. We will talk about it later, now let's focus on the testing itself.

The code below defines a suite named `my-system`. We will use it as the root suite for the whole system.

~~~lisp
(def-suite my-system)
~~~

Then let's define another suite for testing `read-file-as-string` function.

~~~lisp
;; Define a suite and set it as the default for the following tests.
(def-suite read-file-as-string :in my-system)
(in-suite read-file-as-string)

;; Alternatively, the following line is a combination of the 2 lines above.
(def-suite* read-file-as-string :in my-system)
~~~

Here a new suite named `read-file-as-string` has been defined. It is declared to be a child suite of `my-system` as specified by the `:in` keyword. The macro `in-suite` sets it as the default suite for test defined later.

### Defining tests

Before diving into tests, here is a brief introduction of checks you may use inside tests:

* The macro `is` might be the mostly used check. It simply checks if given expression returns a true value and generates a `test-passed` or `test-failure` result accordingly.
* The macro `skip` takes a reason and generates a `test-skipped` result.
* The macro `signals` checks if given condition is signaled during execution.

Please note that all the checks accepts an optional reason that can be formatted. When omitted, FiveAM generates a report according to arguments passed to the function. You may read the `check.lisp` file for more helpers.

The macro `test` provides a simple way to define a test with given name:

~~~lisp
(test read-file-as-string-non-existing-file
  ;; IS accepts a boolean expression with optional reason.
  (let ((result (read-file-as-string "/tmp/non-existing-file.txt"
                                     :error-if-not-exists nil)))
    (is (null result)
      "Should return NIL when :ERROR-IF-NOT-EXISTS is set to NIL"))
  ;; SIGNALS accepts unquoted name of condition and body to evaluate.
  ;; Here it checks if FILE-NOT-EXISTING-ERROR is signaled.
  (signals file-not-existing-error
    (read-file-as-string "/tmp/non-existing-file.txt"
                         :error-if-not-exists t)))

(test read-file-as-string-empty-file 
  (let ((result (read-file-as-string "/tmp/empty.txt")))
    ;; The reason can be omitted.
    (is (not (null result)))
    ;; The reason can be used to provide formatted text.
    (is (= 0 (length result)))
    "Empty string expected but got \"~a\""))


(test read-file-as-string-normal-file
  (let ((result (read-file-as-string "/tmp/hello.txt")))
    ;; Convention: put expected value as the first arg of =, or equal, string= etc.
    ;; FiveAM generates a more readable report following this convention.
    (is (string= "hello" result))))
~~~

In the above code, 3 test was defined with 5 checks in total. Some checks are actually redundant for the sake of demonstration. You may put all the checks in one big test, or in multiple scenarios. It is up to you.

The macro `test` is a convenience for `def-test` to define simple tests. You may read its docstring for a more complete introduction, but please note that it is not recommended to use its fixture feature according to [this issue](https://github.com/lispci/fiveam/issues/31).

### Running tests

FiveAm provides multiple ways to run tests. The macro `run!` is a good start point during development. It accepts a name of suite or test and run it, then prints testing report in standard output. Let's run the tests now!

~~~lisp
(run! 'my-system)
; Running test suite MY-SYSTEM
;  Running test READ-FILE-AS-STRING-EMPTY-FILE ..
;  Running test READ-FILE-AS-STRING-NON-EXISTING-FILE ..
;  Running test READ-FILE-AS-STRING-NORMAL-FILE .
;  Did 5 checks.
;     Pass: 5 (100%)
;     Skip: 0 ( 0%)
;     Fail: 0 ( 0%)
;  => T, NIL, NIL
~~~

If we mess `read-file-as-string-non-existing-file` up by replacing `/tmp/non-existing-file.txt` with `/tmp/hello.txt`, the test would fail (sure!) as expected:

~~~lisp
(run! 'read-file-as-string-non-existing-file)
; Running test READ-FILE-AS-STRING-NON-EXISTING-FILE ff
;  Did 2 checks.
;     Pass: 0 ( 0%)
;     Skip: 0 ( 0%)
;     Fail: 2 (100%)
;  Failure Details:
;  --------------------------------
;  READ-FILE-AS-STRING-NON-EXISTING-FILE []: 
;       Should return NIL when :ERROR-IF-NOT-EXISTS is set to NIL.
;  --------------------------------
;  --------------------------------
;  READ-FILE-AS-STRING-NON-EXISTING-FILE []: 
;       Failed to signal a FILE-NOT-EXISTING-ERROR.
;  --------------------------------
;  => NIL
; (#<IT.BESE.FIVEAM::TEST-FAILURE {10064485F3}>
;  #<IT.BESE.FIVEAM::TEST-FAILURE {1006438663}>)
; NIL
~~~

The behavior of suite/test runner can be customized by the `*on-failure*` variable, which controls what to do when check failure happens. It can be set to one of the following values:
- `:debug` to drop to debugger.
- `:backtrace` to print a backtrace.
- `NIl` (default) to simply continue.

### (Optional) Running tests using Slite

[Slite](https://github.com/tdrhq/slite) stands for SLIme TEst runner. It lets you run FiveAM tests through Emacs. After install it and press keystroke `C-c v` and input `(slite:run-all-fiveam-tests)` for running all tests, or `(fiveam:run test-suite-name)` for running a specific suite/test.

It consists of an ASDF system and a Emacs package. As for September 2021, neither can be installed via Quicklisp or MELPA.

Please refer to its [repo page](https://github.com/tdrhq/slite) for instructions.

### ASDF integration

So it would be nice to provide a one-line trigger to test our `my-system` system. Recall that we said it is better to provide a root suite? Here is the reason:

~~~lisp
(defsystem my-system
  ;; Parts omitted.
  :in-order-to ((test-op (test-op :my-system/test))))

(defsystem mitogrator/test
  ;; Parts omitted.
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :my-system :my-system/test))))
~~~

The last line tells ASDF to load symbol `:my-system` from `my-system/test` package and call `fiveam:run!`. It fact, it is equivalent to `(run! 'my-system)` as mentioned above.

### Testing report customization

It is possible to generate our own testing report. The macro `run!` is nothing more than a composition of `explain!` and `run`. 

Instead of generating a testing report like its cousin `run!`, the function `run` runs suite or test passed in and returns a list of `test-result` instance, usually instances of `test-failure` or `test-passed` sub-classes.

A class `text-explainer` is defined as a basic class for testing report generator. A generic function `explain` is defined to take a `text-plainer` instance and a `test-result` instance (returned by `run`) and generate testing report. The following 2 code snippets are equivalent:

~~~lisp
(run! 'read-file-as-string-non-existing-file)

(explain (make-instance '5am::detailed-text-explainer)
         (run 'read-file-as-string-non-existing-file))
~~~

By creating a new sub-class of `text-explainer` and a method `explain` for it, it is possible to define a new test reporting system.

The following code just provides a proof-of-concept implementation. You may need to read the source code of `5am::detailed-text-explainer` to fully understand it.

~~~lisp
(defclass my-explainer (5am::text-explainer)
  ())

(defmethod 5am:explain ((explainer my-explainer) results &optional (stream *standard-output*) recursive-deps)
  (loop for result in results
        do (case (type-of result)
             ('5am::test-passed
              (format stream "~%Test ~a passed" (5am::name (5am::test-case result))))
             ('5am::test-failure
              (format stream "~%Test ~a failed" (5am::name (5am::test-case result)))))))

(explain (make-instace 'my-explainer)
         (run 'read-file-as-string-non-existing-file))
; Test READ-FILE-AS-STRING-NON-EXISTING-FILE failed
; Test READ-FILE-AS-STRING-NON-EXISTING-FILE passed => NIL
~~~

## Mocking with Mockingbird

TODO

## Testing with Prove

> Warning: Prove is obsolete. This section is kept but it is not encouraged to use it.

### Install and load

`Prove` is in Quicklisp:

~~~lisp
(ql:quickload "prove")
~~~

This command installs `prove` if necessary, and loads it.

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

### Run one test

You can directly run one test by compiling it. With Slime, use the
usual `C-c C-c`.


### More about Prove

`Prove` can also:

* be run on **Travis CI**,
* **colorize** the output,
* report **tests duration**,
* change the default test function,
* set a threshold for slow tests,
* invoke the **CL debugger** whenever getting an error during running tests,
* integrate with **ASDF** so than we can execute `(asdf:test-system)` or
  `(prove:run)` in the REPL (such configuration is provided by
  [cl-project](https://github.com/fukamachi/cl-project), by the same
  author).

See [Prove's documentation](https://github.com/fukamachi/prove)!


## Interactively fixing unit tests

Common Lisp is interactive by nature (or so are most implementations),
and testing frameworks make use of it. It is possible to ask the
framework to open the debugger on a failing test, so that we can
inspect the stack trace and go to the erroneous line instantly, fix it
and re-run the test from where it left off, by choosing the suggested
*restart*.

With Prove, set `prove:*debug-on-error*` to `t`.

<!-- epub-exclude-start -->

Below is a short screencast showing all this in action (with FiveAM):

<iframe width="560" height="315" src="https://www.youtube.com/embed/KsHxgP3SRTs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<!-- epub-exclude-end -->

Note that in the debugger:

- `<enter>` on a backtrace shows more of it
- `v` on a backtrace goes to the corresponding line or function.
- you can discover more options with the menu.


## Code coverage

A code coverage tool produces a visual output that allows to see what
parts of our code were tested or not:


![](assets/coverage.png "source: https://www.snellman.net/blog/archive/2007-05-03-code-coverage-tool-for-sbcl.html")

Such capabilities are included into Lisp implementations. For example, SBCL has the
[sb-cover](http://www.sbcl.org/manual/index.html#sb_002dcover) module
and the feature is also built-in in [CCL](https://ccl.clozure.com/docs/ccl.html#code-coverage)
or [LispWorks](http://www.lispworks.com/documentation/lw71/LW/html/lw-68.htm).

### Generating an html test coverage output

Let's do it with SBCL's [sb-cover](http://www.sbcl.org/manual/index.html#sb_002dcover).

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
(prove:run :yoursystem-test)
~~~

Produce a coverage report, set the output directory:

~~~lisp
(sb-cover:report "coverage/")
~~~

Finally, turn off instrumentation:

~~~lisp
(declaim (optimize (sb-cover:store-coverage-data 0)))
~~~

You can open your browser at
`../yourproject/t/coverage/cover-index.html` to see the report like
the capture above or like
[this code coverage of cl-ppcre](https://www.snellman.net/sbcl/cover/cl-ppcre-report-3/cover-index.html).


## Continuous Integration

Continuous Integration is important to run automatic tests after a
commit or before a pull request, to run code quality checks, to build
and distribute your software… well, to automate everything about software.

We want our programs to be portable across Lisp implementations, so
we'll set up our CI pipeline to run our tests against several of them (it
could be SBCL and CCL of course, but while we're at it ABCL, ECL and
possibly more).

We have a choice of Continuous Integration services: Travis CI, Circle, Gitlab CI, now also GitHub Actions, etc (many existed before GitHub Actions, if you wonder). We'll have a look at how to configure a CI pipeline for Common Lisp, and we'll focus a little more on Gitlab CI on the last part.

We'll also quickly show how to publish coverage reports to the [Coveralls](https://coveralls.io/) service. [cl-coveralls](https://github.com/fukamachi/cl-coveralls) helps to post our coverage to the service.

### GitHub Actions, Circle CI, Travis… with CI-Utils

We'll use [CI-Utils](https://neil-lindquist.github.io/CI-Utils/), a set of utilities that comes with many examples. It also explains more precisely what is a CI system and compares a dozen of services.

It relies on [Roswell](https://github.com/roswell/roswell/) to install the Lisp implementations and to run the tests. They all are installed with a bash one-liner:

    curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | bash

(note that on the Gitlab CI example, we use a ready-to-use Docker image that contains them all)

It also ships with a test runner for FiveAM, which eases some rough parts (like returning the right error code to the terminal). We install ci-utils with Roswell, and we get the `run-fiveam` executable.

Then we can run our tests:

    run-fiveam -e t -l foo/test :foo-tests  # foo is our project

Following is the complete `.travis.yml` file.

The first part should be self-explanatory:

```yml
### Example configuration for Travis CI ###
language: generic

addons:
  homebrew:
    update: true
    packages:
    - roswell
  apt:
    packages:
      - libc6-i386 # needed for a couple implementations
      - default-jre # needed for abcl

# Runs each lisp implementation on each of the listed OS
os:
  - linux
#  - osx # OSX has a long setup on travis, so it's likely easier to just run select implementations on OSX
```

This is how we configure the implementations matrix, to run our tests on several Lisp implementations. We also send the test coverage made with SBCL to Coveralls.

```
env:
  global:
    - PATH=~/.roswell/bin:$PATH
    - ROSWELL_INSTALL_DIR=$HOME/.roswell
#    - COVERAGE_EXCLUDE=t  # for prove or rove
  jobs:
    # The implementation and whether coverage is send to coveralls are controlled with these environmental variables
    - LISP=sbcl-bin COVERALLS=true
    - LISP=ccl-bin
    - LISP=abcl
    - LISP=ecl   # warn: in our experience, compilations times can be long on ECL.

# Additional OS/Lisp combinations can be added to those generated above
jobs:
  include:
    - os: osx
      env: LISP=sbcl-bin
    - os: osx
      env: LISP=ccl-bin
```

Some jobs can be marked as allowed to fail:


```
# Note that this should only be used if there is no interest for the library to work on that system
#  allow_failures:
#    - env: LISP=abcl
#    - env: LISP=ecl
#    - env: LISP=cmucl
#    - env: LISP=alisp
#      os: osx

  fast_finish: true
```

We finally install Roswell, the implementations, and we run our tests.

```
cache:
  directories:
    - $HOME/.roswell
    - $HOME/.config/common-lisp

install:
  - curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | sh
  - ros install ci-utils #for run-fiveam
#  - ros install prove #for run-prove
#  - ros install rove #for [run-] rove

  # If asdf 3.16 or higher is needed, uncomment the following lines
  #- mkdir -p ~/common-lisp
  #- if [ "$LISP" == "ccl-bin" ]; then git clone https://gitlab.common-lisp.net/asdf/asdf.git ~/common-lisp; fi

script:
  - run-fiveam -e t -l foo/test :foo-tests
  #- run-prove foo.asd
  #- rove foo.asd
```

Below with Gitlab CI, we'll use a Docker image that already contains the Lisp binaries and every Debian package required to build Quicklisp libraries.


### Gitlab CI

[Gitlab CI](https://docs.gitlab.com/ce/ci/README.html) is part of
Gitlab and is available on [Gitlab.com](https://gitlab.com/), for
public and private repositories. Let's see straight away a simple
`.gitlab-ci.yml`:

~~~
variables:
  QUICKLISP_ADD_TO_INIT_FILE: "true"

image: clfoundation/sbcl:latest

before_script:
  - install-quicklisp
  - git clone https://github.com/foo/bar ~/quicklisp/local-projects/

test:
  script:
    - make test
~~~

Gitlab CI is based on Docker. With `image` we tell it to use the `latest` tag
of the [clfoundation/sbcl](https://hub.docker.com/r/clfoundation/sbcl/)
image. This includes the latest version of SBCL, many OS packages useful for CI
purposes, and a script to install Quicklisp. Gitlab will load the image, clone
our project and put us at the project root with administrative rights to run
the rest of the commands.

`test` is a "job" we define, `script` is a
recognized keywords that takes a list of commands to run.

Suppose we must install dependencies before running our tests: `before_script`
will run before each job. Here we install Quicklisp (adding it to SBCL's init
file), and clone a library where Quicklisp can find it.

We can try locally ourselves. If we already installed [Docker](https://docs.docker.com/) and
started its daemon (`sudo service docker start`), we can do:

    docker run --rm -it -v /path/to/local/code:/usr/local/share/common-lisp/source clfoundation/sbcl:latest bash

This will download the lisp image (±300MB compressed), mount some local code in
the image where indicated, and drop us in bash. Now we can try a `make test`.

Here is a more complete example that tests against several CL implementations
in parallel:

~~~
variables:
  IMAGE_TAG: latest
  QUICKLISP_ADD_TO_INIT_FILE: "true"
  QUICKLISP_DIST_VERSION: latest

image: clfoundation/$LISP:$IMAGE_TAG

stages:
  - test
  - build

before_script:
  - install-quicklisp
  - git clone https://github.com/foo/bar ~/quicklisp/local-projects/

.test:
  stage: test
  script:
    - make test

abcl test:
  extends: .test
  variables:
    LISP: abcl

ccl test:
  extends: .test
  variables:
    LISP: ccl

ecl test:
  extends: .test
  variables:
    LISP: ecl

sbcl test:
  extends: .test
  variables:
    LISP: sbcl

build:
  stage: build
  variables:
    LISP: sbcl
  only:
    - tags
  script:
    - make build
  artifacts:
    paths:
      - some-file-name
~~~

Here we defined two `stages` (see
[environments](https://docs.gitlab.com/ee/ci/environments/)),
"test" and "build", defined to run one after another. A "build" stage
will start only if the "test" one succeeds.

"build" is asked to run `only` when a
new tag is pushed, not at every commit. When it succeeds, it will make
the files listed in `artifacts`'s `paths` available for download. We can
download them from Gitlab's Pipelines UI, or with an url. This one will download
the file "some-file-name" from the latest "build" job:

    https://gitlab.com/username/project-name/-/jobs/artifacts/master/raw/some-file-name?job=build

When the pipelines pass, you will see:

![](assets/img-ci-build.png)


You now have a ready to use Gitlab CI.

# References

- the [CL Foundation Docker images](https://hub.docker.com/u/clfoundation)
- [Tutorial: Working with FiveAM](http://turtleware.eu/posts/Tutorial-Working-with-FiveAM.html)
