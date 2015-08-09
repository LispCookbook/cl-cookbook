---
title: Testing
---

So you want to easily test the code your writing? The following recipes cover how to write automated tests. We will use an established and well-designed regression testing framework called 'RT'. In the last recipe we will discuss other options.


<a name="instllrt"></a>

## Install RT

You can learn more about Dr. Richard C. Waters' [RT on CLiki](http://www.cliki.net/RT). We will get up and running quickly, by doing a standard asdf-install which will pull down Kevin Rosenburg's packaged version. **Note: sbcl users can simply use sb-rt which is prebundled with their system**.

~~~lisp
;; Install rt from cliki
(asdf-install:install :rt)
~~~

This command downloads and installs the RT package. This step only needs to be done once. If you just did the install then you can skip the next step.

~~~lisp
(asdf:operate 'asdf:load-op :rt)
~~~

This line loads the RT package into your Lisp image.


<a name="writetest"></a>

## Write a Test

Normally we would be writing tests against our own code, but for clarity let's write some tests against operations from the [String recipes](strings.html#substrings) sections.

~~~lisp
; using *my-string* from the substrings recipe "Groucho Marx"
(rt:deftest test-subseq-one-index (subseq *my-string* 8) "Marx")
TEST-SUBSEQ-ONE-INDEX
~~~

That substring recipe told us that we could expect "Marx" if we ask for substring to 8. We use the deftest macro, naming our test `test-subseq-one-index`. In the body of the test we specify the code we want to test, followed by the expected results. The code `(subseq *my-string* 8)`is evaluated and the number of values return must match the number of expected results. In this example their is only one value returned and it matches the one expected value `"Marx"`. For each actual, expected pair, they must be **`equal`**. Note that the REPL prints the test name and under the covers this test is added to the current test suite.


<a name="runtest"></a>

## Run a Test

We have defined a test, let's run it.

~~~lisp
CL-USER> (rt:do-test 'test-subseq-one-index)
TEST-SUBSEQ-ONE-INDEX
~~~

We have RT test our assertion for how `subseq` should behave. The test passes, so no errors are reported. `do-test` simply returns the name of the test. In the next example we will see how RT behaves with failing tests.

Tests can be run one at a time with `do-test` as you incrementally build up your system, or they can be run all at once with `do-tests`. For our second test, let's define and run a failing test.

~~~lisp
CL-USER> (rt:deftest test-subseq-two-indicies (subseq *my-string* 0 7) "Xroucho")
TEST-SUBSEQ-TWO-INDICIES
CL-USER> (rt:do-test 'test-subseq-two-indicies)
Test TEST-SUBSEQ-TWO-INDICIES failed
Form: (SUBSEQ *MY-STRING* 0 7)
Expected value: "Xroucho"
Actual value: "Groucho".
NIL
~~~

We purposely provide an expected value which is wrong, so that the test will fail. Notice RT describes a failed test to standard output. Also the `do-test` function returns `NIL` instead of the test as before.

The great value of test code is running it automatically and frequently to find issues early in development. RT provides the `do-tests` function to regress all of the tests.

~~~lisp
CL-USER> (rt:do-tests)
Doing 2 pending tests of 2 tests total.
 TEST-SUBSEQ-ONE-INDEX
Test TEST-SUBSEQ-TWO-INDICIES failed
Form: (SUBSEQ *MY-STRING* 0 7)
Expected value: "Xroucho"
Actual value: "Groucho".
1 out of 2 total tests failed: TEST-SUBSEQ-TWO-INDICIES.
NIL
~~~

Let's redefine `test-subseq-two-indicies` with a passing expected value. Additionally let's define a third test.

~~~lisp
CL-USER> (rt:deftest test-subseq-two-indicies (subseq *my-string* 0 7) "Groucho")
WARNING: Redefining test TEST-SUBSEQ-TWO-INDICIES
TEST-SUBSEQ-TWO-INDICIES
(rt:deftest test-concatenate-three-strings
    (concatenate 'string "Karl" " " "Marx") "Karl Marx")
TEST-CONCATENATE-THREE-STRINGS
CL-USER> (rt:do-tests)
Doing 3 pending tests of 3 tests total.
 TEST-SUBSEQ-ONE-INDEX TEST-SUBSEQ-TWO-INDICIES
 TEST-CONCATENATE-THREE-STRINGS
No tests failed.
T
~~~

Note that `do-tests` picks up our third test `test-concatenate-three-strings` because it runs the entire test suite and prints a report to standard out that no tests have failed. The value `T` is returned to signal a successful test run.


<a name="rtend"></a>

## RT Odds and Ends

If we pretend for a moment that `subseq` and `concatenate` are Lisp operations which we have written, you can see how easy it is to build up a collection of tests to exercise and regress code as we write and maintain it.

If you like using RT so far, you will want to read up on the documentation which comes with it, or read the [original paper](http://www.merl.com/publications/TR1991-004/) from 1991 which covers RT and COVER (a code coverage framework). At a minimum you will want to add

~~~lisp
(rem-all-tests)
~~~

to the beginning of your test code for the current project. You can use `continue-testing` to run all of the tests which have either failed, or are newly defined.


<a name="othertestframeworks"></a>

## Other Test Frameworks

Test Driven Development, xUnit testing frameworks, Automated Acceptance Tests, etc are quite the rage in some areas of contemporary programming. If RT doesn't fit your needs, or you would like to see what else is out there, please see the following:

* [CLiki Test Frameworks](http://www.cliki.net/test%20framework)
* [ALU Wiki Test Frameworks](http://wiki.alu.org/Test_Frameworks)
* [Unit Testing](http://www.cl-user.net/asp/tags/unit-testing) on The Common Lisp Directory

and one of many which is a good choice to look at is:

* [FiveAM](http://common-lisp.net/project/bese/FiveAM.html) - Contemporary and very Lisp
