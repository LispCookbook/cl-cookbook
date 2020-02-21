---
title: Error and exception handling
---

Common Lisp has mechanisms for error and condition handling as found
in other languages, and can do more.

What is a condition ?

> Just like in languages that support exception handling (Java, C++,
> Python, etc.), a condition represents, for the most part, an
> “exceptional” situation. However, even more so that those languages,
> *a condition in Common Lisp can represent a general situation where
> some branching in program logic needs to take place*, not
> necessarily due to some error condition. Due to the highly
> interactive nature of Lisp development (the Lisp image in
> conjunction with the REPL), this makes perfect sense in a language
> like Lisp rather than say, a language like Java or even Python,
> which has a very primitive REPL. In most cases, however, we may not
> need (or even allow) the interactivity that this system offers
> us. Thankfully, the same system works just as well even in
> non-interactive mode.
>
> [z0ltan](https://z0ltan.wordpress.com/2016/08/06/conditions-and-restarts-in-common-lisp/)


Let's dive into it step by step. More resources are given afterwards.


## Ignore all errors (and return nil)

Sometimes you know that a function can fail and you just want to
ignore it: use `ignore-errors`:

~~~lisp
(ignore-errors
  (/ 3 0))
; in: IGNORE-ERRORS (/ 3 0)
;     (/ 3 0)
;
; caught STYLE-WARNING:
;   Lisp error during constant folding:
;   arithmetic error DIVISION-BY-ZERO signalled
;   Operation was (/ 3 0).
;
; compilation unit finished
;   caught 1 STYLE-WARNING condition
NIL
#<DIVISION-BY-ZERO {1008FF5F13}>
~~~

We get a welcome `division-by-zero` warning but the code runs well and
it returns two things: `nil` and the condition that was signaled. We
could not choose what to return.

Remember that we can `inspect` the condition with a right click in Slime.


## Catching any condition - handler-case

<!-- we will say "handling" for handler-bind -->

`ignore-error` is built from `handler-case`. We can write the previous
example by catching the general `error` but now we can return whatever
we want:

~~~lisp
(handler-case (/ 3 0)
  (error (c)
    (format t "We caught a condition.~&")
    (values 0 c)))
; in: HANDLER-CASE (/ 3 0)
;     (/ 3 0)
;
; caught STYLE-WARNING:
;   Lisp error during constant folding:
;   Condition DIVISION-BY-ZERO was signalled.
;
; compilation unit finished
;   caught 1 STYLE-WARNING condition
We caught a condition.
0
#<DIVISION-BY-ZERO {1004846AE3}>
~~~

We also returned two values, 0 and the signaled condition.

The general form of `handler-case` is

~~~lisp
(handler-case (code that errors out)
   (condition-type (the-condition) ;; <-- optional argument
      (code))
   (another-condition (the-condition)
       ...))
~~~

We can also catch all conditions by matching `t`, like in a `cond`:

~~~lisp
(handler-case
    (progn
      (format t "This won't work…~%")
      (/ 3 0))
  (t (c)
    (format t "Got an exception: ~a~%" c)
    (values 0 c)))
;; …
;; This won't work…
;; Got an exception: arithmetic error DIVISION-BY-ZERO signalled
;; Operation was (/ 3 0).
;; 0
;; #<DIVISION-BY-ZERO {100608F0F3}>
~~~


## Catching a specific condition

We can specify what condition to handle:

~~~lisp
(handler-case (/ 3 0)
  (division-by-zero (c)
    (format t "Caught division by zero: ~a~%" c)))
;; …
;; Caught division by zero: arithmetic error DIVISION-BY-ZERO signalled
;; Operation was (/ 3 0).
;; NIL
~~~

This workflow is similar to a try/catch as found in other languages, but we can do more.


### Ignoring the condition argument

If you don't access the condition object in your handlers, but you still keep it has an argument for good practice, you'll see this compiler warning often:

```
; caught STYLE-WARNING:
;   The variable C is defined but never used.
```

To remove it, use a `declare` call as in:

~~~lisp
(handler-case (/ 3 0)
  (division-by-zero (c)
   (declare (ignore c))
   (format t "Caught division by zero~%"))) ;; we don't print "c" here and don't get the warning.
~~~

## handler-case VS handler-bind

`handler-case` is similar to the `try/catch` forms that we find in
other languages.

`handler-bind` (see the next examples), is what to use
when we need absolute control over what happens when a signal is
raised. It allows us to use the debugger and restarts, either
interactively or programmatically.

If some library doesn't catch all conditions and lets some bubble out
to us, we can see the restarts (established by `restart-case`)
anywhere deep in the stack, including restarts established by other
libraries whose this library called.  And *we can see the stack
trace*, with every frame that was called and, in some lisps, even see
local variables and such. Once we `handler-case`, we "forget" about
this, everything is unwound. `handler-bind` does *not* rewind the
stack.


## Handling conditions - handler-bind

Here we use `handler-bind`.

Its general form is:

~~~lisp
(handler-bind ((a-condition #'function-to-handle-it)
               (another-one #'another-function))
    (code that can...)
    (...error out))
~~~

So, our simple example:

~~~lisp
(handler-bind
             ((division-by-zero #'(lambda (c) (format t "hello condition~&"))))
           (/ 3 0))
~~~

This prints some warnings, then it prints our "hello" *and still
enters the debugger*. If we don't want to enter the debugger, we have
to define a restart and invoke it.


A real example with the
[`unix-opts`](https://github.com/mrkkrp/unix-opts) library, that
parses command line arguments. It defined some conditions:
`unknown-option`, `missing-arg` and `arg-parser-failed`, and it is up
to use to write what to do in these cases.

~~~lisp
(handler-bind ((opts:unknown-option #'unknown-option)
               (opts:missing-arg #'missing-arg)
               (opts:arg-parser-failed #'arg-parser-failed))
  (opts:get-opts))
~~~

Our `unknown-option` function is simple and looks like this:

~~~lisp
(defun unknown-option (condition)
  (format t "~s option is unknown.~%" (opts:option condition))
  (opts:describe)
  (exit)) ;; <-- we return to the command line, no debugger.
~~~

it takes the condition as parameter, so we can read information from
it if needed. Here we get the name of the erroneous option with the
defined reader `(opts:option condition)` (see below).


## Defining and making conditions

We define conditions with `define-condition` and we make (initialize) them with `make-condition`.

~~~lisp
(define-condition my-division-by-zero (error)
  ())

(make-condition 'my-division-by-zero)
;; #<MY-DIVISION-BY-ZERO {1005A5FE43}>
~~~


It's better if we give more information when we create this condition, so let's use slots:

~~~lisp
(define-condition my-division-by-zero (error)
  ((dividend :initarg :dividend
             :initform nil
             :reader dividend)) ;; <-- we'll get the dividend with (dividend condition). See the CLOS tutorial if needed.
  (:documentation "Custom error when we encounter a division by zero.")) ;; good practice ;)
~~~

Now when we'll "signal" or "throw" the condition in our code we'll be
able to populate it with information to be consumed later:

~~~lisp
(make-condition 'my-division-by-zero :dividend 3)
;; #<MY-DIVISION-BY-ZERO {1005C18653}>
~~~

Note: here's a quick reminder on classes, if you are not fully operational
on the CLOS object system:

~~~lisp
(make-condition 'my-division-by-zero :dividend 3)
;;                                   ^^ this is the ":initarg"
~~~

and `:reader dividend` created a *generic function* that is a "getter"
for the dividend of a `my-division-by-zero` object:

~~~lisp
(make-condition 'my-division-by-zero :dividend 3)
;; #<MY-DIVISION-BY-ZERO {1005C18653}>
(dividend *)
;; 3
~~~

an ":accessor" would be both a getter and a setter.


So, the general form of `define-condition` looks and feels like a
regular class definition, but despite the similarities, conditions are
not standard objects.

A difference is that we can't use `slot-value` on slots.


## Signaling (throwing) conditions: error, warn, signal

We can use `error` in two ways:

- `(error "some text")`: signals a condition of type `simple-error`, and opens-up the interactive debugger.
- `(error 'my-error :message "We did this and that and it didn't work.")`: creates and throws a custom condition with its slot "message".

So we can do:

~~~lisp
(error 'my-division-by-zero :dividend 3)
;; which is a shortcut for
(error (make-condition 'my-division-by-zero :dividend 3))
~~~

Throwing these conditions will enter the interactive debugger, where
the user may select a restart.

`warn` will too.

Use `signal` if you do not want to enter the debugger, but you still want to signal to the upper levels that something *exceptional* happened.


### Custom error messages: report


So far, when throwing our error, we saw this default text in the
debugger:

```
Condition COMMON-LISP-USER::MY-DIVISION-BY-ZERO was signalled.
   [Condition of type MY-DIVISION-BY-ZERO]
```

We can do better by giving a `:report` function in our condition declaration:

~~~lisp
(define-condition my-division-by-zero (error)
  ((dividend :initarg :dividend
             :initform nil
             :accessor dividend))
  ;; the :report is the message into the debugger:
  (:report (lambda (condition stream)
     (format stream "You were going to divide ~a by zero.~&" (dividend condition)))))
~~~

Now:

~~~lisp
(error 'my-division-by-zero :dividend 3)
;; Debugger:
;;
;; You were going to divide 3 by zero.
;;    [Condition of type MY-DIVISION-BY-ZERO]
~~~

As usual, once in the debugger, we can inspect the backtrace, go to
the erroneous source line (with `v` in Slime), etc.


~~~lisp
(define-condition troublesome-option (simple-error)
  ((option
    :initarg :option
    :reader option))
  (:report (lambda (c s) (format s "troublesome option: ~s" (option c))))
  (:documentation "Generalization over conditions that have to do with some
particular option."))

(define-condition unknown-option (troublesome-option)
  ()
  (:report (lambda (c s) (format s "unknown option: ~s" (option c))))
  (:documentation "This condition is thrown when parser encounters
unknown (not previously defined with `define-opts`) option."))
~~~


## Signaling (throwing) conditions

We can use `error`, like we did above, in two ways:

- `(error "some text")`: signals a condition of type `simple-error`
- `(error 'my-error :message "We did this and this and it didn't work.")`

Throwing these conditions will enter the interactive debugger,
where the user may select a restart. Use `signal` if you do not want to enter the debugger if your program failed to handle the condition.

Simple example from `unix-opts`: it adds information into the `option` slot:

~~~lisp
(error 'unknown-option
        :option opt)
~~~


## Restarts, interactive choices in the debugger

### Defining restarts

Restarts are the choices we get in the debugger, which always has the
`RETRY` and `ABORT` ones. We can add choices to the top of the list:

~~~lisp
(defun division-restarter ()
  (restart-case (/ 3 0)
    (return-zero () 0)
    (divide-by-one () (/ 3 1))))
~~~

By calling this stupid function we get two new choices at the top of the debugger:

![](simple-restarts.png)

Note: read in lisper.in's blogpost on csv parsing (see Resources) how
this system was used effectively in production.

But that's not all, by handling restarts we can start over the
operation as if the error didn't occur (as seen in the stack).

### Calling restarts programmatically

With `invoke-restart`.

~~~lisp
(defun division-and-bind ()
           (handler-bind
               ((error (lambda (c)
                         (format t "Got error: ~a~%" c) ;; error-message
                         (format t "and will divide by 1~&")
                         (invoke-restart 'divide-by-one))))
             (division-restarter)))
;; (DIVISION-AND-BIND)
;; Got error: arithmetic error DIVISION-BY-ZERO signalled
;; and will divide by 1
;; Operation was (/ 3 0).
;; 3
~~~

Note that we called the form that contains our restarts
(`division-restarter`) and not the function that throws the error.

### Using other restarts

`find-restart 'name-of-restart` will return the most recent bound
restart with the given name, or `nil`. We can invoke it with
`invoke-restart`.

### Prompting the user to enter a new value

Let's add a restart in our `division-restarter` to offer the user to
enter a new dividend, and run the division again.

~~~lisp
(defun division-restarter ()
  (restart-case (/ 3 0)
    (return-nil () nil)
    (divide-by-one () (/ 3 1))
    (choose-another-dividend (new-dividend)
      :report "Please choose another dividend"
      :interactive (lambda ()
                     (format t "Enter a new dividend: ")
                     (list (read))) ;; <-- must return a list.
      (format t "New division: 3/~a = ~a~&" new-dividend (/ 3 new-dividend)))))
~~~

We get prompted in the debugger:

```
arithmetic error DIVISION-BY-ZERO signalled
Operation was (/ 3 0).
  [Condition of type DIVISION-BY-ZERO]

Restarts:
 0: [RETURN-NIL] RETURN-NIL
 1: [DIVIDE-BY-ONE] DIVIDE-BY-ONE
 2: [CHOOSE-ANOTHER-DIVIDEND] Please choose another dividend <-- new
 3: [RETRY] Retry SLIME REPL evaluation request.
 4: [*ABORT] Return to SLIME's top level.
 5: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1002A47FA3}>)
```

The new `choose-another-dividend` restart takes an argument for the
new dividend, that will be fed by the `:interactive` lambda, which
`read`s for user input and must return a list.

We use it like this:

~~~lisp
(division-restarter)
;;
;; Entered debugger, chose the 2nd restart.
;;
Enter a new dividend: 10  <-- got prompted to enter a new value.
New division: 3/10 = 3/10
NIL
~~~

In a real situation we might want to call our "restarter" recursively,
to get into the debugger again if we enter a bad value.

### Hide and show restarts

Restarts can be hidden. In `restart-case`, in addition to `:report`
and `:interactive`, they also accept a `:test` key:

~~~lisp
(restart-case
   (return-zero ()
     :test (lambda ()
             (some-test))
    ...
~~~

## Run some code, condition or not ("finally")

The "finally" part of others `try/catch/finally` forms is done with `unwind-protect`.

It is the construct used in "with-" macros, like `with-open-file`,
which always closes the file after it.


You're now more than ready to write some code and to dive into other resources !


## Resources

* [Practical Common Lisp: "Beyond Exception Handling: Conditions and Restarts"](http://gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html) - the go-to tutorial, more explanations and primitives.
* Common Lisp Recipes, chap. 12, by E. Weitz
* [language reference](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node317.html)
* [lisper.in](https://lisper.in/restarts#signaling-validation-errors) - example with parsing a csv file and using restarts with success, [in a flight travel company](https://www.reddit.com/r/lisp/comments/7k85sf/a_tutorial_on_conditions_and_restarts/drceozm/).
* [Condition Handling in the Lisp family of languages](http://www.nhplace.com/kent/Papers/Condition-Handling-2001.html)
* [z0ltan.wordpress.com](https://z0ltan.wordpress.com/2016/08/06/conditions-and-restarts-in-common-lisp/)
* [https://github.com/svetlyak40wt/python-cl-conditions](https://github.com/svetlyak40wt/python-cl-conditions) - implementation of the CL conditions system in Python.

## See also

* [Algebraic effects - You can touch this !](http://jacek.zlydach.pl/blog/2019-07-24-algebraic-effects-you-can-touch-this.html) - how to use conditions and restarts to implement progress reporting and aborting of a long-running calculation, possibly in an interactive or GUI context.
* [A tutorial on conditions and restarts](https://github.com/stylewarning/lisp-random/blob/master/talks/4may19/root.lisp),  based around computing the roots of a real function. It was presented by the author at a Bay Area Julia meetup on may 2019 ([talk slides here](https://github.com/stylewarning/talks/blob/master/4may19-julia-meetup/Bay%20Area%20Julia%20Users%20Meetup%20-%204%20May%202019.pdf)).
