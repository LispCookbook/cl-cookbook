---
title: Functions
---

<a name="return"></a>

## Creating named functions: `defun`

Creating named functions is done with the `defun` keyword. It follows this model:

~~~lisp
(defun <name> (list of arguments)
  "docstring"
  (function body))
~~~

The return value is the value returned by the last expression of the body
(see below for more). There is no "return xx" statement.

So, for example:

~~~lisp
(defun hello-world ()
  ;;               ^^ no arguments
  (print "hello world!"))
~~~

Call it:

~~~lisp
(hello-world)
;; "hello world!"  <-- output
;; "hello world!"  <-- a string is returned.
~~~

## Arguments

### Base case: required arguments

Add in arguments like this:

~~~lisp
(defun hello (name)
  "Say hello to `name'."
  (format t "hello ~a !~&" name))
;; HELLO
~~~

(where `~a` is the most used `format` directive to print a variable
*aesthetically* and `~&` prints a newline)

Call the function:

~~~lisp
(hello "me")
;; hello me !  <-- this is printed by `format`
;; NIL         <-- return value: `format t` prints a string to standard output and returns nil.
~~~

If you don't specify the right amount of arguments, you'll be trapped
into the interactive debugger with an explicit error message:

   (hello)

> invalid number of arguments: 0

### Optional arguments: `&optional`

Optional arguments are declared after the `&optional` keyword in the
lambda list. They are ordered, they must appear one after another.

This function:

~~~lisp
(defun hello (name &optional age gender) …)
~~~

must be called like this:

~~~lisp
(hello "me") ;; a value for the required argument, zero optional arguments
(hello "me" "7")  ;; a value for age
(hello "me" 7 :h) ;; a value for age and gender
~~~

### Named parameters: `&key`

It is not always convenient to remember the order of the arguments. It
is thus possible to supply arguments by name: we declare them using
`&key <name>`, we set them with `:name <value>` in the function call,
and we use `name` as a regular variable in the function body. They are
`nil` by default.

~~~lisp
(defun hello (name &key happy)
  "If `happy' is `t', print a smiley"
  (format t "hello ~a " name)
  (when happy
    (format t ":)~&"))
~~~

The following calls are possible:

    (hello "me")
    (hello "me" :happy t)
    (hello "me" :happy nil) ;; useless, equivalent to (hello "me")

and this is not valid: `(hello "me" :happy)`:

> odd number of &KEY arguments

A similar example of a function declaration, with several key parameters:

~~~lisp
(defun hello (name &key happy lisper cookbook-contributor-p) …)
~~~

it can be called with zero or more key parameters, in any order:

~~~lisp
(hello "me" :lisper t)
(hello "me" :lisper t :happy t)
(hello "me" :cookbook-contributor-p t :happy t)
~~~

#### Mixing optional and key parameters

It is generally a style warning, but it is possible.

~~~lisp
(defun hello (&optional name &key happy)
  (format t "hello ~a " name)
  (when happy
    (format t ":)~&")))
~~~

In SBCL, this yields:

~~~lisp
; in: DEFUN HELLO
;     (SB-INT:NAMED-LAMBDA HELLO
;         (&OPTIONAL NAME &KEY HAPPY)
;       (BLOCK HELLO (FORMAT T "hello ~a " NAME) (WHEN HAPPY (FORMAT T ":)~&"))))
;
; caught STYLE-WARNING:
;   &OPTIONAL and &KEY found in the same lambda list: (&OPTIONAL (NAME "John") &KEY
;                                                      HAPPY)
;
; compilation unit finished
;   caught 1 STYLE-WARNING condition
~~~

We can call it:

~~~lisp
(hello "me" :happy t)
;; hello me :)
;; NIL
~~~

### Default values

In the lambda list, use pairs to give a default value to an optional or a key argument, like `(happy t)` below:

~~~lisp
(defun hello (name &key (happy t))
~~~

Now `happy` is true by default.

### Variable number of arguments: `&rest`

Sometimes you want a function to accept a variable number of
arguments. Use `&rest <variable>`, where `<variable>` will be a list.

~~~lisp
(defun mean (x &rest numbers)
    (/ (apply #'+ x numbers)
       (1+ (length numbers))))
~~~

~~~lisp
(mean 1)
(mean 1 2)
(mean 1 2 3 4 5)
~~~

### `&allow-other-keys`

Observe:

~~~lisp
(defun hello (name &key happy)
  (format t "hello ~a~&" name))

(hello "me" :lisper t)
;; => Error: unknown keyword argument
~~~

whereas

~~~lisp
(defun hello (name &key happy &allow-other-keys)
  (format t "hello ~a~&" name))

(hello "me" :lisper t)
;; hello me
~~~

We might need `&allow-other-keys` when passing around arguments or
with higher level manipulation of functions.


## Return values

The return value of the function is the value returned by the last
executed form of the body.

There are ways for non-local exits (`return-from <function name> <value>`), but they are usually not needed.

Common Lisp has also the concept of multiple return values.

### Multiple return values: `values` and `multiple-value-bind`

Returning multiple values is *not* like returning a tuple or a list of
results ;) This is a common misconception.

Multiple values are specially useful and powerful because a change in
them needs little to no refactoring.

~~~lisp
(defun foo (a b c)
  a)
~~~

This function returns `a`.

~~~lisp
(defvar *res* (foo :a :b :c))
;; :A
~~~

We use `values` to return multiple values:

~~~lisp
(defun foo (a b c)
  (values a b c))
~~~

~~~lisp
(defvar *res* (foo :a :b :c))
;; :A
~~~

Observe here that `*res*` *is still `:A`*.

All functions that use the return value of `foo` need no change, they
still work. If we had returned a list or an array, this would be
different.

We destructure multiple values with `multiple-value-bind` (or
`mvb`+TAB in Slime for short):

~~~lisp
(multiple-value-bind (res1 res2 res3)
    (foo :a :b :c)
  (format t "res1 is ~a, res2 is ~a, res2 is ~a~&" res1 res2 res3))
;; res1 is A, res2 is B, res2 is C
;; NIL
~~~

Its general form is

~~~lisp
(multiple-value-bind (var-1 .. var-n) expr
  body)
~~~

The variables `var-n` are not available outside the scope of `multiple-value-bind`.

Last but not least: note that `(values)` with no values returns… no values at all.

See also `multiple-value-call`.

## Lambdas

Anonymous functions are created with `lambda`:

~~~lisp
(lambda (x) (print x))
~~~

We can call a lambda with `funcall` or `apply` (see below).

If a lambda expression in the first element of an unquoted list, it is
called:

~~~lisp
((lambda (x) (print x)) "hello")
;; hello
~~~

## Calling functions programatically: `funcall` and `apply`

`funcall` is to be used with a known number of arguments, when `apply`
can be used on a list, for example from `&rest`:

~~~lisp
(funcall #'+ 1 2)
(apply #'+ '(1 2))
~~~

## Higher order functions: functions that return functions

"How do I write a function that returns a function?" is a typical question asked by people who have learned Scheme before they started with Common Lisp. In Scheme, they were accustomed to be able to do things like this:

~~~lisp
==> (define (adder n) (lambda (x) (+ x n)))
adder

==> ((adder 3) 5)
8

==> (define (doubler f) (lambda (x) (f x x)))
doubler

==> ((doubler +) 4)
8
~~~

This can of course be done in Common Lisp, but the syntax and the semantics are different. The first step, creating a function that returns a function, looks very similar apart from minor syntactical conventions, but what happens behind the scenes is different:

~~~lisp
CL-USER> (defun adder (n) (lambda (x) (+ x n)))
ADDER
~~~

Here we have defined the function `adder` which returns an _object_ of _type_ [`function`](http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm). To create such an object you'll have to use the special operator [`function`](http://www.lispworks.com/documentation/HyperSpec/Body/s_fn.htm) and apply it to a _lambda expression_. `(function` _form_`)` may be abbreviated as `#'`_form_. In our example above we used a shorthand notation provided by the macro [`lambda`](http://www.lispworks.com/documentation/HyperSpec/Body/m_lambda.htm). Without this little bit of syntactical sugar we would have to write it as

~~~lisp
CL-USER> (defun adder (n) #'(lambda (x) (+ x n)))
ADDER
~~~

or

~~~lisp
CL-USER> (defun adder (n) (function (lambda (x) (+ x n))))
ADDER
~~~

No matter how we write it, `adder` will now return a function whenever we call it. But we _can't_ use it in the same way we would use it in Scheme:

~~~lisp
CL-USER> (adder 3)
#<Interpreted Function "LAMBDA (N)" {485FFE81}>

CL-USER> ((adder 3) 5)
In: (ADDER 3) 5
    ((ADDER 3) 5)
Error: Illegal function call.
~~~

Here is why: CL has different _namespaces_ for functions and variables, i.e. the same _name_ can refer to different things depending on it's position in a form that's evaluated:

~~~lisp
CL-USER> (boundp 'foo)
NIL
CL-USER> (fboundp 'foo)
NIL
CL-USER> (defparameter foo 42)
FOO
* foo
42
CL-USER> (boundp 'foo)
T
CL-USER> (fboundp 'foo)
NIL
CL-USER> (defun foo (x) (* x x))
FOO
CL-USER> (fboundp 'foo)
T
* foo            ;;; ***
42
CL-USER> (foo 3)        ;;; +++
9
CL-USER> (foo foo)
1764
CL-USER> (function foo)
#<Interpreted Function FOO {48523CC1}>
* #'foo
#<Interpreted Function FOO {48523CC1}>
CL-USER> (let ((+ 3)) (+ + +))
6
~~~

To simplify a bit, you can think of each symbol in CL having (at least) two "cells" in which information is stored. One cell - sometimes referred to as its _value cell_ - can hold a value that is _bound_ to this symbol, and you can use [`boundp`](http://www.lispworks.com/documentation/HyperSpec/Body/f_boundp.htm) to test whether the symbol is bound to a value (in the global environment). You can access the value cell of a symbol with [`symbol-value`](http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_5.htm).


The other cell - sometimes referred to as its _function cell_ - can hold the definition of the symbol's (global) function binding. In this case, the symbol is said to be _fbound_ to this definition. You can use [`fboundp`](http://www.lispworks.com/documentation/HyperSpec/Body/f_fbound.htm) to test whether a symbol is fbound. You can access the function cell of a symbol (in the global environment) with [`symbol-function`](http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_1.htm).


Now, if a _symbol_ is evaluated, it is treated as a _variable_ in that it's value cell is returned - see the line marked with _***_ above. If a _compound form_, i.e. a _cons_, is evaluated and its _car_ is a symbol, then the function cell of this symbol is used - see the line marked _+++_ above.


In Common Lisp, as opposed to Scheme, it is _not_ possible that the car of the compound form to be evaluated is an arbitrary form. If it is not a symbol, it _must_ be a _lambda expression_, which looks like

`(lambda `_lambda-list_ _form*_`)`


This explains the error message we got above - `(adder 3)` is neither a symbol nor a lambda expression. But, you might ask, how _do_ we use the function object that is returned by `adder`? The answer is: Use [`funcall`](http://www.lispworks.com/documentation/HyperSpec/Body/f_funcal.htm) or [`apply`](http://www.lispworks.com/documentation/HyperSpec/Body/f_apply.htm):

~~~lisp
;;; continued from above
CL-USER> (funcall (adder 3) 5)
8
CL-USER> (apply (adder 3) '(5))
8
CL-USER> (defparameter *my-fun* (adder 3))
*MY-FUN*
* *my-fun*
#<Interpreted Function "LAMBDA (N)" {486468C9}>
CL-USER> (funcall *my-fun* 5)
8
CL-USER> (*my-fun* 5)
Warning: This function is undefined:
  *MY-FUN*
~~~

Note that in the last example the function object returned by `(adder 3)` is stored in the _value cell_ of `*my-fun*` - thus the error message. If we want to be able to use the symbol `*my-fun*` in the car of a compound form, we have to explicitely store something in its _function cell_ (which is normally done for us by the macro [`defun`](http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm)):

~~~lisp
;;; continued from above
CL-USER> (fboundp '*my-fun*)
NIL
CL-USER> (setf (symbol-function '*my-fun*) (adder 3))
#<Interpreted Function "LAMBDA (N)" {4869FA19}>
CL-USER> (fboundp '*my-fun*)
T
CL-USER> (*my-fun* 5)
8
~~~

Now we are ready do define `doubler` as well:

~~~lisp
CL-USER> (defun doubler (f)
    (lambda (x) (funcall f x x)))
DOUBLER
CL-USER> (doubler #'+)
#<Interpreted Function "LAMBDA (F)" {48675791}>
CL-USER> (doubler '+)
#<Interpreted Function "LAMBDA (F)" {486761B1}>
CL-USER> (funcall (doubler #'+) 4)
8
CL-USER> (funcall (doubler '+) 4)
8
CL-USER> (defparameter *my-plus* '+)
*MY-PLUS*
CL-USER> (funcall (doubler *my-plus*) 4)
8
CL-USER> (defparameter *my-fun* (doubler '+))
*MY-FUN*
CL-USER> (funcall *my-fun* 4)
8
~~~

Note that the argument to `funcall` (and `apply`) can either be the function itself, i.e. `#'+`, or a symbol which has the function in its function cell (is fbound to the function), i.e. `'+`.


All of the above is _extremely simplified_ - we haven't even mentioned macros, special forms, symbol macros, self-evaluating objects, and lexical environments. Read the CLHS section about [form evaluation](http://www.lispworks.com/documentation/HyperSpec/Body/03_aba.htm) for the real deal.

## `setf` functions

A function name can also be a list of two symbols with `setf` as the
firts one, and where the first argument is the new value:

~~~lisp
(defun (setf <name>) (new-value)
  body)
~~~

This mechanism is particularly used for CLOS methods.

Silly example:

~~~lisp
(defparameter *current-name* ""
  "A global name.")

(defun hello (name)
  (format t "hello ~a~&" name))

(defun (setf hello) (new-value)
  (hello new-value)
  (setf *CURRENT-NAME* new-value)
  (format t "current name is now ~a~&" new-value))

(setf (hello) "Alice")
;; hello Alice
;; current name is now Alice
;; NIL
~~~

<a name="curry"></a>

## Currying functions

### Concept

A related concept is that of _[currying](https://en.wikipedia.org/wiki/Currying)_ which you might be familiar with if you're coming from a functional language. After we've read the last section that's rather easy to implement:

~~~lisp
CL-USER> (declaim (ftype (function (function &rest t) function) curry) (inline curry))
NIL
CL-USER> (defun curry (function &rest args)
           (lambda (&rest more-args)
	           (apply function (append args more-args))))
CURRY
CL-USER> (funcall (curry #'+ 3) 5)
8
CL-USER> (funcall (curry #'+ 3) 6)
9
CL-USER> (setf (symbol-function 'power-of-ten) (curry #'expt 10))
#<Interpreted Function "LAMBDA (FUNCTION &REST ARGS)" {482DB969}>
CL-USER> (power-of-ten 3)
1000
~~~

Note that the [`declaim`](http://www.lispworks.com/documentation/HyperSpec/Body/m_declai.htm) statement above is just a hint for the compiler so it can produce more efficient code if it so wishes. Leaving it out won't change the semantics of the function.

### With the Alexandria library

Now that you know how to do it, you may appreciate using the
implementation of the
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html#Data-and-Control-Flow)
library (in Quicklisp).

~~~lisp
(ql:quickload :alexandria)

(defun adder (foo bar)
  "Add the two arguments."
  (+ foo bar))

(defvar add-one (alexandria:curry #'adder 1) "Add 1 to the argument.")

(funcall add-one 10)  ;; => 11

(setf (symbol-function 'add-one) add-one)
(add-one 10)  ;; => 11
~~~

## Hook system

Hooks VS CLOS methods with method combination (before, after, around).

## Documentation

- functions: http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm#function
- ordinary lambda lists: http://www.lispworks.com/documentation/HyperSpec/Body/03_da.htm
- multiple-value-bind: http://clhs.lisp.se/Body/m_multip.htm
