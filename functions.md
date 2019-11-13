---
title: Functions
---

<a name="return"></a>

## Named functions: `defun`

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

### Multiple return values: `values`, `multiple-value-bind` and `nth-value`

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
(setf *res* (foo :a :b :c))
;; :A
~~~

Observe here that `*res*` *is still `:A`*.

All functions that use the return value of `foo` need *not* to change, they
still work. If we had returned a list or an array, this would be
different.

**multiple-value-bind**

We destructure multiple values with `multiple-value-bind` (or
`mvb`+TAB in Slime for short) and we can get one given its position
with `nth-value`:

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

With **nth-value**:

~~~lisp
(nth-value 0 (values :a :b :c))  ;; => :A
(nth-value 2 (values :a :b :c))  ;; => :C
(nth-value 9 (values :a :b :c))  ;; => NIL
~~~

Look here too that `values` is different from a list:

~~~lisp
(nth-value 0 '(:a :b :c)) ;; => (:A :B :C)
(nth-value 1 '(:a :b :c)) ;; => NIL
~~~

Note that `(values)` with no values returns… no values at all.

**multiple-value-list**

While we are at it: [multiple-value-list](http://www.lispworks.com/documentation/HyperSpec/Body/m_mult_1.htm) turns multiple values to a list:

~~~lisp
(multiple-value-list (values 1 2 3))
;; (1 2 3)
~~~

The reverse is **values-list**, it turns a list to multiple values:

~~~lisp
(values-list '(1 2 3))
;; 1
;; 2
;; 3
~~~


## Anonymous functions: `lambda`

Anonymous functions are created with `lambda`:

~~~lisp
(lambda (x) (print x))
~~~

We can call a lambda with `funcall` or `apply` (see below).

If the first element of an unquoted list is a lambda expression, the
lambda is called:

~~~lisp
((lambda (x) (print x)) "hello")
;; hello
~~~

## Calling functions programmatically: `funcall` and `apply`

`funcall` is to be used with a known number of arguments, when `apply`
can be used on a list, for example from `&rest`:

~~~lisp
(funcall #'+ 1 2)
(apply #'+ '(1 2))
~~~

## Higher order functions: functions that return functions

Writing functions that return functions is simple enough:

~~~lisp
(defun adder (n)
  (lambda (x) (+ x n)))
;; ADDER
~~~

Here we have defined the function `adder` which returns an _object_ of _type_ [`function`](http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm).

To call the resulting function, we must use `funcall` or `apply`:

~~~lisp
(adder 5)
;; #<CLOSURE (LAMBDA (X) :IN ADDER) {100994ACDB}>
(funcall (adder 5) 3)
;; 8
~~~

Trying to call it right away leads to an illegal function call:

~~~lisp
((adder 3) 5)
In: (ADDER 3) 5
    ((ADDER 3) 5)
Error: Illegal function call.
~~~

Indeed, CL has different _namespaces_ for functions and variables, i.e. the same _name_ can refer to different things depending on its position in a form that's evaluated.

~~~lisp
;; The symbol foo is bound to nothing:
CL-USER> (boundp 'foo)
NIL
CL-USER> (fboundp 'foo)
NIL
;; We create a variable:
CL-USER> (defparameter foo 42)
FOO
* foo
42
;; Now foo is "bound":
CL-USER> (boundp 'foo)
T
;; but still not as a function:
CL-USER> (fboundp 'foo)
NIL
;; So let's define a function:
CL-USER> (defun foo (x) (* x x))
FOO
;; Now the symbol foo is bound as a function too:
CL-USER> (fboundp 'foo)
T
;; Get the function:
CL-USER> (function foo)
#<FUNCTION FOO>
;; and the shorthand notation:
* #'foo
#<FUNCTION FOO>
;; We call it:
(funcall (function adder) 5)
#<CLOSURE (LAMBDA (X) :IN ADDER) {100991761B}>
;; and call the lambda:
(funcall (funcall (function adder) 5) 3)
8
~~~

To simplify a bit, you can think of each symbol in CL having (at least) two "cells" in which information is stored. One cell - sometimes referred to as its _value cell_ - can hold a value that is _bound_ to this symbol, and you can use [`boundp`](http://www.lispworks.com/documentation/HyperSpec/Body/f_boundp.htm) to test whether the symbol is bound to a value (in the global environment). You can access the value cell of a symbol with [`symbol-value`](http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_5.htm).


The other cell - sometimes referred to as its _function cell_ - can hold the definition of the symbol's (global) function binding. In this case, the symbol is said to be _fbound_ to this definition. You can use [`fboundp`](http://www.lispworks.com/documentation/HyperSpec/Body/f_fbound.htm) to test whether a symbol is fbound. You can access the function cell of a symbol (in the global environment) with [`symbol-function`](http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_1.htm).


Now, if a _symbol_ is evaluated, it is treated as a _variable_ in that its value cell is returned (just `foo`). If a _compound form_, i.e. a _cons_, is evaluated and its _car_ is a symbol, then the function cell of this symbol is used (as in `(foo 3)`).


In Common Lisp, as opposed to Scheme, it is _not_ possible that the car of the compound form to be evaluated is an arbitrary form. If it is not a symbol, it _must_ be a _lambda expression_, which looks like `(lambda `_lambda-list_ _form*_`)`.

This explains the error message we got above - `(adder 3)` is neither a symbol nor a lambda expression.

If we want to be able to use the symbol `*my-fun*` in the car of a compound form, we have to explicitly store something in its _function cell_ (which is normally done for us by the macro [`defun`](http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm)):

~~~lisp
;;; continued from above
CL-USER> (fboundp '*my-fun*)
NIL
CL-USER> (setf (symbol-function '*my-fun*) (adder 3))
#<CLOSURE (LAMBDA (X) :IN ADDER) {10099A5EFB}>
CL-USER> (fboundp '*my-fun*)
T
CL-USER> (*my-fun* 5)
8
~~~

Read the CLHS section about [form evaluation](http://www.lispworks.com/documentation/HyperSpec/Body/03_aba.htm) for more.

## Closures

Closures allow to capture lexical bindings:

~~~lisp
(let ((limit 3)
      (counter -1))
    (defun my-counter ()
      (if (< counter limit)
          (incf counter)
          (setf counter 0))))

(my-counter)
0
(my-counter)
1
(my-counter)
2
(my-counter)
3
(my-counter)
0
~~~

Or similarly:

~~~lisp
(defun repeater (n)
  (let ((counter -1))
     (lambda ()
       (if (< counter n)
         (incf counter)
         (setf counter 0)))))

(defparameter *my-repeater* (repeater 3))
;; *MY-REPEATER*
(funcall *my-repeater*)
0
(funcall *my-repeater*)
1
(funcall *my-repeater*)
2
(funcall *my-repeater*)
3
(funcall *my-repeater*)
0
~~~


See more on [Practical Common Lisp](http://www.gigamonkeys.com/book/variables.html).

## `setf` functions

A function name can also be a list of two symbols with `setf` as the
first one, and where the first argument is the new value:

~~~lisp
(defun (setf <name>) (new-value <other arguments>)
  body)
~~~

This mechanism is particularly used for CLOS methods.

A silly example:

~~~lisp
(defparameter *current-name* ""
  "A global name.")

(defun hello (name)
  (format t "hello ~a~&" name))

(defun (setf hello) (new-value)
  (hello new-value)
  (setf *current-name* new-value)
  (format t "current name is now ~a~&" new-value))

(setf (hello) "Alice")
;; hello Alice
;; current name is now Alice
;; NIL
~~~

<a name="curry"></a>

## Currying

### Concept

A related concept is that of _[currying](https://en.wikipedia.org/wiki/Currying)_ which you might be familiar with if you're coming from a functional language. After we've read the last section that's rather easy to implement:

~~~lisp
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

## Documentation

- functions: http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm#function
- ordinary lambda lists: http://www.lispworks.com/documentation/HyperSpec/Body/03_da.htm
- multiple-value-bind: http://clhs.lisp.se/Body/m_multip.htm
