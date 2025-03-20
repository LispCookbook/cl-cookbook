---
title: Functions
---

<a name="return"></a>

## Named functions: `defun`

Creating named functions is done with the `defun` keyword. It follows this model:

~~~lisp
(defun function-name (zero or some arguments)
  "docstring"
  (code of function body))
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

The `print` function prints its one argument to standard output *and
returns it*. "hello world" is thus the returned value of our function.


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
;; NIL         <-- return value: `format t` prints a string
;;                 to standard output and returns nil.
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
(hello "me") ;; a value for the required argument,
             ;; zero optional arguments
(hello "me" "7")  ;; a value for age
(hello "me" 7 :h) ;; a value for age and gender
~~~

### Named parameters: `&key`

It is not always convenient to remember the order of the arguments. It
is thus possible to supply arguments by name: we declare them using
`&key argname`, we set them with `:argname "value"` in the function call,
and we use `argname` as a regular variable in the function body.

Key arguments are `nil` by default.

~~~lisp
(defun hello (name &key happy)
  "If `happy' is `t', print a smiley"
  (format t "hello ~a " name)
  (when happy
    (format t ":)~&")))
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

Last but not least, you would quickly realize it, but we can choose the keys programmatically (they can be variables):

~~~lisp
(let ((key :happy)
      (val t))
  (hello "me" key val))
;; hello me :)
;; NIL
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

~~~
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

### Default values to key parameters

In the lambda list, use pairs to give a default value to an optional or a key argument, like `(happy t)` below:

~~~lisp
(defun hello (name &key (happy t))
~~~

Now `happy` is true by default.

### Was a key parameter specified?

You can skip this tip for now if you want, but come back later to it as it can turn handy.

We saw that a default key parameter is `nil` by default (`(defun hello
(name &key happy) …)`). But how can be distinguish between "the value
is NIL by default" and "the user wants it to be NIL"?

We saw how to use a tuple to set its default value:

`&key (happy t)`

To answer our question, use a triple like this:

`&key (happy t happy-p)`

where `happy-p` serves as a *predicate* variable (using `-p` is only a
convention, give it the name you want) to know if the key was
supplied. If it was, then it will be `T`.

So now, we will print a sad face if `:happy` was explicitely set to
NIL. We don't print it by default.

~~~lisp
(defun hello (name &key (happy nil happy-p))
  (format t "Key supplied? ~a~&" happy-p)
  (format t "hello ~a " name)
  (when happy-p
    (if happy
      (format t ":)")
      (format t ":("))))
~~~

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
(mean 1 2)  ;; => 3/2 (yes, it is printed as a ratio)
(mean 1 2 3 4 5) ;;  => 3
~~~

### Defining key arguments, and allowing more: `&allow-other-keys`

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

Here's a real example. We define a function to open a file that always
uses `:if-exists :supersede`, but still passes any other keys to the
`open` function.

~~~lisp
(defun open-supersede (f &rest other-keys &key &allow-other-keys)
  (apply #'open f :if-exists :supersede other-keys))
~~~

In the case of a duplicated `:if-exists` argument, our first one takes precedence.


## Return values

The return value of the function is the value returned by the last
executed form of the body.

There are ways for non-local exits (`return-from <function name> <value>`), but they are usually not needed.

Common Lisp has also the concept of multiple return values.

### Multiple return values

Returning multiple values is **not** like returning a tuple or a list of
results.

#### Quick example

Let's define a function that returns one value:

~~~lisp
(defun foo ()
  :a)
~~~

now we set the result of calling this function to a variable:

~~~lisp
(defparameter *var* (foo))
~~~

`*var*` is now `:a`. That's a very classical behaviour.

Now our function will return *multiple values*, using `values`:

~~~lisp
(foo ()
  (values :a :b :c))
~~~

and we set `*var*` to its result again:

~~~lisp
(setf *var* (foo))
~~~

What is the value of `*var*`? *It is still :a*. We didn't ask to
capture the remaining values, so `:b` and `:c` were discarded.

This is actually very handy: you can change a function to return more
multiple values than it did, and you don't need to change and refactor
the call sites.

#### Returning multiple values: `values`

The function `values` is used to return multiple values:

~~~lisp
(values 'a 'b)
;; => A
;; => B
~~~

Calling `values` with no arguments returns no value at all.

It is different than returning `nil`.

Unless you use the functions described below to capture multiple
values, only the first will be seen and used by other functions:

~~~lisp
(+ (values 1 2 3) (values 10 20 30))
;; => 11
~~~

`values` does **not** create a list.

#### Why multiple values. A look at CL built-ins

While most Common Lisp forms return a single value, it is
sometimes useful for a function to return several (or none).

For example, `round` returns two values, the rounded result
as well as how much was removed to do the rounding:

~~~lisp
(round 10.33333333)
;; => 10
;; => 0.33333302
~~~

Most of the time you only need the rounded value, but if for
some reason you want to know the remainder, it can be captured.
If you expect all the values calculated by a function to be
used most of the time, then it is better to bundle up the results
in a list, a CLOS instance, *etc.,* and return that.  Only use
multiple values when the first values are most often needed,
and the later ones less often used.

Similarly, getting the content of a hash-table returns two values: the
result, and a boolean saying if the key was found or not. See below.

#### Capturing multiple values: `multiple-value-bind`, `nth-value`, `multiple-value-list` et all

The most common way to capture multiple values is with
`multiple-value-bind`:

~~~lisp
(multiple-value-bind (c d) (values 1 2)
  (list c d))
;; => (1 2)
~~~

The number of values returned does not have to match the
number of variables to bind.  If there are too many values,
the extras are discarded, and if there are too many variables
to bind, the extras are set to `nil`:

~~~lisp
(multiple-value-bind (a b) (values 1 2 3 4)
  (list a b))
;; =>(1 2)
~~~

~~~lisp
(multiple-value-bind (a b) (values 1)
  (list a b))
;; => (1 NIL)
~~~

The function `values` is `setf`-able, which can also be
used to capture values:

~~~lisp
(let (c d)
  (setf (values c d) (values 1 2))
  (list c d))
;; => (1 2)
~~~

Or you can shunt multiple values directly into a function call
with `multiple-value-call`:

~~~lisp
(multiple-value-call #'list (values 1 2 3))
;; => (1 2 3)
~~~

The function `multiple-value-list` is equivalent to the code
above:

~~~lisp
(multiple-value-list (values 1 2 3))
;; => (1 2 3)
~~~~

And you can go the other way, turning a list into multiple
return values with `values-list`:

~~~lisp
(values-list '(1 2 3))
;; => 1
;; => 2
;; => 3
~~~

You can select a particular value with `nth-value`:

~~~lisp
(nth-value 0 (values :a :b :c))  ;; => :A
(nth-value 2 (values :a :b :c))  ;; => :C
(nth-value 9 (values :a :b :c))  ;; => NIL
~~~

Note here too and let us stress again that `values` is different from a list:

~~~lisp
(nth-value 0 (list :a :b :c)) ;; => (:A :B :C)
;; => a list is one data structure of its own

(nth-value 1 (list :a :b :c)) ;; => NIL
;; => no second value to capture
~~~

#### Using multiple values to report success or failure

A typical use for multiple values is to distinguish between finding
`nil` and a lookup failure.  For example, `gethash` returns two
values.  The first is the result of the lookup, which might be `nil` if
nothing is found, but could be an actual `nil` stored in the
hashtable.  The second value returned is a flag indicating if the
lookup was a success:

~~~lisp
(defvar *hash* (make-hash-table))
(setf (gethash 'a *hash*) 12)
(setf (gethash 'b *hash*) nil)
~~~

~~~lisp
(gethash 'a *hash*)
;; => 12           <--- first returned value: our result
;; => T            <--- second returned value: the key was found

(gethash 'b *hash*)
;; => NIL
;; => T

(gethash 'c *hash*)
;; => NIL
;; => NIL          <---- this key wasn't found.
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

There is one thing to keep in mind with `apply`, it is that we can't
use it with super-large lists: the argument list of functions
have a length limit.

We can find this limit in the variable `call-arguments-limit`. It
depends on the implementation. While it is rather large on SBCL
(4611686018427387903), we have another option to apply a function with
arguments of arbitrary length: `reduce`.

### `reduce`

`reduce` is used to apply functions on lists and vectors of arbitrary
length. It repeateadly calls the function with two arguments and walks
over the argument list.

For example, instead of using `apply` like above:

    (apply #'min '(22 1 2 3)) ;; imagine a super large list

we can use `reduce`:

    (reduce #'min '(22 1 2 3))

If our argument was 1000 elements long, `apply` would call the `min`
function with 1000 arguments, while `reduce` would call `min` (nearly)
a 1000 times with 2 arguments each time.

`reduce` walks over the list, which means the following:

- `min` is first called with arguments 22 and 1, and it produces an
  intermediate result: 1.
- `min` is called again with this intermediate result as first argument, and the following argument of the argument list, 2. An intermediate result is produced, 1 again.
- `min` is called again with arguments 1 and 3, and returns the final result, 1.

Look, we can trace it:

~~~lisp
CL-USER> (trace min)
CL-USER> (reduce #'min '(22 1 2 3))
  0: (MIN 22 1)
  0: MIN returned 1
  0: (MIN 1 2)
  0: MIN returned 1
  0: (MIN 1 3)
  0: MIN returned 1
1
~~~

Its full signature is the following:

```lisp
(reduce function sequence &key key from-end start end initial-value)
```

where `key`, `from-end`, `start` and `end` are key arguments found in
other built-in functions (see our data-structures chapter). If given,
`:initial-value` is placed before the first subsequence.

Read more about `reduce` on the Community Spec:

- https://cl-community-spec.github.io/pages/reduce.html


### Referencing functions by name: single quote `'` or sharpsign-quote `#'`?

In the example above, we used `#'`, but a single quote also works, and
we can encounter it in the wild. Which one to use?

It is generally safer to use `#'`, because it respects lexical scope. Observe:

~~~lisp
(defun foo (x)
  (* x 100))

(flet ((foo (x) (1+ x)))
  (funcall #'foo 1))
;; => 2, as expected

;; But:

(flet ((foo (x) (1+ x)))
  (funcall 'foo 1))
;; => 100
~~~

`#'` is actually the shorthand for `(function …)`:

~~~lisp
(function +)
;; #<FUNCTION +>

(flet ((foo (x) (1+ x)))
  (print (function foo))
  (funcall (function foo) 1))
;; #<FUNCTION (FLET FOO) {1001C0ACFB}>
;; 2
~~~

Using `function` or the `#'` shorthand allows us to refer to local
functions. If we pass instead a symbol to `funcall`, what is
called is always the function named by that symbol in the *global environment*.

In addition, `#'` catches the function by value. If the function is redefined, bindings that refered to this function by `#'` will still run its original behaviour.

Let's assign a function to a parameter:

~~~lisp
(defparameter *foo-caller* #'foo)
(funcall *foo-caller* 1)
;; => 100
~~~

Now, if we redefine `foo`, the behaviour of `*foo-caller*` will *not* change:

~~~lisp
(defun foo (x) (1+ x))
;; WARNING: redefining CL-USER::FOO in DEFUN
;; FOO

(funcall *foo-caller* 1)
;; 100  ;; and not 2
~~~

If we bind the caller with `'foo`, a single quote, the function will be resolved at runtime:

~~~lisp
(defun foo (x) (* x 100))  ;; back to original behavior.
(defparameter *foo-caller-2* 'foo)
;; *FOO-CALLER-2*
(funcall *foo-caller-2* 1)
;; 100

;; We change the definition:
(defun foo (x) (1+ x))
;; WARNING: redefining CL-USER::FOO in DEFUN
;; FOO

;; We try again:
(funcall *foo-caller-2* 1)
;; 2
~~~

The behaviour you want depends on your use case. Generally, using sharpsign-quote is less surprising. But if you are running a tight loop and you want live-update mechanisms (think a game or live visualisations), you might want to use a single quote so that your loop picks up the user's new function definition.


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

Closures allow you to capture lexical bindings. This can be useful to store state
that you don't want to have to keep passing into your function(s), either as a convenience
or to keep state variables out of a reachable namespace.

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

In addition to counter generators, another common use of lexical closures is
memoization — caching previous results of functions that are expensive to
calculate. Using the non-memoized Fibonacci function below quickly gets quite
time-consuting to calculate.

~~~lisp
(defun fibonacci (n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(time (fibonacci 40))
;; Evaluation took:
;;   2.843 seconds of real time
;;   2.841360 seconds of total run time (2.796188 user, 0.045172 system)
;;   99.93% CPU
;;   0 bytes consed
;; 102334155
~~~

Using a hash table to store previously calculated results can speed things up.
We could use a `defvar`, but this is a good use case for a closure since that
avoids adding a variable only used by one function to the namespace.

~~~lisp
(let ((memo (make-hash-table)))
  (defun fibonacci (n)
    (let ((value (gethash n memo)))
      (cond ((<= n 1) n)
            (value value)
            (t (setf (gethash n memo)
                     (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))))

(time (fibonacci 40))
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000016 seconds of total run time (0.000015 user, 0.000001 system)
;;   100.00% CPU
;;   0 bytes consed
;; 102334155
~~~

There are several memoization libraries, some of which wrap this lexical closure and caching
mechanics up in a macro.

See more on [Practical Common Lisp](http://www.gigamonkeys.com/book/variables.html).

## `setf` functions

A function name can also be a list of two symbols with `setf` as the
first one, and where the first argument is the new value:

~~~lisp
(defun (setf function-name) (new-value other optional arguments)
  body)
~~~

This mechanism is often used for CLOS methods.

Let's work towards an example. Let's say we manipulate a hash-table
that represents a square. We store the square width in this
hash-table:

~~~lisp
(defparameter *square* (make-hash-table))
(setf (gethash :width *square*) 21)
~~~

During our program life cycle, we can change the square width, with `setf` as we did above.

We define a function to compute a square area. We don't store it in
the hash-table as it is redundant with the dimension.

~~~lisp
(defun area (square)
  (expt (gethash :width square) 2))
~~~

Now, our programming needs lead to the situation where it would be
very handy to change the *area* of the square… and have this reflected
on the square's dimensions. It can be ergonomic for your program's
application interface to define a setf-function, like this:

~~~lisp
(defun (setf area) (new-area square)
  (let ((width (sqrt new-area)))
    (setf (gethash :width square) width)))
~~~

And now you can do:

~~~lisp
(setf (area *SQUARE*) 100)
;; => 10.0
~~~

and check your square (`describe`, `inspect`…), the new width was set.


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
(ql:quickload "alexandria")

(defun adder (foo bar)
  "Add the two arguments."
  (+ foo bar))

(defvar add-one (alexandria:curry #'adder 1) "Add 1 to the argument.")

(funcall add-one 10)  ;; => 11

(setf (symbol-function 'add-one) add-one)
(add-one 10)  ;; => 11
~~~

## Documentation

- functions: <http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm#function>
- ordinary lambda lists: <http://www.lispworks.com/documentation/HyperSpec/Body/03_da.htm>
- multiple-value-bind: <http://clhs.lisp.se/Body/m_multip.htm>
