---
title: Type System
---

Common Lisp has a complete and flexible type system and corresponding
tools to inspect, check and manipulate types. It allows creating
custom types, adding type declarations to variables and functions and
thus to get compile-time warnings and errors.


## Values Have Types, Not Variables

Being different from some languages such as C/C++, variables in Lisp are just
*placeholders* for objects[^1]. When you [`setf`][setf] a variable, an object
is "placed" in it. You can place another value to the same variable later, as
you wish.

This implies a fact that in Common Lisp **objects have types**, while
variables do not. This might be surprising at first if you come from a C/C++
background.

For example:

~~~lisp
(defvar *var* 1234)
*VAR*

(type-of *var*)
(INTEGER 0 4611686018427387903)
~~~

The function [`type-of`][type-of] returns the type of the given object. The
returned result is a [type-specifier][type-specifiers]. In this case the first
element is the type and the remaining part is extra information (lower and
upper bound) of that type.  You can safely ignore it for now. Also remember
that integers in Lisp have no limit!

Now let's try to [`setf`][setf] the variable:

~~~lisp
* (setf *var* "hello")
"hello"

* (type-of *var*)
(SIMPLE-ARRAY CHARACTER (5))
~~~

You see, `type-of` returns a different result: [`simple-array`][simple-array]
of length 5 with contents of type [`character`][character]. This is because
`*var*` is evaluated to string `"hello"` and the function `type-of` actually
returns the type of object `"hello"` instead of variable `*var*`.

## Type Hierarchy

The inheritance relationship of Lisp types consists a type graph and the root
of all types is `T`. For example:

~~~lisp
* (describe 'integer)
COMMON-LISP:INTEGER
  [symbol]

INTEGER names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:INTEGER>:
  Class precedence-list: INTEGER, RATIONAL, REAL, NUMBER, T
  Direct superclasses: RATIONAL
  Direct subclasses: FIXNUM, BIGNUM
  No direct slots.

INTEGER names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (SB-KERNEL::LOW '*) (SB-KERNEL::HIGH '*))
~~~

The function [`describe`][describe] shows that the symbol [`integer`][integer]
is a primitive type-specifier that has optional information lower bound and
upper bound. Meanwhile, it is a built-in class. But why?

Most common Lisp types are implemented as CLOS classes. Some types are simply
"wrappers" of other types. Each CLOS class maps to a corresponding type. In
Lisp types are referred to indirectly by the use of [`type
specifiers`][type-specifiers].

There are some differences between the function [`type-of`][type-of] and
[`class-of`][class-of]. The function `type-of` returns the type of a given
object in type specifier format while `class-of` returns the implementation
details.

~~~lisp
* (type-of 1234)
(INTEGER 0 4611686018427387903)

* (class-of 1234)
#<BUILT-IN-CLASS COMMON-LISP:FIXNUM>
~~~

## Checking Types

The function [`typep`][typep] can be used to check if the first argument is of
the given type specified by the second argument.

~~~lisp
* (typep 1234 'integer)
T
~~~

The function [`subtypep`][subtypep] can be used to inspect if a type inherits
from the another one. It returns 2 values:
- `T, T` means first argument is sub-type of the second one.
- `NIL, T` means first argument is *not* sub-type of the second one.
- `NIL, NIL` means "not determined".

For example:

~~~lisp
* (subtypep 'integer 'number)
T
T

* (subtypep 'string 'number)
NIL
T
~~~

Sometimes you may want to perform different actions according to the type of
an argument. The macro [`typecase`][typecase] is your friend:

~~~lisp
* (defun plus1 (arg)
    (typecase arg
      (integer (+ arg 1))
      (string (concatenate 'string arg "1"))
      (t 'error)))
PLUS1

* (plus1 100)
101 (7 bits, #x65, #o145, #b1100101)

* (plus1 "hello")
"hello1"

* (plus1 'hello)
ERROR
~~~

## Type Specifier

A type specifier is a form specifying a type. As mentioned above, returning
value of the function `type-of` and the second argument of `typep` are both
type specifiers.

As shown above, `(type-of 1234)` returns `(INTEGER 0
4611686018427387903)`. This kind of type specifiers are called compound type
specifier. It is a list whose head is a symbol indicating the type. The rest
part of it is complementary information.

~~~lisp
* (typep '#(1 2 3) '(vector number 3))
T
~~~

Here the complementary information of the type `vector` is its elements type
and size respectively.

The rest part of a compound type specifier can be a `*`, which means
"anything". For example, the type specifier `(vector number *)` denotes a
vector consisting of any number of numbers.

~~~lisp
* (typep '#(1 2 3) '(vector number *))
T
~~~

The trailing parts can be omitted, the omitted elements are treated as
`*`s:

~~~lisp
* (typep '#(1 2 3) '(vector number))
T

* (typep '#(1 2 3) '(vector))
T
~~~

As you may have guessed, the type specifier above can be shortened as
following:

~~~lisp
* (typep '#(1 2 3) 'vector)
T
~~~

You may refer to the [CLHS page][type-specifiers] for more information.

## Defining New Types

You can use the macro [`deftype`][deftype] to define a new type-specifier.

Its argument list can be understood as a direct mapping to elements of rest
part of a compound type specifier. They are be defined as optional to allow
symbol type specifier.

Its body should be a macro checking whether given argument is of this type
(see [`defmacro`][defmacro]).

Now let us define a new data type. The data type should be a array with at
most 10 elements. Also each element should be a number smaller than 10. See
following code for an example:

~~~lisp
* (defun small-number-array-p (thing)
    (and (arrayp thing)
      (<= (length thing) 10)
      (every #'numberp thing)
      (every (lambda (x) (< x 10)) thing)))

* (deftype small-number-array (&optional type)
    `(and (array ,type 1)
          (satisfies small-number-array-p)))

* (typep '#(1 2 3 4) '(small-number-array number))
T

* (typep '#(1 2 3 4) 'small-number-array)
T

* (typep '#(1 2 3 4 100) 'small-number-array)
NIL

* (small-number-array-p '#(1 2 3 4 5 6 7 8 9 0 1))
NIL
~~~

## Type Checking

Common Lisp supports run-time type checking via the macro
[`check-type`][check-type]. It accepts a [`place`][place] and a type specifier
as arguments and signals an [`type-error`][type-error] if the contents of
place are not of the given type.

~~~lisp
* (defun plus1 (arg)
    (check-type arg number)
    (1+ arg))
PLUS1

* (plus1 1)
2 (2 bits, #x2, #o2, #b10)

* (plus1 "hello")
; Debugger entered on #<SIMPLE-TYPE-ERROR expected-type: NUMBER datum: "Hello">

The value of ARG is "Hello", which is not of type NUMBER.
   [Condition of type SIMPLE-TYPE-ERROR]
...
~~~


## Compile-time type checking

You may provide type information for variables, function arguments
etc via [`proclaim`][proclaim], [`declaim`][declaim] and [`declare`][declare].
However, similar to the `:type` slot
introduced in [CLOS section][clos], the effects of type declarations are
undefined in Lisp standard and are implementation specific. So there is no
guarantee that the Lisp compiler will perform compile-time type checking.

However, it is possible, and SBCL is an implementation that does
thorough type checking.

Let's recall first that Lisp already warns about simple type
warnings. The following function wrongly wants to concatenate a string
and a number. When we compile it, we get a type warning.

~~~lisp
(defconstant +foo+ 3)
(defun bar ()
  (concatenate 'string "+" +foo+))
; caught WARNING:
;   Constant 3 conflicts with its asserted type SEQUENCE.
;   See also:
;     The SBCL Manual, Node "Handling of Types"
~~~

The example is simple, but it already shows a capacity some other
languages don't have, and it is actually useful during development ;)
Now, we'll do better.


### Declaring the type of variables

Use the macro [`declaim`][declaim].

Let's declare that our global variable `*name*` is a string (you can
type the following in any order in the REPL):

~~~lisp
(declaim (type (string) *name*))
(defparameter *name* "book")
~~~

Now if we try to set it with a bad type, we get a `simple-type-error`:

~~~lisp
(setf *name* :me)
Value of :ME in (THE STRING :ME) is :ME, not a STRING.
   [Condition of type SIMPLE-TYPE-ERROR]
~~~

We can do the same with our custom types. Let's quickly declare the type `list-of-strings`:

~~~lisp
(defun list-of-strings-p (list)
  "Return t if LIST is non nil and contains only strings."
  (and (consp list)
       (every #'stringp list)))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))
~~~

Now let's declare that our `*all-names*` variables is a list of strings:

~~~lisp
(declaim (type (list-of-strings) *all-names*))
;; and with a wrong value:
(defparameter *all-names* "")
;; we get an error:
Cannot set SYMBOL-VALUE of *ALL-NAMES* to "", not of type
(SATISFIES LIST-OF-STRINGS-P).
   [Condition of type SIMPLE-TYPE-ERROR]
~~~

We can compose types:

~~~lisp
(declaim (type (or null list-of-strings) *all-names*))
~~~

### Declaring the input and output types of functions

We use again the `declaim` macro, with `ftype (function …)` instead of just `type`:

~~~lisp
(declaim (ftype (function (fixnum) fixnum) add))
;;                         ^^input ^^output [optional]
(defun add (n)
  (+ n  1))
~~~

With this we get nice type warnings at compile time.

If we change the function to erroneously return a string instead of a
fixnum, we get a warning:

~~~lisp
(defun add (n)
  (format nil "~a" (+ n  1)))
; caught WARNING:
;   Derived type of ((GET-OUTPUT-STREAM-STRING STREAM)) is
;     (VALUES SIMPLE-STRING &OPTIONAL),
;   conflicting with the declared function return type
;     (VALUES FIXNUM &REST T).
~~~

If we use `add` inside another function, to a place that expects a
string, we get a warning:

~~~lisp
(defun bad-concat (n)
  (concatenate 'string (add n)))
; caught WARNING:
;   Derived type of (ADD N) is
;     (VALUES FIXNUM &REST T),
;   conflicting with its asserted type
;     SEQUENCE.
~~~

If we use `add` inside another function, and that function declares
its argument types which appear to be incompatible with those of
`add`, we get a warning:

~~~lisp
(declaim (ftype (function (string)) bad-arg))
(defun bad-arg (n)
    (add n))
; caught WARNING:
;   Derived type of N is
;     (VALUES STRING &OPTIONAL),
;   conflicting with its asserted type
;     FIXNUM.
~~~

This all happens indeed *at compile time*, either in the REPL,
either with a simple `C-c C-c` in Slime, or when we `load` a file.

### Declaring class slots types

A class slot accepts a `:type` slot option. It is however generally
*not* used to check the type of the initform. SBCL, starting with
[version 1.5.9][sbcl159] released on
november 2019, now gives those warnings, meaning that this:

~~~lisp
(defclass foo ()
  ((name :type number :initform "17")))
~~~

throws a warning at compile time.


Note: see also [sanity-clause][sanity-clause], a data
serialization/contract library to check slots' types during
`make-instance` (which is not compile time).

### Limitations

Complex types involving `satisfies` are not checked inside a function
body, only at its boundaries. Even if it does a lot, SBCL doesn't do
as much as a statically typed language.

Consider this example, where we badly increment an integer with a
string:

~~~lisp
(declaim (ftype (function () string) bad-adder))
(defun bad-adder ()
  (let ((res 10))
    (loop for name in '("alice")
       do (incf res name))  ;; bad
    (format nil "finally doing sth with ~a" res)))
~~~

Compiling this function doesn't throw a type warning.

However, if we had the problematic line at the function's boundary
we'd get the warning:

~~~lisp
(defun bad-adder ()
  (let ((res 10))
    (loop for name in  '("alice")
       return (incf res name))))
; in: DEFUN BAD-ADDER
;     (SB-INT:NAMED-LAMBDA BAD-ADDER
;         NIL
;       (BLOCK BAD-ADDER
;         (LET ((RES 10))
;           (LOOP FOR NAME IN *ALL-NAMES* RETURN (INCF RES NAME)))))
;
; caught WARNING:
;   Derived type of ("a hairy form" NIL (SETQ RES (+ NAME RES))) is
;     (VALUES (OR NULL NUMBER) &OPTIONAL),
;   conflicting with the declared function return type
;     (VALUES STRING &REST T).
~~~

What can we conclude? This is yet another reason to decompose your
code into small functions.


## See also

- the article [Static type checking in SBCL](https://medium.com/@MartinCracauer/static-type-checking-in-the-programmable-programming-language-lisp-79bb79eb068a), by Martin Cracauer
- the article [Typed List, a Primer](https://alhassy.github.io/TypedLisp/) - let's explore Lisp's fine-grained type hierarchy! with a shallow comparison to Haskell.
- the [Coalton](https://github.com/stylewarning/coalton) library
  (pre-alpha): adding Hindley-Milner type checking to Common Lisp
  which allows for gradual adoption, in the same way Typed Racket or
  Hack allows for. It is as an embedded DSL in Lisp that resembles
  Standard ML or OCaml, but lets you seamlessly interoperate with
  non-statically-typed Lisp code (and vice versa).

---

[^1]: The term *object* here has nothing to do with Object-Oriented or so. It
    means "any Lisp datum".

[defvar]: http://www.lispworks.com/documentation/lw51/CLHS/Body/m_defpar.htm
[setf]: http://www.lispworks.com/documentation/lw50/CLHS/Body/m_setf_.htm
[type-of]: http://www.lispworks.com/documentation/HyperSpec/Body/f_tp_of.htm
[type-specifiers]: http://www.lispworks.com/documentation/lw51/CLHS/Body/04_bc.htm
[number]: http://www.lispworks.com/documentation/lw61/CLHS/Body/t_number.htm
[typep]: http://www.lispworks.com/documentation/lw51/CLHS/Body/f_typep.htm
[subtypep]: http://www.lispworks.com/documentation/lw71/CLHS/Body/f_subtpp.htm
[string]: http://www.lispworks.com/documentation/lw71/LW/html/lw-426.htm
[simple-array]: http://www.lispworks.com/documentation/lw70/CLHS/Body/t_smp_ar.htm
[integer]: http://www.lispworks.com/documentation/lw71/CLHS/Body/t_intege.htm
[describe]: http://www.lispworks.com/documentation/lw51/CLHS/Body/f_descri.htm
[clos]: clos.html
[character]: http://www.lispworks.com/documentation/lcl50/ics/ics-14.html
[number]: http://www.lispworks.com/documentation/lw61/CLHS/Body/t_number.htm
[complex]: http://www.lispworks.com/documentation/lw70/CLHS/Body/t_comple.htm
[real]: http://www.lispworks.com/documentation/lw70/CLHS/Body/t_real.htm
[rational]: http://www.lispworks.com/documentation/HyperSpec/Body/t_ration.htm
[class-of]: http://www.lispworks.com/documentation/HyperSpec/Body/f_clas_1.htm
[typecase]: http://www.lispworks.com/documentation/lw60/CLHS/Body/m_tpcase.htm
[deftype]: http://www.lispworks.com/documentation/lw51/CLHS/Body/m_deftp.htm
[defmacro]: http://www.lispworks.com/documentation/lw70/CLHS/Body/m_defmac.htm
[check-type]: http://www.lispworks.com/documentation/HyperSpec/Body/m_check_.htm#check-type
[type-error]: http://www.lispworks.com/documentation/HyperSpec/Body/e_tp_err.htm#type-error
[place]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#place
[proclaim]: http://www.lispworks.com/documentation/HyperSpec/Body/f_procla.htm
[declaim]: http://www.lispworks.com/documentation/HyperSpec/Body/m_declai.htm
[declare]: http://www.lispworks.com/documentation/HyperSpec/Body/s_declar.htm
[safety]: http://www.lispworks.com/documentation/HyperSpec/Body/d_optimi.htm#speed
[sbcl159]: http://www.sbcl.org/news.html#1.5.9
[sanity-clause]: https://github.com/fisxoj/sanity-clause
