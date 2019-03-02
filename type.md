---
title: Type System
---

Common Lisp has a complete and flexible type system and corresponding tools to
inspect, check and manipulate types.

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
* (defvar *var* 1234)
*VAR*

* (type-of *var*)
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
* (describe integer)
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

## Working with Types

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

## Defining New Type

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

What about compile-time type checking?

Well, you may provide type information for variables, function arguments etc
etc via the macro [`declare`][declare]. However, similar to `:type` slot
introduced in [CLOS section][clos], the effects of type declarations are
undefined in Lisp standard and are implementation specific. So there is no
guarantee that Lisp compiler will perform compile-time type checking.

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
[declare]: http://www.lispworks.com/documentation/HyperSpec/Body/s_declar.htm
[safety]: http://www.lispworks.com/documentation/HyperSpec/Body/d_optimi.htm#speed
