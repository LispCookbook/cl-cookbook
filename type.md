---
title: Type System
---

Common Lisp has a complete and flexible type system and corresponding tools to
inspect, check and manipulate types.

## Values Have Types, Not Variables

Being different from some languages such as C/C++, variables in Lisp are just
*placeholders* for values. When you [`setf`][setf] something, the value is only
*bound* to the variable. You can bind another value to the same variable later,
as you wish.

This implies a fact that in Common Lisp **values have types**, while variables
do not. This might be surprising at first if you come from a C/C++ background.

For example:

~~~lisp
* (defvar *var* 1234)
*VAR*

* (type-of *var*)
(INTEGER 0 4611686018427387903)
~~~

The function [`type-of`][type-of] returns the type of the given variable. The
returned result is a type specifier. The first element is the type and the
remaining part is extra information of that type. In this case it is lower
bound and upper bound. You can safely ignore it for now. Also remember that
numbers in Lisp have no limit!

Now let's try to [`setf`][setf] the variable:

~~~lisp
* (setf *var* "hello")
"hello"

* (type-of *var*)
(SIMPLE-ARRAY CHARACTER (5))
~~~

You see, the type of the same variable changed to
[`simple-array`][simple-array], with contents of type [`character`][character]
and length 5.

## Type Hierarchy

The inheritance relationship of Lisp types consists a type tree. For example:

~~~lisp
* (describe 'number)
COMMON-LISP:NUMBER
  [symbol]

NUMBER names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:NUMBER>:
  Class precedence-list: NUMBER, T
  Direct superclasses: T
  Direct subclasses: COMPLEX, REAL
  No direct slots.
~~~

The function [`describe`][describe] shows that the type [`number`][number]
inherits from the type `T` and is directly inherited by the types
[`complex`][complex] and [`real`][real].

Similarly, the precedence list of the [`integer`][integer] type is
[`integer`][integer] <- [`rational`][rational] <- [`real`][real] <-
[`number`][number] <- `T`. The type `T` is the root of **all** types.

It might be confusing that [`number`][number] is a built-in-class. This is
because Lisp types are implemented as CLOS classes. For example:

~~~lisp
* (type-of 1234)
(INTEGER 0 4611686018427387903)

* (class-of 1234)
#<BUILT-IN-CLASS COMMON-LISP:FIXNUM>
~~~

The function [`class-of`][class-of] gives a more specific result.

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

Sometimes you may want to perform different actions according to the type of a
parameter. The macro [`typecase`][typecase] is your friend in this case:

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

The function `type-of` usually returns a type specifier in the form of a
list. The head of the list is a symbol and the rest is subsidiary type
information. Such a type specifier is called a compound type specifier. For
example, `(integer 0 4611686018427387903)` and `(vector number 100)` are type
specifiers of this kind.

~~~lisp
* (typep '#(1 2 3) '(vector number 3))
T
~~~

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
