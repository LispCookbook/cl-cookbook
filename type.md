---
title: Type System
---

Common Lisp has a complete and flexible type system and corresponding tools to
inspect, check and manipulate types.

## Values Have Types, Not Variables

Being different from some languages such as C/C++, variables in Lisp are just
*placeholders* for values. When you [`SETF`][setf] something, the value is only
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

Function [`TYPE-OF`][type-of] returns the type of given variable. The returned
result is a [type specifier][type-specifier]. The first element is the type and
the remaining part is extra information of that type. In this case it is lower
bound and upper bound. You can safely ignore it for now. Also remember that
numbers in Lisp have no limit!

Now let's try to [`SETF`][setf] the variable:

~~~lisp
* (setf *var* "hello")
"hello"

* (type-of *var*)
(SIMPLE-ARRAY CHARACTER (5))
~~~

You see, the type of the same variable is changed to
[`SIMPLE-ARRAY`][simple-array], with contents of type [`CHARACTER`][character]
and length 5.

## Type Hierarchy

Inheritance relationship of Lisp types consists a type tree. For example:

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

Function [`DESCRIBE`][describe] shows that type [`NUMBER`][number] inherits type
`T` and is directly inherited by types [`COMPLEX`][complex] and [`REAL`][real].

Similarly, the precedence list of type [`INTEGER`][integer] is
[`INTEGER`][integer] <- [`RATIONAL`][rational] <- [`REAL`][real] <-
[`NUMBER`][number] <- `T`. Type `T` is the root of **all** types.

It might be confusing that [`NUMBER`][number] is a built-in-class. This is
because Lisp types are implemented as CLOS classes. For example:

~~~lisp
* (type-of 1234)
(INTEGER 0 4611686018427387903)

* (class-of 1234)
#<BUILT-IN-CLASS COMMON-LISP:FIXNUM>
~~~

Function [`CLASS-OF`][class-of] gives more specific result.

## Work with Types

Function [`TYPEP`][typep] can be used to check if given argument is of given
type.

~~~lisp
* (typep 1234 'integer)
T
~~~

Function [`SUBTYPEP`][subtypep] can be used to inspect if a type is sub-type of
another. It returns 2 values:
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
parameter. Macro [`TYPECASE`][typecase] is your friend in this case:

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

You can also use [`DEFTYPE`][deftype] to define a new type-specifier. The body
should be a macro checking given argument is of this type (see
[`DEFMACRO`][defmacro]). For example:

~~~lisp
* (deftype square (&optional type size)
  `(and (array ,type (,size ,size))))
SQUARE

* (defvar *square* (make-array '(2 2) :initial-element 100))
*SQUARE*

* *square*
#2A((100 100) (100 100))

* (type-of *square*)
(SIMPLE-ARRAY T (2 2))

* (typep *square* 'square)
T
~~~

[defvar]: http://www.lispworks.com/documentation/lw51/CLHS/Body/m_defpar.htm
[setf]: http://www.lispworks.com/documentation/lw50/CLHS/Body/m_setf_.htm
[type-of]: http://www.lispworks.com/documentation/HyperSpec/Body/f_tp_of.htm
[type-specifier]: http://www.lispworks.com/documentation/lw51/CLHS/Body/04_bc.htm
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
