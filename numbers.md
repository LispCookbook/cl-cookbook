---
title: Numbers
---

## Introduction

Common Lisp has a rich set of numerical types, including integer,
rational, floating point, and complex. 

Some sources:

* [Numbers](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node16.html#SECTION00610000000000000000)
 in Common Lisp the Language, 2nd Edition
* [Numbers, Characters and Strings](http://www.gigamonkeys.com/book/numbers-characters-and-strings.html) 
 in Practical Common Lisp


### Integer types

Common Lisp provides a true integer type, called `bignum`, limited only by the total
memory available (not the machine word size). For example this would
overflow a 64 bit integer by some way:

~~~lisp
* (expt 2 200)
1606938044258990275541962092341162602522202993782792835301376
~~~

For efficiency, integers can be limited to a fixed number of bits,
called a `fixnum` type. The range of integers which can be represented
is given by:

~~~lisp
* most-positive-fixnum
4611686018427387903
* most-negative-fixnum
-4611686018427387904
~~~

Functions which operate on or evaluate to integers include:

* [isqrt](http://clhs.lisp.se/Body/f_sqrt_.htm), which returns the greatest 
  integer less than or equal to the exact positive square root of natural.

~~~lisp
* (isqrt 10)
3
* (isqrt 4)
2
~~~

* [gcd](http://clhs.lisp.se/Body/f_gcd.htm) to find the Greatest Common Denominator
* [lcm](http://clhs.lisp.se/Body/f_lcm.htm#lcm) for the Least Common Multiple.


### Rational types



### Floating point types

Floating point types attempt to represent the continuous real numbers
using a finite number of bits. This means that many real numbers
cannot be represented, but are approximated. This can lead to some nasty
surprises, particularly when converting between base-10 and the base-2
internal representation. If you are working with floating point
numbers then reading [What Every Computer Scientist Should Know About
Floating-Point Arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html) 
is highly recommended.

#### Floating point literals

When reading floating point numbers, the default type is set by the
special variable `*read-default-float-format*`. By default this is
`SINGLE-FLOAT`, so if you want to ensure that a number is read as
double precision then put a `d0` suffix at the end

~~~lisp
* (type-of 1.24)
SINGLE-FLOAT

* (type-of 1.24d0)
DOUBLE-FLOAT
~~~

Other suffixes are `s` (short), `f` (single float), `d` (double
float), `l` (long float) and `e` (default; usually single float).

The default type can be changed, but note that this may break packages
which assume `single-float` type. 

~~~lisp
* (setq *read-default-float-format* 'double-float)
* (type-of 1.24)
DOUBLE-FLOAT
~~~

Note that unlike in some languages, appending a single decimal point
to the end of a number does not make it a float:
~~~lisp
* (type-of 10.)
(INTEGER 0 4611686018427387903)

* (type-of 10.0)
SINGLE-FLOAT
~~~

#### Floating point errors 

If the result of a floating point calculation is too large then a
floating point overflow occurs. By default in
[SBCL](http://www.sbcl.org/) (and other implementations) this results
in an error condition:

~~~lisp
* (exp 1000)
; Evaluation aborted on #<FLOATING-POINT-OVERFLOW {10041720B3}>.
~~~

The error can be caught and handled, or this behaviour can be
changed, to return `+infinity`. In SBCL this is:

~~~lisp
* (sb-int:set-floating-point-modes :traps '(:INVALID :DIVIDE-BY-ZERO))

* (exp 1000)
#.SB-EXT:SINGLE-FLOAT-POSITIVE-INFINITY

* (/ 1 (exp 1000))
0.0
~~~

The calculation now silently continues, without an error condition. 

A similar functionality to disable floating overflow errors 
exists in [CCL](https://ccl.clozure.com/):
~~~lisp
* (set-fpu-mode :overflow nil)
~~~

In SBCL the floating point modes can be inspected:

~~~lisp
* (sb-int:get-floating-point-modes)
(:TRAPS (:OVERFLOW :INVALID :DIVIDE-BY-ZERO) :ROUNDING-MODE :NEAREST
 :CURRENT-EXCEPTIONS NIL :ACCRUED-EXCEPTIONS NIL :FAST-MODE NIL)
~~~

#### Arbitrary precision

For arbitrary high precision calculations there is the
[computable-reals](http://quickdocs.org/computable-reals/) library on
QuickLisp:

~~~lisp
* (ql:quickload :computable-reals)
* (use-package :computable-reals)

* (sqrt-r 2)
+1.41421356237309504880...

* (sin-r (/r +pi-r+ 2))
+1.00000000000000000000...
~~~

The precision to print is set by `*PRINT-PREC*`, by default 20
~~~lisp
* (setq *PRINT-PREC* 50)
* (sqrt-r 2)
+1.41421356237309504880168872420969807856967187537695...
~~~

### Complex types

There are 5 types of complex number: The real and imaginary parts must
be of the same type, and can be rational, or one of the floating point
types (short, single, double or long). 

Complex values can be created using the `#C` reader macro or the
[complex](http://clhs.lisp.se/Body/f_comp_2.htm#complex). The reader
macro does not allow the use of expressions as real and imaginary parts:

~~~lisp
* #C(1 1)
#C(1 1)

* #C((+ 1 2) 5)
; Evaluation aborted on #<TYPE-ERROR expected-type: REAL datum: (+ 1 2)>.

* (complex (+ 1 2) 5)
#C(3 5)
~~~

If constructed with mixed types then the higher precision type will be used for both parts. 

~~~lisp
* (type-of #C(1 1))
(COMPLEX (INTEGER 1 1))

* (type-of #C(1.0 1))
(COMPLEX (SINGLE-FLOAT 1.0 1.0))

* (type-of #C(1.0 1d0))
(COMPLEX (DOUBLE-FLOAT 1.0d0 1.0d0))
~~~

The real and imaginary parts of a complex number can be extracted using
[realpart and imagpart](http://clhs.lisp.se/Body/f_realpa.htm):

~~~lisp
* (realpart #C(7 9))
7
* (imagpart #C(4.2 9.5))
9.5
~~~

## Reading numbers from strings

The [parse-integer](http://clhs.lisp.se/Body/f_parse_.htm) function reads an integer
from a string.

See the [strings section](https://lispcookbook.github.io/cl-cookbook/strings.html#converting-a-string-to-a-number)
on converting between strings and numbers.

## Converting numbers

The `coerce` function converts objects from one type to another,
including numeric types.




### Convert float to rational

The `rational` and `rationalize` functions convert a real numeric
argument into a rational. `rational` assumes that floating point
arguments are exact; `rationalize` expoits the fact that floating
point numbers are only exact to their precision, so can often find a
simpler rational number.

### Convert rational to integer

If the result of a calculation is a rational number where the numerator
is a multiple of the denominator, then it is automatically converted
to an integer:

~~~lisp
* (type-of (* 1/2 4))
(INTEGER 0 4611686018427387903)
~~~

## Rounding floating-point numbers

The `ceiling`, `floor`, `round` and `truncate` functions convert
floating point or rational numbers to integers.

## Comparing numbers

The `=` predicate returns `T` if all arguments are numerically equal. 


## Operating on a series of numbers

Many Common Lisp functions operate on sequences, which can be either lists
or vectors (1D arrays). See the section on 
[mapping](https://lispcookbook.github.io/cl-cookbook/data-structures.html#mapping-map-mapcar-remove-if-not).

Operations on multidimensional arrays are discussed in 
[this section](https://lispcookbook.github.io/cl-cookbook/arrays.html).

Libraries are available for defining and operating on lazy sequences,
including "infinite" sequences of numbers. For example 

* [Clazy](https://common-lisp.net/project/clazy/) which is on QuickLisp
* [lazy-seq](https://github.com/fredokun/lisp-lazy-seq)

## Working with Roman numerals

The `format` function can convert numbers to roman numerals with the
`~@r` directive:

~~~lisp
* (format nil "~@r" 42)
"XLII"
~~~

There is a [gist by tormaroe](https://gist.github.com/tormaroe/90ddd9dc7cc191040be4) for
reading roman numerals.


## Generating random numbers

The [random](http://clhs.lisp.se/Body/f_random.htm#random) function
generates either integer or floating point random numbers, depending on
the type of its argument. 

~~~lisp
* (random 10)
7

* (type-of (random 10))
(INTEGER 0 4611686018427387903)
* (type-of (random 10.0))
SINGLE-FLOAT
* (type-of (random 10d0))
DOUBLE-FLOAT
~~~

In SBCL a [Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)
 pseudo-random number generator is used. See section 
[7.13 of the SBCL manual](http://www.sbcl.org/manual/#Random-Number-Generation) for details.
 
The random seed is stored in [*random-state*](http://clhs.lisp.se/Body/v_rnd_st.htm#STrandom-stateST) 
whose internal representation is implementation dependent. 

Other resources:

* The [random-state](http://quickdocs.org/random-state/) package is available on QuickLisp,
  and provides a number of portable random number generators.

## Trigonometric functions

## Taking logarithms

## Using complex numbers

Common Lisp's mathematical functions generally handle complex numbers,
and return complex numbers when this is the true result. For example:

~~~lisp
* (sqrt -1)
#C(0.0 1.0)

* (exp #C(0.0 0.5))
#C(0.87758255 0.47942555)

* (sin #C(1.0 1.0))
#C(1.2984576 0.63496387)
~~~

