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

Common Lisp provides a true integer type, called `bignum`s, limited only by the total
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


### Rational types

### Floating point types

Floating point types attempt to represent the continuous real numbers
using a finite number of bits. This means that many real numbers
cannot be represented, but are approximated. This can cause some nasty
surprises, particularly when converting between base-10 and the base-2
internal representation. If you are working with floating point
numbers then reading [What Every Computer Scientist Should Know About
Floating-Point Arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html) 
is highly recommended.

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

A similar functionality exists in [CCL](https://ccl.clozure.com/):
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

## Reading numbers

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


## Converting numbers

The `coerce` function converts objects from one type to another,
including numeric types.



### Convert to rational

The `rational` and `rationalize` functions convert a real numeric
argument into a rational. `rational` assumes that floating point
arguments are exact; `rationalize` expoits the fact that floating
point numbers are only exact to their precision, so can often find a
simpler rational number.

## Comparing numbers

The `=` predicate returns `T` if all arguments are numerically equal. 

## Rounding floating-point numbers

The `ceiling`, `floor`, `round` and `truncate` functions convert
floating point or rational numbers to integers.

## Operating on a series of integers

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
~~~

There are 5 types of complex number: The real and imaginary parts must
be of the same type, and can be rational, or one of the floating point
types (short, single, double or long). 
