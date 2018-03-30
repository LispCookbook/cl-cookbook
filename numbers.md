---
title: Numbers
---

## Introduction

Common Lisp has a rich set of numerical types, including integer,
rational, floating point, and complex. 

### Integer types

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

The default type can be changed, but note that this may break packages
which assume `single-float` type. 

~~~lisp
* (setq *read-default-float-format* 'double-float)
* (type-of 1.24)
DOUBLE-FLOAT
~~~



## Converting numbers

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

## Generating random numbers

## Trigonometric functions

## Taking logarithms

## Using complex numbers
