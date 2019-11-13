---
title: Pattern Matching
---

The ANSI Common Lisp standard does not include facilities for pattern
matching, but libraries existed for this task and
[Trivia](https://github.com/guicho271828/trivia) became a community
standard.

For an introduction to the concepts of pattern matching, see [Trivia's wiki](https://github.com/guicho271828/trivia/wiki/What-is-pattern-matching%3F-Benefits%3F).

Trivia matches against *a lot* of lisp objects and is extensible.

The library is in Quicklisp:

~~~lisp
(ql:quickload "trivia")
~~~

For the following examples, let's `use` the library:

~~~lisp
(use-package :trivia)
~~~


## Common destructuring patterns

### `cons`

~~~lisp
(match '(1 2 3)
  ((cons x y)
  ; ^^ pattern
   (print x)
   (print y)))
;; |-> 1
;; |-> (2 3)
~~~

### `list`, `list*`

~~~lisp
(match '(something #(0 1 2))
  ((list a (vector 0 _ b))
   (values a b)))
SOMETHING
2
~~~

`list*` pattern:

~~~lisp
(match '(1 2 . 3)
  ((list* _ _ x)
   x))
3
~~~

Note that using `list` would match nothing.

### `vector`, `vector*`

`vector` checks if the object is a vector, if the lengths are the
same, and if the contents matches against each subpatterns.

`vector*` is similar, but called a soft-match variant that allows if
the length is larger-than-equal to the length of subpatterns.

~~~lisp
(match #(1 2 3)
  ((vector _ x _)
   x))
;; -> 2
~~~

~~~lisp
(match #(1 2 3 4)
  ((vector _ x _)
   x))
;; -> NIL : does not match
~~~

~~~lisp
(match #(1 2 3 4)
  ((vector* _ x _)
   x))
;; -> 2 : soft match.
~~~

~~~
<vector-pattern> : vector      | simple-vector
                   bit-vector  | simple-bit-vector
                   string      | simple-string
                   base-string | simple-base-string | sequence
(<vector-pattern> &rest subpatterns)
~~~

### Class and structure pattern

There are three styles that are equivalent:

~~~lisp
(defstruct foo bar baz)
(defvar *x* (make-foo :bar 0 :baz 1)

(match *x*
  ;; make-instance style
  ((foo :bar a :baz b)
   (values a b))
  ;; with-slots style
  ((foo (bar a) (baz b))
   (values a b))
  ;; slot name style
  ((foo bar baz)
   (values bar baz)))
~~~

### `type`, `satisfies`

The `type` pattern matches if the object is of type. `satisfies` matches
if the predicate returns true for the object. A lambda form is
acceptable.

### `assoc`, `property`, `alist`, `plist`

All these patterns first check if the pattern is a list. If that is
satisfied, then they obtain the contents, and the value is matched
against the subpattern.


### Array, simple-array, row-major-array patterns

See https://github.com/guicho271828/trivia/wiki/Type-Based-Destructuring-Patterns#array-simple-array-row-major-array-pattern !

## Logic based patterns

### `and`, `or`

~~~lisp
(match x
  ((or (list 1 a)
       (cons a 3))
   a))
~~~

matches against both `(1 2)` and `(4 . 3)` and returns 2 and 4, respectively.

### `not`

It does not match when subpattern matches. The variables used in the
subpattern are not visible in the body.

### `guards`

The syntax is `guard` + `subpattern` + `a test form`, and the body.

~~~lisp
(match (list 2 5)
  ((guard (list x y)     ; subpattern
          (= 10 (* x y)) ; test-form
          (- x y) (satisfies evenp)) ; generator1, subpattern1
   t))
~~~

If the subpattern is true, the test form is evaluated, and if it is
true it is matched against subpattern1.

The above returns `nil`, since `(- x y) == 3` does not satisfies `evenp`.


## Nesting patterns

Patterns can be nested:

~~~lisp
(match '(:a (3 4) 5)
  ((list :a (list _ c) _)
   c))
~~~

returns `4`.

## See more

See [special patterns](https://github.com/guicho271828/trivia/wiki/Special-Patterns): `place`, `bind` and `access`.
