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

If you prefer, you can create a package for our tests:

~~~lisp
(defpackage :trivia-tests (:use :cl :trivia))
(in-package :trivia-tests)
~~~


## Common destructuring patterns

### Match on simple values

You can use `match` to test a variable against other values:

~~~lisp
(let ((x 3))
  (match x
     (0 :no-match)
     (1 :still-no-match)
     (3 :yes)
     (_ :other)))
;; => :yes
~~~

The same, with a list:

~~~lisp
(let ((x (list :a)))
  (match x
    (0 :nope)
    (1 :still-nope)
    (3 :yes)
    ((list :a) :a-list) ;; <-- match (list :a) entirely. Next we'll match on the content.
    (_ :other)))
;; => :A-LIST
~~~

It works with strings too (wherease CL's built-in `case` doesn't):

~~~lisp
(let ((x "a string"))
  (match x
    ("a string" :a-string)
    (_ :other)))
;; => :A-STRING
~~~

We can use this knowledge to write an elegant recursive Fibonacci function:

~~~lisp
(defun fib (n)
  (match n
    (0 0)
    (1 1)
    (_ (+ (fib (- n 1)) (fib (- n 2))))))
~~~

It would be more interesting to match against the content of our
variable `x`, when it is a compound data structure: that's what we'll
do in the next sections with Trivia's patterns.


### The fall-through is `_`

Observe how we used `_` (the underscore) for the fall-through cause,
and not `t` as it is used in `cond` or `case`.


### `cons`

The `cons` pattern matches againts lists and other cons cells.

The lengths of the matched object and of the pattern can be
different. Below, `y` receives all the rest that is not matched by `x`.

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

`list` is a strict pattern, it expects the length of the matched
object to be the same length as its subpatterns.

~~~lisp
(match '(something 2 3)
  ((list a b _)
   (values a b)))
SOMETHING
2
~~~

Without the `_` placeholder, it would not match:

~~~lisp
(match '(something 2 3)
  ((list a b)
   (values a b)))
NIL
~~~

The `list*` pattern is flexible on the object's length:

~~~lisp
(match '(something 2 3)
  ((list* a b)
   (values a b)))
SOMETHING
(2 3)
~~~

~~~lisp
(match '(1 2 . 3)
  ((list* _ _ x)
   x))
3
~~~

However pay attention that if `list*` receives only one object, that
object is returned, regardless of whether or not it is a list:

~~~lisp
(match #(0 1 2)
  ((list* a)
   a))
#(0 1 2)
~~~

This is related to the definition of `list*` in the HyperSpec: http://clhs.lisp.se/Body/f_list_.htm.


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
against the subpattern. The `assoc` and `property` patterns match
single values. The `alist` and `plist` patterns effectively `and`
several patterns.

~~~lisp
(match '(:a 1 :b 2)
  ((property :a 1) 'found))
;; -> FOUND
~~~

~~~lisp
(match '(:a 1 :b 2)
  ((property :a n) n))
;; -> 1
~~~

~~~lisp
(match '(:a 1 :b 2)
  ((property :d n) n))
;; -> NIL
~~~

Like `cl:getf` you can add a default value to the `property` pattern.
Unlike `cl:getf` you can further add a flag to catch if the default
is being used.

~~~lisp
(match '(:a 1 :b 2)
  ((property :c c 3) c))
;; -> 3
~~~

~~~lisp
(match '(:a 1 :b 2)
  ((property :c c 3 foundp) (list c foundp)))
;; -> (3 NIL)
~~~

The pattern `property!` will only match if the key is actually present.

~~~lisp
(match '(:a 1 :b 2)
  ((property :d n) (list n))
  (_ 'fail))
;; -> (NIL)
~~~

~~~lisp
(match '(:a 1 :b 2)
  ((property! :d n) (list n))
  (_ 'fail))
;; -> FAIL
~~~

Several properties can be matched with `plist`.

~~~lisp
(match '(:a 1 :b 2)
  ((plist :a 1 :b x) x))
;; -> 2
~~~

The pattern `assoc` matches association lists. It can take the `:test` keyword like `cl:assoc`.

~~~lisp
(match '((a . 1) (b . 2) (c . 3))
  ((assoc 'a 1) 'ok))
;; -> OK
~~~

~~~lisp
(match '((a . 1) (b . 2) (c . 3))
  ((assoc 'b x) x))
;; -> 2
~~~

~~~lisp
(match '(("one" . 1) ("two" . 2))
  ((assoc "one" x :test #'string-equal) x))
;; -> 1
~~~

The pattern `alist` matches several elements in an association list.

~~~lisp
(match '((a . 1) (b . 2) (c . 3))
  ((alist ('a . 1) ('c . n)) n))
;; -> 3
~~~

### Array, simple-array, row-major-array patterns

See https://github.com/guicho271828/trivia/wiki/Type-Based-Destructuring-Patterns#array-simple-array-row-major-array-pattern !

## Logic based patterns

We can combine any pattern with some logic.

### `and`, `or`

The following:

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

## Guards

Guards allow us to use patterns *and* to verify them against a predicate.

The syntax is `guard` + `subpattern` + `a test form`, and the body.

~~~lisp
(match (list 2 5)
  ((guard (list x y)     ; subpattern1
          (= 10 (* x y))) ; test-form
   :ok))
~~~

If the subpattern is true, the test form is evaluated, and if it is
true it is matched against subpattern1.


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
