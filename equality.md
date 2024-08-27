---
title: Equality
---

Common Lisp has various equality functions: `=`, `eq`, `eql`, `equal`,
`equalp`, `string-equal`, `char-equal`… but what are the differences??
We tell you everything, with examples.

In short:

- `=` is only for numbers and `equal` is the equality predicate that does what you expect on many things.
- you can't overload built-in operators such as `=` or `equal` for your own classes, unless you use a library.
- when you manipulate sequences of strings with functional built-ins (`remove-if`, `find`…) and you are surprised to get no results, you probably forgot the `:test` key argument: `(find "foo" '("hello" "foo") :test #'equal)`.
- not using a generic predicate gives better static analysis and performance.


## `=` is for numbers (beware of `NIL`)

The `=` function compares the value of two or more numbers:

~~~lisp
(= 2 2) ;; => T
(= 2 2.0 2 2) ;; => T
(= 2 4/2) ;; => T

(= 2 42) ;; => NIL
~~~

but `=` is only for numbers. In the below example we get an error with
the interactive debugger. We show the error message, the condition
type, and the backtrace, from SBCL.

~~~lisp
(= 2 NIL)
;; => ERROR:
The value
  NIL
is not of type
  NUMBER
when binding SB-KERNEL::Y

   [Condition of type TYPE-ERROR]

Restarts:
  …

Backtrace:
  0: (SB-KERNEL:TWO-ARG-= 2 NIL) [external]
  1: (SB-VM::GENERIC-=)
  2: (= 2 NIL)
~~~

Note how `SB-KERNEL::Y` refers to an internal variable of the
compiler. No, you don't have a `Y` in your code.

As a consequence, if your equality check with numbers might contain
NILs, you can use `equalp`, or encapsulate your variables with `(or …
0)`, or do prior checks with `(null …)`.

## `eq` is low-level. Think pointers, position in memory.

> (eq x y) is true if and only if x and y are the same identical object.

Use `eq` for symbols and keywords.

Those are true:

~~~lisp
(eq :a :a)
(eq 'a 'a)
~~~

If we compare an object with itself, it is `eq`:

~~~lisp
(let ((x '(a . b)))
  (eq x x))
;; => T
~~~

`eq` does **not** meaningfully work to compare numbers, lists, strings and other
compound objects. It looks like it can, but it isn't specified to be
true for all implementations.

As such, `eq` works for numbers on my implementation, but it might not on yours:

~~~lisp
(eq 2 2) ;; => T or NIL, this is not specified (it is T on my implementation).

;; However:
(eq
  49827139472193749213749218734917239479213749127394871293749123
  49827139472193749213749218734917239479213749127394871293749123) ;; => NIL on my implementation, and on yours?
~~~

Thea reasion is that an implementation might allocate the exact same position in memory for
the same number, but it might not. This isn't dictated by the standard.

Likewise, these might depend on the implementation:

~~~lisp
(eq '(a . b) '(a . b)) ;; might be true or false.
(eq #\a #\a) ;; true or false
~~~

Comparing lists or strings are false:

~~~lisp
(eq (list 'a) (list 'a)) ;; => NIL
(eq "a" "a") ;; => NIL
~~~

<!-- or unspecified? -->

those strings (vectors of characters) are not equal by `eq` because your implementation might have
created two different string objects in memory.


## `eql` is a more general `eq` also for numbers of same types and characters.

> The `eql` predicate is true if its arguments are `eq`, or if they are numbers of the same type with the same value, or if they are character objects that represent the same character.

In terms of usefulness, we could say that `eq` < `eql`.

Now this number comparison is true:

~~~lisp
(eql 3 3) ;; => T
~~~

but beware, this one isn't because 3 and 3.0 are not of the same type
(integer and single float):

~~~lisp
(eql 3 3.0) ;; => NIL
~~~

for complex numbers:

~~~lisp
(eql #c(3 -4) #c(3 -4)) ;; is true.
(eql #c(3 -4.0) #c(3 -4)) ;; is false (because of -4.0 and -4)
~~~

Comparing two characters works:

~~~lisp
(eql #\A #\A) ;; => T
~~~

And we still can't meaningfully compare lists or cons cells:

~~~lisp
(eql (cons 'a 'b) (cons 'a 'b)) ;; => NIL
~~~

## `equal` is also for strings (for objects whose printed representation is similar).

> The `equal` predicate is true if its arguments are structurally similar (isomorphic) objects. A rough rule of thumb is that two objects are `equal` if and only if their printed representations are the same.

Again, conceptually, we could say that `eq` < `eql` < `equal`.

We can still not compare numbers of different types:

~~~lisp
(equal 3 3.0) ;; => NIL
~~~

but we can now compare lists and cons cells. Indeed, their printed
representation is the same. No matter this time if they are different
objects in memory.

~~~lisp
(equal (cons 'a 'b) (cons 'a 'b)) ;; => T

(equal (list 'a) (list 'a)) ;; => T
~~~

We can compare strings!

~~~lisp
(equal "Foo" "Foo") ;; => T
~~~

No matter if they are different objects in memory:

~~~lisp
(equal "Foo" (copy-seq "Foo")) ;; => T
~~~

Case is important. Indeed, "FOO" doesn't print the same as "foo":

~~~lisp
(equal "FOO" "foo") ;; => NIL
~~~

## `equalp` is case-insensitive for strings and for numerical value of numbers.

> Two objects are `equalp` if they are `equal`; if they are characters and satisfy `char-equal`, which ignores alphabetic case and certain other attributes of characters; if they are numbers and have the same numerical value, even if they are of different types; or if they have components that are all `equalp`.

Continuing with our ordering, we could say that `eq` < `eql` < `equal` < `equalp`.

We can compare two numbers, looking at their value, even if they have different types:

~~~lisp
(equalp 3 3.0) ;; => T
~~~

Now look at our string comparison:

~~~lisp
(equalp "FOO" "foo") ;; => T
~~~

`equalp` is case *in*sensitive for strings because a string is a
sequence of characters, `equalp` compares all of its components and it
uses `char-equal` for characters, which ignores the characters' case.


## Other comparison functions

### `null`

The function `null` returns true if its one argument is NIL.


### `eql` is used by default by many CL built-ins

This is a common issue for newcomers who manipulate
strings. Sometimes, you use a CL built-in function and you are puzzled
why you get no result.

Look at this:

~~~lisp
(find "foo" (list "test" "foo" "bar"))
;; NIL
~~~

we want to know if the string "foo" exists in the given list. We get
NIL. What's happening?

This CL built-in function, as all that work for sequences, use `eql`
for testing each elements. But `(eql "foo" "foo")` doesn't meaningfully work for
strings. We need to use another test function.

All of those functions accept a `:test` keyword parameter, that allows
you to change the test function:

~~~lisp
(find "foo" (list "test" "foo" "bar") :test #'equal)
;; => "foo"
~~~

We can also use `equalp` to ignore the string case:

~~~lisp
(find "FOO" (list "test" "foo" "bar") :test #'equalp)
;; => "foo"
~~~

You will find more examples about those built-in functions in [data-structures](https://lispcookbook.github.io/cl-cookbook/data-structures.html).

### `char-equal`

We have a special operator to compare characters:

> `char-equal` ignores alphabetic case and certain other attributes of characters


### strings and `string-equal`

`string-equal` has a specific function signature to compare strings
and substrings (you can specify the *start* and *end* boundaries for
the comparison), but be aware that it uses `char-equal`, so the
comparison is case-*in*sensitive. And it works with symbols.

~~~lisp
(string-equal :foo "foo") ;; => T
(string-equal :foo "FOO") ;; => T
~~~

This is its docstring:

```
STRING-EQUAL

This is a function in package COMMON-LISP

Signature
(string1 string2 &key (start1 0) end1 (start2 0) end2)

Given two strings (string1 and string2), and optional integers start1,
start2, end1 and end2, compares characters in string1 to characters in
string2 (using char-equal).
```

There are also the functions: ` string=; string/=; string<; string>; string<=; string>=; string-equal; string-not-equal; string-lessp; string-greaterp; string-not-greaterp; string-not-lessp`.

See our page [strings.html](https://lispcookbook.github.io/cl-cookbook/strings.html).

### Compare trees with `tree-equal`

Here you have it:

> `tree-equal` returns T if X and Y are isomorphic trees with identical leaves


## Compare function table: to compare against (this), use (that) function

```txt
To compare against...      Use...

Objects/Structs            EQ

NIL                        EQ (but the function NULL is more concise and probably cheaper)

T                          EQ (or just the value but then you don't care for the type)

Precise numbers            EQL

Floats                     =

Characters                 EQL or CHAR-EQUAL

Lists, Conses, Sequences   EQ (if you want the exact same object)
                           EQUAL (if you just care about elements)

Strings                    EQUAL (case-sensitive), EQUALP (case-insensitive)
                           STRING-EQUAL (if you throw symbols into the mix)

Trees (lists of lists)     TREE-EQUAL (with appropriate :TEST argument)
```

## How to compare your own objects AKA built-in functions are not object-oriented

Use `eq` to check that two objects are identical, that they are the same object in memory

If you want to compare your own objects with a logic of your own (for
example, two "person" objects will be considered equal if they have
the same name and surname), you can't specialize a built-in function
for this. Use your own `person=` or similar function, or use a library (see our links below).

While this can be seen as a limitation, using specialised functions instead of generic ones
has the advantage of being (much) faster.

As an example, let's consider the `person` class from the CLOS tutorial:

```lisp
(defclass person ()
  ((name
    :initarg :name
    :accessor name)))
```

Let's create two person objects, they have the same name but are two different objects:

```lisp
(defparameter *p1* (make-instance 'person :name "me"))
(defparameter *p2-same-name* (make-instance 'person :name "me"))
```

Use `eq` to compare two objects:

~~~lisp
(eq *p1* *p1*) ;; => T
(eq *p1* *p2-same-name*) ;; => NIL
~~~

We use our own `person=` method to compare different objects and decide when they are equal:

~~~lisp
(defmethod person= (p1 p2)
  (string= (name p1) (name p2)))

(person= *p1* *p2-same-name*)  ;; => T
~~~

If you really want to use `=` or `equal`, use a library, see below.

## Coalescing: the implications of `compile-file`

Let's take back our `(eql "a" "a")` example, that returns NIL.

We must precise that it return NIL on the REPL. The interpreter
doesn't see the two strings "a" as the same object in memory, so it
returns NIL.

However, a compiler might coalesce objects together.

If you compile a file with `compile-file`, the compiler might have
coalesced different objects together. It might have noticed that "a"
and "a" are two literal strings that are similar and it might have
saved them at the same memory location.

Thus our equality predicate can return T now.

Conclusion: use the right equality predicate.

This is also why we shouldn't modify variables that we defined with
literals, for example `'(1 2 3)` (using a quote) instead of `(list 1 2 3)` (using the `list` function).

Note that `compile` is not allowed to coalesce objects.


## Credits

- [CLtL2: Equality Predicates](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node74.html)
- the compare table: [Leslie P. Polzer on Stack-Overflow](https://stackoverflow.com/questions/547436/whats-the-difference-between-eq-eql-equal-and-equalp-in-common-lisp)

## See also

- [`equal` on the CL Community Spec](https://cl-community-spec.github.io/pages/equal.html)
- [equals](https://github.com/karlosz/equals/) - generic equality for Common Lisp.
- [generic-cl](https://github.com/alex-gutev/generic-cl/) - a generic function interface to CL built-ins.
  - we can use `=` or `<` on our own custom objects.
