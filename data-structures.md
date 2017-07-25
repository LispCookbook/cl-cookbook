---
title: Data structures
---

We hope to give here a clear reference of the common data
structures. To really learn the language, you should take the time to
read other resources. The following resources, which we relied upon,
also have many more detail:

- [Practical CL](http://gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html), by Peter Seibel
- [CL Recipes](http://weitz.de/cl-recipes/), by E. Weitz, full of explanations and tips,
- the
  [CL standard](http://cvberry.com/tech_writings/notes/common_lisp_standard_draft.html)
  with a nice TOC, functions reference, extensive descriptions, more
  examples and warnings (i.e: everything).
- Common Lisp quick reference: http://clqr.boundp.org/


## Lists

### Building lists. Cons cells, lists.

_A list is also a sequence, so we can use the functions shown below._

The list basic element is the cons sell. We build lists by assembling
cons cells.

~~~lisp
(cons 1 2)
;; => (1 . 2) ;; representation with a point, a dotted pair.
~~~

It looks like this:

```
[o|o]--- 2
 |
 1
```

If the `cdr` of the first cell is another cons cell, and if the `cdr` of
this last one is `nil`, we build a list:

~~~lisp
(cons 1 (cons 2 nil))
;; => (1 2)
~~~

It looks like this:

```
[o|o]---[o|/]
 |       |
 1       2
```
(ascii art by [draw-cons-tree](https://github.com/cbaggers/draw-cons-tree)).

See that the representation is not a dotted pair ? The lisp printer
understands the convention.

Finally we can simply use `list`:

~~~lisp
(list 1 2)
;; => (1 2)
~~~

and its quote shortcut:

~~~lisp
'(1 2)
;; => (1 2)
~~~


### car/cdr or first/rest (and second... to tenth)

~~~lisp
(car (cons 1 2)) ;; => 1
(cdr (cons 1 2)) ;; => 2
(first (cons 1 2)) ;; => 1
(first '(1 2 3)) ;; => 1
(rest '(1 2 3)) ;; => (2 3)
~~~

We can assign *any* new value with `setf`.

### last, butlast, nbutlast (&optional n)

return the last cons cell in a list (or the nth last cons cells).

~~~lisp
(last '(1 2 3))
;; => (3)
(car (last '(1 2 3)) )
;; => 3
(butlast '(1 2 3))
;; => (1 2)
~~~


### reverse, nreverse

`reverse` returns a new list.

`nreverse` is destructive. The N stands for **non-consing**, meaning it
doesn't need to allocate any new cons cells. It is equivalent to:

~~~lisp
(setf *list* (reverse *list*))
~~~

### append

`append` takes any number of list arguments and returns a new list
containing the elements of all its arguments:

~~~lisp
(append (list 1 2) (list 3 4))
;; => (1 2 3 4)
~~~

The new list shares some cons cells with the `(3 4)`:

http://gigamonkeys.com/book/figures/after-append.png

__Note__: [cl21](cl21.htm)'s `append` is generic (for strings, lists, vectors and
its abstract-sequence).

`nconc` is the recycling equivalent.

### push

`push` adds a given element to the head of a given list.

~~~lisp
(defparameter mylist '(1 2 3))
(push 0 mylist)
;; => (0 1 2 3)
~~~


There is no built-in function to **add to the end of a list**. It is a
more costly operation (have to traverse the whole list). So if you
need to do this: either consider using another data structure, either
just `reverse` your list when needed.

### pop

a destructive operation.

### nthcdr (index, list)

Use this if `first`, `second` and the rest up to `tenth` are not
enough.

### car/cdr and composites (cadr, caadr…) - accessing lists inside lists

They make sense when applied to lists containing other lists.

~~~lisp
(caar (list 1 2 3))                  ==> error
(caar (list (list 1 2) 3))           ==> 1
(cadr (list (list 1 2) (list 3 4)))  ==> (3 4)
(caadr (list (list 1 2) (list 3 4))) ==> 3
~~~

### destructuring-bind (parameter*, list)

It binds the parameter values to the list elements. We can destructure
trees, plits and even provide defaults.

Simple matching:

~~~lisp
(destructuring-bind (x y z) (list 1 2 3)
  (list :x x :y y :z z))
;; => (:X 1 :Y 2 :Z 3)
~~~

Matching inside sublists:

~~~lisp
(destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z))
;; => (:X 1 :Y1 2 :Y2 20 :Z 3)
~~~

The parameter list can use the usual `&optional`, `&rest` and `&key`
parameters.

~~~lisp
(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
  (list :x x :y1 y1 :y2 y2 :z z))
;; => (:X 1 :Y1 2 :Y2 NIL :Z 3)
~~~

~~~lisp
(destructuring-bind (&key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z))
;; => (:X 3 :Y 2 :Z 1)
~~~

The `&whole` parameter is bound to the whole list. It must be the
first one and others can follow.

~~~lisp
(destructuring-bind (&whole whole-list &key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z :whole whole-list))
;; => (:X 3 :Y 2 :Z 1 :WHOLE-LIST (:Z 1 :Y 2 :X 3))
~~~

Destructuring a plist, giving defaults:

(example from Common Lisp Recipes, by E. Weitz, Apress, 2016)

~~~lisp
(destructuring-bind (&key a (b :not-found) c
                     &allow-other-keys)
    ’(:c 23 :d "D" :a #\A :foo :whatever)
  (list a b c))
;; => (#\A :NOT-FOUND 23)
~~~

If this gives you the will to do pattern matching, see
[pattern matching](pattern_matching.html).


### Predicates: null, listp

`null` is equivalent to `not`, but considered better style.

`listp` tests wether an object is a cons cell or nil.

and sequences' predicates.


### ldiff, tailp, list*, make-list, fill, revappend, nreconc, consp, atom

~~~lisp
(make-list 3 :initial-element "ta")
;; => ("ta" "ta" "ta")
~~~

~~~lisp
(make-list 3)
;; => (NIL NIL NIL)
(fill * "hello")
;; => ("hello" "hello" "hello")
~~~

## Sequences

**lists** and **vectors** (and thus **strings**) are sequences.

_Note_: see also the [strings](strings.html) page.

Many of the sequence functions take keyword arguments. All keyword
arguments are optional and, if specified, may appear in any order.

Pay attention to the `:test` argument. It defaults to `eql` (for
strings, use `:equal`).

The `:key` argument should be passed either nil, or a function of one
argument. This key function is used as a filter through which the
elements of the sequence are seen. For instance, this:

~~~lisp
(find x y :key 'car)
~~~

is similar to `(assoc* x y)`: It searches for an element of the list
whose car equals x, rather than for an element which equals x
itself. If `:key` is omitted or nil, the filter is effectively the
identity function.

Example with an alist (see definition below):

~~~lisp
(defparameter my-alist (list (cons 'foo "foo")
                             (cons 'bar "bar")))
;; => ((FOO . "foo") (BAR . "bar"))
(find 'bar my-alist)
;; => NIL
(find 'bar my-alist :key 'car)
;; => (BAR . "bar")
~~~

For more, use a `lambda` that takes one parameter.

~~~lisp
(find 'bar my-alist :key (lambda (it) (car it)))
~~~

_Note_: and [cl21](cl21.html#shorter-lambda) has short lambdas:

~~~lisp
(find 'bar my-alist :key ^(car %))
(find 'bar my-alist :key (lm (it) (car it)))
~~~


### Predicates: every, some,…

`every, notevery (test, sequence)`: return nil or t, respectively, as
soon as one test on any set of the corresponding elements of sequences
returns nil.

~~~lisp
(defparameter foo '(1 2 3))
(every #'evenp foo)
;; => NIL
(some #'evenp foo)
;; => T
~~~

with a list of strings:

~~~lisp
(defparameter str '("foo" "bar" "team"))
(every #'stringp str)
;; => T
(some #'(lambda (it) (= 3 (length it))) str)
;; => T
(some ^(= 3 (length %)) str) ;; in CL21
;; => T
~~~

`some`, `notany` *(test, sequence)*: return either the value of the test, or nil.

`mismatch` *(sequence-a, sequence-b)*: Return position in sequence-a where
sequence-a and sequence-b begin to mismatch. Return NIL if they match
entirely. Other parameters: `:from-end bool`, `:start1`, `:start2` and
their `:end[1,2]`.

### Functions

#### length (sequence)

#### member (elt, sequence)

#### elt (sequence, index)

beware, here the sequence comes first.

#### count (foo sequence)

Return the number of elements in sequence that match *foo*.

Additional paramaters: `:from-end`, `:start`, `:end`.

See also `count-if`, `count-not` *(test-function sequence)*.

#### subseq (sequence start, [end])

It is "setf"able, but only works if the new sequence has the same
length of the one to replace.

#### sort, stable-sort (sequence, test [, key function])

#### find, position (foo, sequence)

also `find-if`, `find-if-not`, `position-if`, `position-if-not` *(test
sequence)*. See `:key` and `:test` parameters.

#### search (sequence-a, sequence-b)

Search sequence-b for a subsequence matching sequence-a. Return
position in sequence-b, or NIL. Has the `from-end`, `end1/2` and others
parameters.

#### substitute, nsubstitute[if,if-not]

#### sort, stable-sort, merge

#### replace (sequence-a, sequence-b)

Replace elements of sequence-a with elements of
sequence-b.

#### remove, delete (foo sequence)

Make a copy of sequence without elements matching foo. Has
`:start/end`, `:key` and `:count` parameters.

`delete` is the recycling version of `remove`.

~~~lisp
(remove "foo" '("foo" "bar" "foo") :test 'equal)
;; => ("bar")
~~~

see also `remove-if[-not]` below.

### mapping (map, mapcar, remove-if[-not],...)

If you're used to map and filter in other languages, you probably want
`mapcar`. But it only works on lists, so to iterate on vectors (and
produce either a vector or a list, use `(map 'list function vector)`.

mapcar also accepts multiple lists with `&rest more-seqs`.  The
mapping stops as soon as the shortest sequence runs out.

_Note: cl21's `map` is a generic `mapcar` for lists and vectors._

`map` takes the output-type as first argument (`'list`, `'vector` or
`'string`):

~~~lisp
(defparameter foo '(1 2 3))
(map 'list (lambda (it) (* 10 it)) foo)
~~~

`reduce` *(function, sequence)*. Special parameter: `:initial-value`.

~~~lisp
(reduce '- '(1 2 3 4))
;; => -8
(reduce '- '(1 2 3 4) :initial-value 100)
;; => 90
~~~

**Filter** is here called `remove-if-not`.

### Creating lists with variables

That's one use of the `backquote`:

~~~lisp
(defparameter *var* "bar")
;; First try:
'("foo" *var* "baz") ;; no backquote
;; => ("foo" *VAR* "baz") ;; nope
~~~

Second try, with backquote interpolation:

~~~lisp
`("foo" ,*var* "baz")     ;; backquote, comma
;; => ("foo" "bar" "baz") ;; good
~~~

The backquote first warns we'll do interpolation, the comma introduces
the value of the variable.

If our variable is a list:

~~~lisp
(defparameter *var* '("bar" "baz"))
;; First try:
`("foo" ,*var*)
;; => ("foo" ("bar" "baz")) ;; nested list
`("foo" ,@*var*)            ;; backquote, comma-@ to
;; => ("foo" "bar" "baz")
~~~

E. Weitz warns that "objects generated this way will very likely share
structure (see Recipe 2-7)".


### Comparing lists

We can use sets functions.

## Set

`intersection`

What elements are both in list-a and list-b ?

~~~lisp
(defparameter list-a '(0 1 2 3))
(defparameter list-b '(0 2 4))
(intersection list-a list-b)
;; => (2 0)
~~~

`set-difference`

Remove the elements of list-b from list-a:

~~~lisp
(set-difference list-a list-b)
;; => (3 1)
(set-difference list-b list-a)
;; => (4)
~~~

`union`

join the two lists:

~~~lisp
(union list-a list-b)
;; => (3 1 0 2 4) ;; order can be different in your lisp
~~~

`set-exclusive-or`

Remove the elements that are in both lists:

~~~lisp
(set-exclusive-or list-a list-b)
;; => (4 3 1)
~~~

and their recycling "n" counterpart (`nintersection`,…).

## Fset - immutable data structure

You may want to have a look at this library:

https://common-lisp.net/project/fset/Site/FSet-Tutorial.html


## Arrays and vectors

**Arrays** have constant-time access characteristics.

They can be fixed or adjustable. A *simple array* is neither displaced
(using `:displaced-to`, to point to another array) nor adjustable
(`:adjust-array`), nor does it have a fill pointer (`fill-pointer`,
that moves when we add or remove elements).

A **vector** is an array with rank 1 (of one dimension). It is also a
*sequence* (see above).

A *simple vector* is a simple array that is also not specialized (it
doesn't use `:element-type` to set the types of the elements).


### Create an array, one or many dimensions

`make-array` *(sizes-list :adjustable bool)*

`adjust-array` *(array, sizes-list, :element-type, :initial-element)*

### Access: aref (array i [j …])

`aref` *(array i j k …)* or `row-major-aref` *(array i)* equivalent to
`(aref i i i …)`.

The result is `setf`able.

~~~lisp
(defparameter myarray (make-array '(2 2 2) :initial-element 1))
myarray
;; => #3A(((1 1) (1 1)) ((1 1) (1 1)))
(aref myarray 0 0 0)
;; => 1
(setf (aref myarray 0 0 0) 9)
;; => 9
(row-major-aref myarray 0)
;; => 9
~~~


### Sizes

`array-total-size` *(array)*: how many elements will fit in the array ?

`array-dimensions` *(array)*: list containing the length of the array's dimensions.

`array-dimension` *(array i)*: length of the *i*th dimension.

`array-rank` number of dimensions of the array.

~~~lisp
(defparameter myarray (make-array '(2 2 2)))
;; => MYARRAY
myarray
;; => #3A(((0 0) (0 0)) ((0 0) (0 0)))
(array-rank myarray)
;; => 3
(array-dimensions myarray)
;; => (2 2 2)
(array-dimension myarray 0)
;; => 2
(array-total-size myarray)
;; => 8
~~~


### Vectors

Create with `vector` or the reader macro `#()`. It returns a _simple
vector._

~~~lisp
(vector 1 2 3)
;; => #(1 2 3)
#(1 2 3)
;; => #(1 2 3)
~~~


`vector-push` *(foo vector)*: replace the vector element pointed to by
the fill pointer by foo. Can be destructive.

`vector-push-extend` *(foo vector [extension-num])*t

`vector-pop` *(vector)*: return the element of vector its fill pointer
points to.

`fill-pointer` *(vector)*. `setf`able.

and see also the *sequence* functions.

### Transforming a vector to a list.

If you're mapping over it, see the `map` function whose first parameter
is the result type.

Or use `(coerce vector 'list)`.

## Hash Table, plist, alist

Hash tables map keys to values (dictionaries in other languages).

Alists and plists are lighter ways to do so, and as their name suggest
they're built on lists, so (as usual see E. Weitz for more):

- alists can be ordered
- we can push a new cons cell with a key that already exists, remove
  that one in front and we have a stack
- they have a human-readable printed representation
- they can be easily (de)serialized
- because of RASSOC, keys and values in alists are essentially
interchangeable; whereas in hash tables, keys and values play very
different roles,

### Hash tables: make-hash-table, gethash, remhash, maphash, #H

We create one with `make-hash-table` and access a key with
`gethash`. Keys are `setf`able:

~~~lisp
(defparameter *hash* (make-hash-table))
(setf (gethash 'foo *hash*) "hello-foo")
;; => "hello-foo"
(gethash 'foo *hash*)
;; => "hello-foo"
;;     T          ;; two values are returned.
~~~

Accessing a key that doesn't exist, with a default:

~~~lisp
(gethash 'bar *hash* "default-bar")
;; => "default-bar"
;;     NIL
~~~

To remove an element: `remhash`.

To map over a hash-table: `maphash`.

_Note: [cl21](cl21.html#hash-table) provides the `#H` reader macro to create hash tables (and more):_

~~~lisp
#H(:name "Eitaro Fukamachi" :living "Japan")
;=> #H(:LIVING "Japan" :NAME "Eitaro Fukamachi")
~~~

Number of keys: `hash-table-count`.

There are more, see the specs !

### alist

An association list is a list of cons cells.

This simple example:

~~~lisp
(defparameter my-alist (list (cons 'foo "foo")
                             (cons 'bar "bar")))
;; => ((FOO . "foo") (BAR . "bar"))
~~~

looks like this:

```
[o|o]---[o|/]
 |       |
 |      [o|o]---"bar"
 |       |
 |      BAR
 |
[o|o]---"foo"
 |
FOO
```

The constructor `pairlis` associates a list of keys and a list of values:

~~~lisp
(pairlis '(:foo :bar)
         '("foo" "bar"))
;; => ((:BAR . "bar") (:FOO . "foo"))
~~~

To get a key, we have `assoc` (use `:test 'equal` when your keys are
strings, as usual). It returns the whole cons cell, so you may want to
use `cdr` or `second` to get the value. There is `assoc-if`, and
`rassoc` to get a cons cell by its value.

To add a key, we `push` another cons cell:

~~~lisp
(push (cons 'team "team") my-alist)
;; => ((TEAM . "team") (FOO . "foo") (BAR . "bar"))
~~~

We can use `pop` and other functions that operate on lists, like `remove`:

~~~lisp
(remove :team my-alist)
;; => ((:TEAM . "team") (FOO . "foo") (BAR . "bar")) ;; didn't remove anything
(remove :team my-alist :key 'car)
;; => ((FOO . "foo") (BAR . "bar")) ;; returns a copy
~~~

Remove only one element with `:count`:

~~~lisp
(push (cons 'bar "bar2") my-alist)
;; => ((BAR . "bar2") (TEAM . "team") (FOO . "foo") (BAR . "bar")) ;; twice the 'bar key
(remove 'bar my-alist :key 'car :count 1)
;; => ((TEAM . "team") (FOO . "foo") (BAR . "bar"))
;; because otherwise:
(remove 'bar my-alist :key 'car)
;; => ((TEAM . "team") (FOO . "foo")) ;; no more 'bar
~~~


### plist

A property list is simply a list that alternates a key, a value, and
so on, where its keys are symbols (we can not set its `:test`). More
precisely, it first has a cons cell whose `car` is the key, whose
`cdr` points to the following cons cell whose `car` is the
value.

For example this plist:

~~~lisp
(defparameter my-plist (list 'foo "foo" 'bar "bar"))
~~~

looks like this:

```
[o|o]---[o|o]---[o|o]---[o|/]
 |       |       |       |
FOO     "foo"   BAR     "bar"

```

We access an element with `getf (list elt)` (it returns the value)
(the list comes as first element),

we remove an element with `remf`.

~~~lisp
(defparameter my-plist (list 'foo "foo" 'bar "bar"))
;; => (FOO "foo" BAR "bar")
(setf (getf my-plist 'foo) "foo!!!")
;; => "foo!!!"
~~~


## Tree

`tree-equal`, `copy-tree`. They descend recursively into the car and
the cdr of the cons cells they visit.

### Sycamore - purely functional weight-balanced binary trees

[https://github.com/ndantam/sycamore](https://github.com/ndantam/sycamore)

Features:

* Fast, purely functional weight-balanced binary trees.
  * Leaf nodes are simple-vectors, greatly reducing tree height.
* Interfaces for tree Sets and Maps (dictionaries).
* [Ropes](http://en.wikipedia.org/wiki/Rope_(data_structure))
* Purely functional [pairing heaps](http://en.wikipedia.org/wiki/Pairing_heap)
* Purely functional amortized queue.

See more in other resources !
