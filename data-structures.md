---
title: Data structures
---

We hope to give here a clear reference of the common data
structures. To really learn the language, you should take the time to
read other resources. The following resources, which we relied upon,
also have many more details:

- [Practical CL](http://gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html), by Peter Seibel
- [CL Recipes](http://weitz.de/cl-recipes/), by E. Weitz, full of explanations and tips,
- the
  [CL standard](http://cberr.us/tech_writings/notes/common_lisp_standard_draft.html)
  with a nice TOC, functions reference, extensive descriptions, more
  examples and warnings (i.e: everything). [PDF mirror](https://gitlab.com/vancan1ty/clstandard_build/-/blob/master/cl-ansi-standard-draft-w-sidebar.pdf)
- a [Common Lisp quick reference](http://clqr.boundp.org/)

Don't miss the appendix and when you need more data structures, have a
look at the
[awesome-cl](https://github.com/CodyReichert/awesome-cl#data-structures)
list and [Quickdocs](https://quickdocs.org/-/search?q=data%20structure).

## Lists

### Building lists. Cons cells, lists.

_A list is also a sequence, so we can use the functions shown below._

The list basic element is the cons cell. We build lists by assembling
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

See that the representation is not a dotted pair ? The Lisp printer
understands the convention.

Finally we can simply build a literal list with `list`:

~~~lisp
(list 1 2)
;; => (1 2)
~~~

or by calling quote:

~~~lisp
'(1 2)
;; => (1 2)
~~~

which is shorthand notation for the function call `(quote (1 2))`.

### Circular lists

A cons cell car or cdr can refer to other objects, including itself or
other cells in the same list. They can therefore be used to define
self-referential structures such as circular lists.

Before working with circular lists, tell the printer to recognise them
and not try to print the whole list by setting
[\*print-circle\*](http://clhs.lisp.se/Body/v_pr_cir.htm)
to `T`:

~~~lisp
(setf *print-circle* t)
~~~

A function which modifies a list, so that the last `cdr` points to the
start of the list is:

~~~lisp
(defun circular! (items)
  "Modifies the last cdr of list ITEMS, returning a circular list"
  (setf (cdr (last items)) items))

(circular! (list 1 2 3))
;; => #1=(1 2 3 . #1#)

(fifth (circular! (list 1 2 3)))
;; => 2
~~~

The [list-length](http://www.lispworks.com/documentation/HyperSpec/Body/f_list_l.htm#list-length)
function recognises circular lists, returning `nil`.

The reader can also create circular lists, using
[Sharpsign Equal-Sign](http://www.lispworks.com/documentation/HyperSpec/Body/02_dho.htm)
notation. An object (like a list) can be prefixed with `#n=` where `n`
is an unsigned decimal integer (one or more digits). The
label `#n#` can be used to refer to the object later in the
expression:

~~~lisp
'#42=(1 2 3 . #42#)
;; => #1=(1 2 3 . #1#)
~~~

Note that the label given to the reader (`n=42`) is discarded after
reading, and the printer defines a new label (`n=1`).

Further reading

* [Let over Lambda](https://letoverlambda.com/index.cl/guest/chap4.html#sec_5) section on cyclic expressions


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
(car (last '(1 2 3)) ) ;; or (first (last …))
;; => 3
(butlast '(1 2 3))
;; => (1 2)
~~~

In [Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html#Conses), `lastcar` is equivalent of `(first (last …))`:

~~~lisp
(alexandria:lastcar '(1 2 3))
;; => 3
~~~


### reverse, nreverse

`reverse` and `nreverse` return a new sequence.

`nreverse` is destructive. The N stands for **non-consing**, meaning
it doesn't need to allocate any new cons cells. It *might* (but in
practice, does) reuse and modify the original sequence:

~~~lisp
(defparameter mylist '(1 2 3))
;; => (1 2 3)
(reverse mylist)
;; => (3 2 1)
mylist
;; => (1 2 3)
(nreverse mylist)
;; => (3 2 1)
mylist
;; => (1) in SBCL but implementation dependent.
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

`nconc` is the recycling equivalent.

### push (item, place)

`push` prepends *item* to the list that is stored in *place*, stores
the resulting list in *place*, and returns the list.

~~~lisp
(defparameter mylist '(1 2 3))
(push 0 mylist)
;; => (0 1 2 3)
~~~

~~~lisp
(defparameter x ’(a (b c) d))
;; => (A (B C) D)
(push 5 (cadr x))
;; => (5 B C)
x
;; => (A (5 B C) D)
~~~

`push` is equivalent to `(setf place (cons item place ))` except that
the subforms of *place* are evaluated only once, and *item* is evaluated
before *place*.

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
trees, plists and even provide defaults.

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

`listp` tests whether an object is a cons cell or nil.

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

### member (elt, list)

Returns the tail of `list` beginning with the first element satisfying `eql`ity.

Accepts `:test`, `:test-not`, `:key` (functions or symbols).

~~~lisp
(member 2 '(1 2 3))
;; (2 3)
~~~

### Replacing objects in a tree: subst, sublis

[subst](http://www.lispworks.com/documentation/HyperSpec/Body/f_substc.htm) and
`subst-if` search and replace occurences of an element
or subexpression in a tree (when it satisfies the optional `test`):

~~~lisp
(subst 'one 1 '(1 2 3))
;; => (ONE 2 3)

(subst  '(1 . one) '(1 . 1) '((1 . 1) (2 . 2) (3 . 3)) :test #'equal)
;; ((1 . ONE) (2 . 2) (3 . 3))
~~~

[sublis](http://www.lispworks.com/documentation/HyperSpec/Body/f_sublis.htm)
allows to replace many objects at once. It substitutes the objects
given in `alist` and found in `tree` with their new values given in
the alist:

~~~lisp
(sublis '((x . 10) (y . 20))
        '(* x (+ x y) (* y y)))
;; (* 10 (+ 10 20) (* 20 20))
~~~

`sublis` accepts the `:test` and `:key` arguments. `:test` is a
function that takes two arguments, the key and the subtree.

~~~lisp
(sublis '((t . "foo"))
        '("one" 2 ("three" (4 5)))
        :key #'stringp)
;; ("foo" 2 ("foo" (4 5)))
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
~~~

`some`, `notany` *(test, sequence)*: return either the value of the test, or nil.


### Functions

See also sequence functions defined in
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html#Sequences):
`starts-with`, `ends-with`, `ends-with-subseq`, `length=`, `emptyp`,…

#### length (sequence)

#### elt (sequence, index) - find by index

beware, here the sequence comes first.

#### count (foo sequence)

Return the number of elements in sequence that match *foo*.

Additional paramaters: `:from-end`, `:start`, `:end`.

See also `count-if`, `count-not` *(test-function sequence)*.

#### subseq (sequence start, [end])

~~~lisp
(subseq (list 1 2 3) 0)
;; (1 2 3)
(subseq (list 1 2 3) 1 2)
;; (2)
~~~

However, watch out if the `end` is larger than the list:

~~~lisp
(subseq (list 1 2 3) 0 99)
;; => Error: the bounding indices 0 and 99 are bad for a sequence of length 3.
~~~

To this end, use `alexandria-2:subseq*`:

~~~lisp
(alexandria-2:subseq* (list 1 2 3) 0 99)
;; (1 2 3)
~~~

`subseq` is "setf"able, but only works if the new sequence has the same
length of the one to replace.


#### sort, stable-sort (sequence, test [, key function])

These sort functions are destructive, so one may prefer to copy the sequence with `copy-seq` before sorting:

~~~lisp
(sort (copy-seq seq) :test #'string<)
~~~

Unlike `sort`, `stable-sort` guarantees to keep the order of the argument.
In theory, the result of this:

~~~lisp
(sort '((1 :a) (1 :b)) #'< :key #'first)
~~~

could be either `((1 :A) (1 :B))`, either `((1 :B) (1 :A))`. On my tests, the order is preserved, but the standard does not guarantee it.


#### find, position (foo, sequence) - get index

also `find-if`, `find-if-not`, `position-if`, `position-if-not` *(test
sequence)*. See `:key` and `:test` parameters.

~~~lisp
(find 20 '(10 20 30))
;; 20
(position 20 '(10 20 30))
;; 1
~~~

#### search and mismatch (sequence-a, sequence-b)

`search` searches in sequence-b for a subsequence that matches sequence-a. It returns the
*position* in sequence-b, or NIL. It has the `from-end`, `end1`, `end2` and the usual `test` and `key`
parameters.

~~~lisp
(search '(20 30) '(10 20 30 40))
;; 1
(search '("b" "c") '("a" "b" "c"))
;; NIL
(search '("b" "c") '("a" "b" "c") :test #'equal)
;; 1
(search "bc" "abc")
;; 1
~~~

`mismatch` returns the position where the two sequences start to differ:

~~~lisp
(mismatch '(10 20 99) '(10 20 30))
;; 2
(mismatch "hellolisper" "helloworld")
;; 5
(mismatch "same" "same")
;; NIL
(mismatch "foo" "bar")
;; 0
~~~

#### substitute, nsubstitute[if,if-not]

Return a sequence of the same kind as `sequence` with the same elements,
except that all elements equal to `old` are replaced with `new`.

~~~lisp
(substitute #\o #\x "hellx") ;; => "hello"
(substitute :a :x '(:a :x :x)) ;; => (:A :A :A)
(substitute "a" "x" '("a" "x" "x") :test #'string=) ;; => ("a" "a" "a")
~~~

#### sort, stable-sort, merge

(see above)

#### replace (sequence-a, sequence-b, &key start1, end1)

Destructively replace elements of sequence-a with elements of
sequence-b.

The full signature is:

~~~lisp
(replace sequence1 sequence2 &rest args &key (start1 0) (end1 nil) (start2 0)
 (end2 nil))
~~~

Elements are copied to the subseqeuence bounded by START1 and END1,
from the subsequence bounded by START2 and END2. If these subsequences
are not of the same length, then the shorter length determines how
many elements are copied.

~~~lisp
(replace "xxx" "foo")
"foo"

(replace "xxx" "foo" :start1 1)
"xfo"

(replace "xxx" "foo" :start1 1 :start2 1)
"xoo"

(replace "xxx" "foo" :start1 1 :start2 1 :end2 2)
"xox"
~~~


#### remove, delete (foo sequence)

Make a copy of sequence without elements matching foo. Has
`:start/end`, `:key` and `:count` parameters.

`delete` is the recycling version of `remove`.

~~~lisp
(remove "foo" '("foo" "bar" "foo") :test 'equal)
;; => ("bar")
~~~

see also `remove-if[-not]` below.

#### remove-duplicates, delete-duplicates (sequence)

[remove-duplicates](http://clhs.lisp.se/Body/f_rm_dup.htm) returns a
new sequence with uniq elements. `delete-duplicates` may modify the
original sequence.

`remove-duplicates` accepts the following, usual arguments: `from-end
test test-not start end key`.

~~~lisp
(remove-duplicates '(:foo :foo :bar))
(:FOO :BAR)

(remove-duplicates '("foo" "foo" "bar"))
("foo" "foo" "bar")

(remove-duplicates '("foo" "foo" "bar") :test #'string-equal)
("foo" "bar")
~~~


### mapping (map, mapcar, remove-if[-not],...)

If you're used to map and filter in other languages, you probably want
`mapcar`. But it only works on lists, so to iterate on vectors (and
produce either a vector or a list, use `(map 'list function vector)`.

mapcar also accepts multiple lists with `&rest more-seqs`.  The
mapping stops as soon as the shortest sequence runs out.

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

### Flatten a list (Alexandria)

With
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html),
we have the `flatten` function.


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

We show below how to use set operations on lists.

A set doesn't contain twice the same element and is unordered.

Most of these functions have recycling (modifying) counterparts, starting with "n": `nintersection`,… They all accept the usual `:key` and `:test` arguments, so use the test `#'string=` or `#'equal` if you are working with strings.

For more, see functions in
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html#Conses):
`setp`, `set-equal`,… and the FSet library, shown in the next section.

### `intersection` of lists

What elements are both in list-a and list-b ?

~~~lisp
(defparameter list-a '(0 1 2 3))
(defparameter list-b '(0 2 4))
(intersection list-a list-b)
;; => (2 0)
~~~

### Remove the elements of list-b from list-a (`set-difference`)

~~~lisp
(set-difference list-a list-b)
;; => (3 1)
(set-difference list-b list-a)
;; => (4)
~~~

### Join two lists with uniq elements (`union`)

~~~lisp
(union list-a list-b)
;; => (3 1 0 2 4) ;; order can be different in your lisp
~~~

### Remove elements that are in both lists (`set-exclusive-or`)

~~~lisp
(set-exclusive-or list-a list-b)
;; => (4 3 1)
~~~

### Add an element to a set (`adjoin`)

~~~lisp
(adjoin 3 list-a)
;; => (0 1 2 3) <-- nothing was changed, 3 was already there.

(adjoin 5 list-a)
;; => (5 0 1 2 3)

list-a
;; => (0 1 2 3)
~~~

### Check if this is a subset (`subsetp`)

~~~lisp
(subsetp '(1 2 3) list-a)
;; => T

(subsetp '(1 1 1) list-a)
;; => T

(subsetp '(3 2 1) list-a)
;; => T

(subsetp '(0 3) list-a)
;; => T
~~~

## Fset - immutable data structure

You may want to have a look at the
[FSet](https://common-lisp.net/project/fset/Site/FSet-Tutorial.html )
library (in Quicklisp).


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

## Hash Table

Hash Tables are a powerful data structure, associating keys with
values in a very efficient way. Hash Tables are often preferred over
association lists whenever performance is an issue, but they introduce
a little overhead that makes assoc lists better if there are only a
few key-value pairs to maintain.

Alists can be used sometimes differently though:

- they can be ordered
- we can push cons cells that have the same key, remove the one in
  front and we have a stack
- they have a human-readable printed representation
- they can be easily (de)serialized
- because of RASSOC, keys and values in alists are essentially
interchangeable; whereas in hash tables, keys and values play very
different roles (as usual, see CL Recipes for more).


<a name="create"></a>

### Creating a Hash Table

Hash Tables are created using the function
[`make-hash-table`](http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_has.htm). It
has no required argument. Its most used optional keyword argument is
`:test`, specifying the function used to test the equality of keys.

<div class="info-box info">
<strong>Note:</strong> see shorter notations in the <a href="https://github.com/ruricolist/serapeum/">Serapeum</a> or <a href="https://github.com/vseloved/rutils">Rutils</a> libraries. For example, Serapeum has <code>dict</code>, and Rutils a <code>#h</code> reader macro.
</div>

<a name="add"></a>

### Adding an Element to a Hash Table

If you want to add an element to a hash table, you can use `gethash`,
the function to retrieve elements from the hash table, in conjunction
with
[`setf`](http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm).

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (setf (gethash 'one-entry *my-hash*) "one")
"one"
CL-USER> (setf (gethash 'another-entry *my-hash*) 2/4)
1/2
CL-USER> (gethash 'one-entry *my-hash*)
"one"
T
CL-USER> (gethash 'another-entry *my-hash*)
1/2
T
~~~

With Serapeum's `dict`, we can create a hash-table and add elements to
it in one go:

~~~lisp
(defparameter *my-hash* (dict :one-entry "one" :another-entry 2/4))
;; =>
 (dict
  :ONE-ENTRY "one"
  :ANOTHER-ENTRY 1/2
 )
~~~

<a name="get"></a>

### Getting a value from a Hash Table

The function
[`gethash`](http://www.lispworks.com/documentation/HyperSpec/Body/f_gethas.htm)
takes two required arguments: a key and a hash table. It returns two
values: the value corresponding to the key in the hash table (or `nil`
if not found), and a boolean indicating whether the key was found in
the table. That second value is necessary since `nil` is a valid value
in a key-value pair, so getting `nil` as first value from `gethash`
does not necessarily mean that the key was not found in the table.

#### Getting a key that does not exist with a default value

`gethash` has an optional third argument:

~~~lisp
(gethash 'bar *my-hash* "default-bar")
;; => "default-bar"
;;     NIL
~~~

#### Getting all keys or all values of a hash table

The
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html)
library (in Quicklisp) has the functions `hash-table-keys` and
`hash-table-values` for that.

~~~lisp
(ql:quickload "alexandria")
;; […]
(alexandria:hash-table-keys *my-hash*)
;; => (BAR)
~~~


<a name="test"></a>

### Testing for the Presence of a Key in a Hash Table

The first value returned by `gethash` is the object in the hash table
that's associated with the key you provided as an argument to
`gethash` or `nil` if no value exists for this key. This value can act
as a
[generalized boolean](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#generalized_boolean">generalized
boolean) if you want to test for the presence of keys.

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (setf (gethash 'one-entry *my-hash*) "one")
"one"
CL-USER> (if (gethash 'one-entry *my-hash*)
           "Key exists"
           "Key does not exist")
"Key exists"
CL-USER> (if (gethash 'another-entry *my-hash*)
           "Key exists"
           "Key does not exist")
"Key does not exist"
~~~

But note that this does _not_ work if `nil` is amongst the values that
you want to store in the hash.

~~~lisp
CL-USER> (setf (gethash 'another-entry *my-hash*) nil)
NIL
CL-USER> (if (gethash 'another-entry *my-hash*)
           "Key exists"
           "Key does not exist")
"Key does not exist"
~~~

In this case you'll have to check the _second_ return value of `gethash` which will always return `nil` if no value is found and T otherwise.

~~~lisp
CL-USER> (if (nth-value 1 (gethash 'another-entry *my-hash*))
           "Key exists"
           "Key does not exist")
"Key exists"
CL-USER> (if (nth-value 1 (gethash 'no-entry *my-hash*))
           "Key exists"
           "Key does not exist")
"Key does not exist"
~~~


<a name="del"></a>

### Deleting from a Hash Table

Use
[`remhash`](http://www.lispworks.com/documentation/HyperSpec/Body/f_remhas.htm)
to delete a hash entry. Both the key and its associated value will be
removed from the hash table. `remhash` returns T if there was such an
entry, `nil` otherwise.

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (setf (gethash 'first-key *my-hash*) 'one)
ONE
CL-USER> (gethash 'first-key *my-hash*)
ONE
T
CL-USER> (remhash 'first-key *my-hash*)
T
CL-USER> (gethash 'first-key *my-hash*)
NIL
NIL
CL-USER> (gethash 'no-entry *my-hash*)
NIL
NIL
CL-USER> (remhash 'no-entry *my-hash*)
NIL
CL-USER> (gethash 'no-entry *my-hash*)
NIL
NIL
~~~


<a name="del-tab"></a>

### Deleting a Hash Table

Use
[`clrhash`](http://www.lispworks.com/documentation/HyperSpec/Body/f_clrhas.htm)
to delete a hash table. This will remove all of the data from the hash table and return the deleted table. 

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (setf (gethash 'first-key *my-hash*) 'one)
ONE
CL-USER> (setf (gethash 'second-key *my-hash*) 'two)
TWO
CL-USER> *my-hash*
#<hash-table :TEST eql :COUNT 2 {10097BF4E3}>
CL-USER> (clrhash *my-hash*)
#<hash-table :TEST eql :COUNT 0 {10097BF4E3}>
CL-USER> (gethash 'first-key *my-hash*)
NIL
NIL
CL-USER> (gethash 'second-key *my-hash*)
NIL
NIL
~~~


<a name="traverse"></a>

### Traversing a Hash Table

If you want to perform an action on each entry (i.e., each key-value
pair) in a hash table, you have several options:

You can use
[`maphash`](http://www.lispworks.com/documentation/HyperSpec/Body/f_maphas.htm)
which iterates over all entries in the hash table. Its first argument
must be a function which accepts _two_ arguments, the key and the
value of each entry. Note that due to the nature of hash tables you
_can't_ control the order in which the entries are provided by
`maphash` (or other traversing constructs). `maphash` always returns
`nil`.

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (setf (gethash 'first-key *my-hash*) 'one)
ONE
CL-USER> (setf (gethash 'second-key *my-hash*) 'two)
TWO
CL-USER> (setf (gethash 'third-key *my-hash*) nil)
NIL
CL-USER> (setf (gethash nil *my-hash*) 'nil-value)
NIL-VALUE
CL-USER> (defun print-hash-entry (key value)
    (format t "The value associated with the key ~S is ~S~%" key value))
PRINT-HASH-ENTRY
CL-USER> (maphash #'print-hash-entry *my-hash*)
The value associated with the key FIRST-KEY is ONE
The value associated with the key SECOND-KEY is TWO
The value associated with the key THIRD-KEY is NIL
The value associated with the key NIL is NIL-VALUE
~~~

You can also use
[`with-hash-table-iterator`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_hash.htm),
a macro which turns (via
[`macrolet`](http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm))
its first argument into an iterator that on each invocation returns
three values per hash table entry - a generalized boolean that's true
if an entry is returned, the key of the entry, and the value of the
entry. If there are no more entries, only one value is returned -
`nil`.

~~~lisp
;;; same hash-table as above
CL-USER> (with-hash-table-iterator (my-iterator *my-hash*)
           (loop
              (multiple-value-bind (entry-p key value)
                  (my-iterator)
                (if entry-p
                    (print-hash-entry key value)
                    (return)))))
The value associated with the key FIRST-KEY is ONE
The value associated with the key SECOND-KEY is TWO
The value associated with the key THIRD-KEY is NIL
The value associated with the key NIL is NIL-VALUE
NIL
~~~

Note the following caveat from the HyperSpec: "It is unspecified what
happens if any of the implicit interior state of an iteration is
returned outside the dynamic extent of the `with-hash-table-iterator`
form such as by returning some closure over the invocation form."


And there's always [`loop`](http://www.lispworks.com/documentation/HyperSpec/Body/06_a.htm):

~~~lisp
;;; same hash-table as above
CL-USER> (loop for key being the hash-keys of *my-hash*
           do (print key))
FIRST-KEY
SECOND-KEY
THIRD-KEY
NIL
NIL
CL-USER> (loop for key being the hash-keys of *my-hash*
           using (hash-value value)
           do (format t "The value associated with the key ~S is ~S~%" key value))
The value associated with the key FIRST-KEY is ONE
The value associated with the key SECOND-KEY is TWO
The value associated with the key THIRD-KEY is NIL
The value associated with the key NIL is NIL-VALUE
NIL
CL-USER> (loop for value being the hash-values of *my-hash*
           do (print value))
ONE
TWO
NIL
NIL-VALUE
NIL
CL-USER> (loop for value being the hash-values of *my-hash*
           using (hash-key key)
           do (format t "~&~A -> ~A" key value))
FIRST-KEY -> ONE
SECOND-KEY -> TWO
THIRD-KEY -> NIL
NIL -> NIL-VALUE
NIL
~~~

#### Traversing keys or values

To map over keys or values we can again rely on Alexandria with
`maphash-keys` and `maphash-values`.


<a name="count"></a>

### Counting the Entries in a Hash Table

No need to use your fingers - Common Lisp has a built-in function to
do it for you:
[`hash-table-count`](http://www.lispworks.com/documentation/HyperSpec/Body/f_hash_1.htm).

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (hash-table-count *my-hash*)
0
CL-USER> (setf (gethash 'first *my-hash*) 1)
1
CL-USER> (setf (gethash 'second *my-hash*) 2)
2
CL-USER> (setf (gethash 'third *my-hash*) 3)
3
CL-USER> (hash-table-count *my-hash*)
3
CL-USER> (setf (gethash 'second *my-hash*) 'two)
TWO
CL-USER> (hash-table-count *my-hash*)
3
CL-USER> (clrhash *my-hash*)
#<EQL hash table, 0 entries {48205F35}>
CL-USER> (hash-table-count *my-hash*)
0
~~~

### Printing a hash table readably

**With print-object** (non portable)

It is very tempting to use `print-object`. It works under several
implementations, but this method is actually not portable. The
standard doesn't permit to do so, so this is undefined behaviour.

~~~lisp
(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
          (loop for key being the hash-keys of object
                using (hash-value value)
                collect (list key value))))

;; WARNING:
;;   redefining PRINT-OBJECT (#<STRUCTURE-CLASS COMMON-LISP:HASH-TABLE>
;;                            #<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>) in DEFMETHOD
;; #<STANDARD-METHOD COMMON-LISP:PRINT-OBJECT (HASH-TABLE T) {1006A0D063}>
~~~

and let's try it:

~~~lisp
(let ((ht (make-hash-table)))
  (setf (gethash :foo ht) :bar)
  ht)
;; #HASH{(FOO : BAR)}
~~~

**With a custom function** (portable way)

Here's a portable way.

This snippets prints the keys, values and the test function of a
hash-table, and uses `alexandria:alist-hash-table` to read it back in:

~~~lisp
;; https://github.com/phoe/phoe-toolbox/blob/master/phoe-toolbox.lisp
(defun print-hash-table-readably (hash-table
                                  &optional (stream *standard-output*))
  "Prints a hash table readably using ALEXANDRIA:ALIST-HASH-TABLE."
  (let ((test (hash-table-test hash-table))
        (*print-circle* t)
        (*print-readably* t))
    (format stream "#.(ALEXANDRIA:ALIST-HASH-TABLE '(~%")
    (maphash (lambda (k v) (format stream "   (~S . ~S)~%" k v)) hash-table)
    (format stream "   ) :TEST '~A)" test)
    hash-table))
~~~

Example output:

```
#.(ALEXANDRIA:ALIST-HASH-TABLE
'((ONE . 1))
  :TEST 'EQL)
#<HASH-TABLE :TEST EQL :COUNT 1 {10046D4863}>
```

This output can be read back in to create a hash-table:

~~~lisp
(read-from-string
 (with-output-to-string (s)
   (print-hash-table-readably
    (alexandria:alist-hash-table
     '((a . 1) (b . 2) (c . 3))) s)))
;; #<HASH-TABLE :TEST EQL :COUNT 3 {1009592E23}>
;; 83
~~~

**With Serapeum** (readable and portable)

The [Serapeum library](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#hash-tables)
has the `dict` constructor, the function `pretty-print-hash-table` and
the `toggle-pretty-print-hash-table` switch, all which do *not* use
`print-object` under the hood.

~~~lisp
CL-USER> (serapeum:toggle-pretty-print-hash-table)
T
CL-USER> (serapeum:dict :a 1 :b 2 :c 3)
(dict
  :A 1
  :B 2
  :C 3
 )
~~~

This printed representation can be read back in.


<a name="size"></a>

### Performance Issues: The Size of your Hash Table

The `make-hash-table` function has a couple of optional parameters
which control the initial size of your hash table and how it'll grow
if it needs to grow. This can be an important performance issue if
you're working with large hash tables. Here's an (admittedly not very
scientific) example with [CMUCL](http://www.cons.org/cmucl) pre-18d on
Linux:

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (hash-table-size *my-hash*)
65
CL-USER> (hash-table-rehash-size *my-hash*)
1.5
CL-USER> (time (dotimes (n 100000) (setf (gethash n *my-hash*) n)))
Compiling LAMBDA NIL:
Compiling Top-Level Form:

Evaluation took:
  0.27 seconds of real time
  0.25 seconds of user run time
  0.02 seconds of system run time
  0 page faults and
  8754768 bytes consed.
NIL
CL-USER> (time (dotimes (n 100000) (setf (gethash n *my-hash*) n)))
Compiling LAMBDA NIL:
Compiling Top-Level Form:

Evaluation took:
  0.05 seconds of real time
  0.05 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.
NIL
~~~

The values for
[`hash-table-size`](http://www.lispworks.com/documentation/HyperSpec/Body/f_hash_4.htm)
and
[`hash-table-rehash-size`](http://www.lispworks.com/documentation/HyperSpec/Body/f_hash_2.htm)
are implementation-dependent. In our case, CMUCL chooses and initial
size of 65, and it will increase the size of the hash by 50 percent
whenever it needs to grow. Let's see how often we have to re-size the
hash until we reach the final size...

~~~lisp
CL-USER> (log (/ 100000 65) 1.5)
18.099062
CL-USER> (let ((size 65)) (dotimes (n 20) (print (list n size)) (setq size (* 1.5 size))))
(0 65)
(1 97.5)
(2 146.25)
(3 219.375)
(4 329.0625)
(5 493.59375)
(6 740.3906)
(7 1110.5859)
(8 1665.8789)
(9 2498.8184)
(10 3748.2275)
(11 5622.3413)
(12 8433.512)
(13 12650.268)
(14 18975.402)
(15 28463.104)
(16 42694.656)
(17 64041.984)
(18 96062.98)
(19 144094.47)
NIL
~~~

The hash has to be re-sized 19 times until it's big enough to hold
100,000 entries. That explains why we saw a lot of consing and why it
took rather long to fill the hash table. It also explains why the
second run was much faster - the hash table already had the correct
size.

Here's a faster way to do it:
If we know in advance how big our hash will be, we can start with the right size:

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table :size 100000))
*MY-HASH*
CL-USER> (hash-table-size *my-hash*)
100000
CL-USER> (time (dotimes (n 100000) (setf (gethash n *my-hash*) n)))
Compiling LAMBDA NIL:
Compiling Top-Level Form:

Evaluation took:
  0.04 seconds of real time
  0.04 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.
NIL
~~~

That's obviously much faster. And there was no consing involved
because we didn't have to re-size at all. If we don't know the final
size in advance but can guess the growth behaviour of our hash table
we can also provide this value to `make-hash-table`. We can provide an
integer to specify absolute growth or a float to specify relative
growth.

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table :rehash-size 100000))
*MY-HASH*
CL-USER> (hash-table-size *my-hash*)
65
CL-USER> (hash-table-rehash-size *my-hash*)
100000
CL-USER> (time (dotimes (n 100000) (setf (gethash n *my-hash*) n)))
Compiling LAMBDA NIL:
Compiling Top-Level Form:

Evaluation took:
  0.07 seconds of real time
  0.05 seconds of user run time
  0.01 seconds of system run time
  0 page faults and
  2001360 bytes consed.
NIL
~~~

Also rather fast (we only needed one re-size) but much more consing
because almost the whole hash table (minus 65 initial elements) had to
be built during the loop.

Note that you can also specify the `rehash-threshold` while creating a
new hash table. One final remark: Your implementation is allowed to
_completely ignore_ the values provided for `rehash-size` and
`rehash-threshold`...

## Alist

### Definition

An association list is a list of cons cells.

This simple example:

~~~lisp
(defparameter *my-alist* (list (cons 'foo "foo")
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

### Construction

We can construct an alist like its representation:


~~~lisp
(setf *my-alist* '((:foo . "foo")
                 (:bar . "bar")))
~~~


The constructor `pairlis` associates a list of keys and a list of values:

~~~lisp
(pairlis '(:foo :bar)
         '("foo" "bar"))
;; => ((:BAR . "bar") (:FOO . "foo"))
~~~

Alists are just lists, so you can have the same key multiple times in the same alist:

~~~lisp
(setf *alist-with-duplicate-keys*
  '((:a . 1)
    (:a . 2)
    (:b . 3)
    (:a . 4)
    (:c . 5)))
~~~


### Access

To get a key, we have `assoc` (use `:test 'equal` when your keys are
strings, as usual). It returns the whole cons cell, so you may want to
use `cdr` or `second` to get the value or even better `assoc-value list key` from `Alexandria`.


~~~lisp
(alexandria:assoc-value *my-alist* :foo)
;; it actually returns 2 values
;; "foo"
;; (:FOO . "FOO")
~~~

There is `assoc-if`, and `rassoc` to get a cons cell by its value.

If the alist has repeating (duplicate) keys, you can use `remove-if-not`, for example, to retrieve all of them.

~~~lisp
(remove-if-not
  #'(lambda (entry)
      (eq :a entry))
  *alist-with-duplicate-keys*
  :key #'car)
~~~

### Insert and remove entries

To add a key, we `push` another cons cell:

~~~lisp
(push (cons 'team "team") *my-alist*)
;; => ((TEAM . "team") (FOO . "foo") (BAR . "bar"))
~~~

We can use `pop` and other functions that operate on lists, like `remove`:

~~~lisp
(remove :team *my-alist*)
;; => ((:TEAM . "team") (FOO . "foo") (BAR . "bar")) ;; didn't remove anything
(remove :team *my-alist* :key 'car)
;; => ((FOO . "foo") (BAR . "bar")) ;; returns a copy
~~~

Remove only one element with `:count`:

~~~lisp
(push (cons 'bar "bar2") *my-alist*)
;; => ((BAR . "bar2") (TEAM . "team") (FOO . "foo") (BAR . "bar")) ;; twice the 'bar key
(remove 'bar *my-alist* :key 'car :count 1)
;; => ((TEAM . "team") (FOO . "foo") (BAR . "bar"))
;; because otherwise:
(remove 'bar *my-alist* :key 'car)
;; => ((TEAM . "team") (FOO . "foo")) ;; no more 'bar
~~~

### Update entries

Replace a value:

~~~lisp
*my-alist*
;; => '((:FOO . "foo") (:BAR . "bar"))
(assoc :foo *my-alist*)
;; => (:FOO . "foo")
(setf (cdr (assoc :foo *my-alist*)) "new-value")
;; => "new-value"
*my-alist*
;; => '((:foo . "new-value") (:BAR . "bar"))
~~~

Replace a key:

~~~lisp
*my-alist*
;; => '((:FOO . "foo") (:BAR . "bar")))
(setf (car (assoc :bar *my-alist*)) :new-key)
;; => :NEW-KEY
*my-alist*
;; => '((:FOO . "foo") (:NEW-KEY . "bar")))
~~~

In the
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html#Conses)
library, see more functions like `hash-table-alist`, `alist-plist`,…


## Plist

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

## Structures

Structures offer a way to store data in named slots. They support
single inheritance.

Classes provided by the Common Lisp Object System (CLOS) are more flexible however structures may offer better performance (see for example the SBCL manual).

### Creation

Use `defstruct`:

~~~lisp
(defstruct person
   id name age)
~~~

At creation slots are optional and default to `nil`.

To set a default value:

~~~lisp
(defstruct person
   id
   (name "john doe")
   age)
~~~

Also specify the type after the default value:

~~~lisp
(defstruct person
  id
  (name "john doe" :type string)
  age)
~~~

We create an instance with the generated constructor `make-` +
`<structure-name>`, so `make-person`:

~~~lisp
(defparameter *me* (make-person))
*me*
#S(PERSON :ID NIL :NAME "john doe" :AGE NIL)
~~~

note that printed representations can be read back by the reader.

With a bad name type:

~~~lisp
(defparameter *bad-name* (make-person :name 123))
~~~

```
Invalid initialization argument:
  :NAME
in call for class #<STRUCTURE-CLASS PERSON>.
   [Condition of type SB-PCL::INITARG-ERROR]
```

We can set the structure's constructor so as to create the structure
without using keyword arguments, which can be more convenient
sometimes. We give it a name and the order of the arguments:

~~~lisp
(defstruct (person (:constructor create-person (id name age)))
     id
     name
     age)
~~~

Our new constructor is `create-person`:

~~~lisp
(create-person 1 "me" 7)
#S(PERSON :ID 1 :NAME "me" :AGE 7)
~~~

However, the default `make-person` does *not* work any more:

~~~lisp
(make-person :name "me")
;; debugger:
obsolete structure error for a structure of type PERSON
[Condition of type SB-PCL::OBSOLETE-STRUCTURE]
~~~



### Slot access

We access the slots with accessors created by `<name-of-the-struct>-` + `slot-name`:

~~~lisp
(person-name *me*)
;; "john doe"
~~~

we then also have `person-age` and `person-id`.

### Setting

Slots are `setf`-able:

~~~lisp
(setf (person-name *me*) "Cookbook author")
(person-name *me*)
;; "Cookbook author"
~~~

### Predicate

A predicate function is generated:

~~~lisp
(person-p *me*)
T
~~~

### Single inheritance

Use single inheritance with the `:include <struct>` argument:

~~~lisp
(defstruct (female (:include person))
     (gender "female" :type string))
(make-female :name "Lilie")
;; #S(FEMALE :ID NIL :NAME "Lilie" :AGE NIL :GENDER "female")
~~~

Note that the CLOS object system is more powerful.

### Limitations

After a change, instances are not updated.

If we try to add a slot (`email` below), we have the choice to lose
all instances, or to continue using the new definition of
`person`. But the effects of redefining a structure are undefined by
the standard, so it is best to re-compile and re-run the changed
code.

~~~lisp
(defstruct person
       id
       (name "john doe" :type string)
       age
       email)

attempt to redefine the STRUCTURE-OBJECT class PERSON
incompatibly with the current definition
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [CONTINUE] Use the new definition of PERSON, invalidating already-loaded code and instances.
 1: [RECKLESSLY-CONTINUE] Use the new definition of PERSON as if it were compatible, allowing old accessors to use new instances and allowing new accessors to use old instances.
 2: [CLOBBER-IT] (deprecated synonym for RECKLESSLY-CONTINUE)
 3: [RETRY] Retry SLIME REPL evaluation request.
 4: [*ABORT] Return to SLIME's top level.
 5: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1002A0FFA3}>)
~~~

If we choose restart `0`, to use the new definition, we lose access to `*me*`:

~~~lisp
*me*
obsolete structure error for a structure of type PERSON
   [Condition of type SB-PCL::OBSOLETE-STRUCTURE]
~~~

There is also very little introspection.
Portable Common Lisp does not define ways of finding out defined super/sub-structures nor what slots a structure has.

The Common Lisp Object System (which came after into the language)
doesn't have such limitations. See the [CLOS section](clos.html).

* [structures on the hyperspec](http://www.lispworks.com/documentation/HyperSpec/Body/08_.htm)
* David B. Lamkins, ["Successful Lisp, How to Understand and Use Common Lisp"](http://www.communitypicks.com/r/lisp/s/17592186045679-successful-lisp-how-to-understand-and-use-common).

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

## Controlling how much of data to print (`*print-length*`, `*print-level*`)

Use `*print-length*` and `*print-level*`.

They are both `nil` by default.

If you have a very big list, printing it on the REPL or in a
stacktrace can take a long time and bring your editor or even your
server down. Use `*print-length*` to choose the maximum of elements of
the list to print, and to show there is a rest with a `...`
placeholder:

~~~lisp
(setf *print-length* 2)
(list :A :B :C :D :E)
;; (:A :B ...)
~~~

And if you have a very nested data structure, set `*print-level*` to
choose the depth to print:

~~~lisp
(let ((*print-level* 2))
  (print '(:a (:b (:c (:d :e))))))
;; (:A (:B #))             <= *print-level* in action
;; (:A (:B (:C (:D :E))))  <= the list is returned, the let binding is not in effect anymore.
~~~

`*print-length*` will be applied at each level.

Reference: the [HyperSpec](http://clhs.lisp.se/Body/v_pr_lev.htm).


## Appendix A - generic and nested access of alists, plists, hash-tables and CLOS slots

The solutions presented below might help you getting started, but keep
in mind that they'll have a performance impact and that error messages
will be less explicit.

* the [access](https://github.com/AccelerationNet/access) library (battle tested, used by the Djula templating system) has a generic `(access my-var :elt)` ([blog post](https://lisp-journey.gitlab.io/blog/generice-consistent-access-of-data-structures-dotted-path/)). It also has `accesses` (plural) to access and set nested values.
* [rutils](https://github.com/vseloved/rutils) as a generic `generic-elt` or `?`,

## Appendix B - accessing nested data structures

Sometimes we work with nested data structures, and we might want an
easier way to access a nested element than intricated "getf" and
"assoc" and all. Also, we might want to just be returned a `nil` when
an intermediary key doesn't exist.

The `access` library given above provides this, with `(accesses var key1 key2…)`.
