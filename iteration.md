---
title: Loop, iteration, mapping
---

<!-- needs some text before the first heading -->

## Introduction: loop, iterate, for, mapcar, series

**[loop](http://www.lispworks.com/documentation/lw51/CLHS/Body/m_loop.htm)**
is the built-in macro for iteration.

Its simplest form is `(loop (print "hello"))`: this will print forever.

A simple iteration over a list is:

~~~lisp
(loop for x in '(1 2 3)
  do (print x))
~~~

It prints what's needed but returns `nil`.

If you want to return a list, use `collect`:

~~~lisp
(loop for x in '(1 2 3)
  collect (* x 10))
;; (10 20 30)
~~~

The Loop macro is different than most Lisp expressions in having a complex
internal domain-specific language that doesn't use s-expressions.
So you need to read Loop expressions with half of your brain in Lisp mode, and
the other half in Loop mode. You love it or you hate it.

Think of Loop expressions as having four parts: expressions that set up
variables that will be iterated, expressions that conditionally terminate the
iteration, expressions that do something on each iteration, and expressions that
do something right before the Loop exits.  In addition, Loop expressions can
return a value.  It is very rare to use all of these parts in a given Loop
expression, but you can combine them in many ways.

**[iterate](https://common-lisp.net/project/iterate/doc/index.html)** is a
popular iteration macro that aims at being simpler, "lispier" and more
predictable than `loop`, besides being extensible. However it isn't built-in,
so you have to import it:

    (ql:quickload "iterate")
    (use-package :iterate)

Iterate looks like this:

~~~lisp
(iter (for i from 1 to 5)
    (collect (* i i)))
~~~

(if you use loop and iterate in the same package, you might run into name conflicts)

Iterate also comes with `display-iterate-clauses` that can be quite handy:

~~~lisp
(display-iterate-clauses '(for))
;; FOR PREVIOUS &OPTIONAL INITIALLY BACK     Previous value of a variable
;; FOR FIRST THEN            Set var on first, and then on subsequent iterations
;; ...
~~~

Much of the examples on this page that are valid for loop are also valid for iterate,
with minor modifications.

**[for](https://github.com/Shinmera/for/)** is an extensible iteration
macro that is often shorter than loop, that "unlike loop is extensible
and sensible, and unlike iterate does not require code-walking and is
easier to extend".

It has the other advantage of having one construct that works for all
data structures (lists, vectors, hash-tables…): in doubt, just use
`for… over`:

~~~lisp
(for:for ((x over <your data structure>))
   (print …))
~~~

You also have to quickload it:

    (ql:quickload "for")


We'll also give examples with **`mapcar`** and `map`, and eventually
with their friends `mapcon`, `mapcan`, `maplist`, `mapc` and `mapl`
which E. Weitz categorizes very well in his "Common Lisp Recipes",
chap. 7. The one you are certainly accustomed to from other languages is
`mapcar`: it takes a function, one or more lists as arguments,
applies the function on each *element* of the lists one by one and
returns a list of result.

~~~lisp
(mapcar (lambda (it) (+ it 10)) '(1 2 3))
(11 12 13)
~~~

`map` is generic, it accepts list and vectors as arguments, and
expects the type for its result as first argument:

~~~lisp
(map 'vector (lambda (it) (+ it 10)) '(1 2 3))
;; #(11 12 13)
(map 'list (lambda (it) (+ it 10)) #(1 2 3))
;; (11 12 13)
(map 'string (lambda (it) (code-char it)) '#(97 98 99))
;; "abc"
~~~

The other constructs have their advantages in some situations ;) They
either process the *tails* of lists, or *concatenate* the return
values, or don't return anything. We'll see some of them.

If you like `mapcar`, use it a lot, and would like a quicker and
shorter way to write lambdas, then you might like one of those
[lambda shorthand libraries](https://github.com/CodyReichert/awesome-cl#lambda-shorthands).

Here is an example with [cl-punch](https://github.com/windymelt/cl-punch/):

~~~lisp
(mapcar ^(* _ 10) '(1 2 3))
;; (10 20 30)
~~~

and voilà :) We won't use this more in this recipe, but feel free to do.

Last but not least, you might like
**[series](http://series.sourceforge.net/)**,
a library that describes itself as combining aspects of sequences,
streams, and loops. Series expressions look like operations on
sequences (= functional programming), but can achieve the same high level of efficiency as a
loop. Series first appeared in "Common Lisp the Language", in the
appendix A (it nearly became part of the language). Series looks like
this:

~~~lisp
(collect
  (mapping ((x (scan-range :from 1 :upto 5)))
    (* x x)))
;; (1 4 9 16 25)
~~~

`series` is good, but its function names are different from what we
find in functional languages today. You might like the ["Generators
The Way I Want Them Generated"](https://github.com/cbeo/gtwiwtg)
library. It is a lazy sequences library, similar to `series` although
younger and not as complete, with a "modern" API with words like `take`, `filter`,
`for` or `fold`, and that is easy to use.

~~~lisp
(range :from 20)
;; #<GTWIWTG::GENERATOR! {1001A90CA3}>

(take 4 (range :from 20))
;; (20 21 22 23)
~~~

At the time of writing, GTWIWTG is licensed under the GPLv3.


## Recipes

### Looping forever, return

~~~lisp
(loop
    (print "hello"))
~~~

`return` can return a result:

~~~lisp
(loop for i in '(1 2 3)
     when (> i 1)
     return i)
2
~~~


### Looping a fixed number of times

#### dotimes

~~~lisp
(dotimes (n 10)
  (print n))
~~~

Here `dotimes` returns `nil`. The return value is evaluated at the end of the loop.

You can use `return` inside of it:

~~~lisp
(dotimes (i 10)
   (if (> i 3)
       (return)
       (print i)))
~~~


#### loop… repeat

~~~lisp
(loop repeat 10
  do (format t "Hello!~%"))
~~~

This prints 10 times "hello" and returns `nil`.

~~~lisp
(loop repeat 10 collect (random 10))
;; (5 1 3 5 4 0 7 4 9 1)
~~~

with `collect`, this returns a list.

#### Series

~~~lisp
(iterate ((n (scan-range :below 10)))
  (print n))
~~~

### Looping an infinite number of times, cycling over a circular list

We can build an infinite list by setting its last element to the list itself:

~~~lisp
(loop with list-a = '(1 2 3)
      with infinite-list = (setf (cdr (last list-a)) list-a)
      for item in infinite-list
      repeat 8
      collect item)
;; (1 2 3 1 2 3 1 2)
~~~

Illustration: `(last '(1 2 3))` is `(3)`, a list, or rather a cons cell, whose `car` is 3 and `cdr` is NIL. See the [data-structures chapter](data-structures.html) for a reminder. This is the representation of `(list 3)`:

~~~
[o|/]
 |
 3
~~~

The representation of `(list 1 2 3)`:

```
[o|o]---[o|o]---[o|/]
 |       |       |
 1       2       3
```

By setting the `cdr` of the last element to the list itself, we make it recur on itself.

A notation shortcut is possible with the `#=` syntax:

~~~lisp
(defparameter *list-a* '#1=(1 2 3 . #1#))
(setf *print-circle* t)  ;; don't print circular lists forever
*list-a*
~~~

If you need to alternate only between two values, use `for … then`:

~~~lisp
(loop repeat 4
      for up = t then (not up)
      do (print up))
T
NIL
T
NIL
~~~

### Iterate's for loop

For lists and vectors:

~~~lisp
(iter (for item in '(1 2 3))
  (print item))
(iter (for i in-vector #(1 2 3))
  (print i))
~~~

Looping over a hash-table is also straightforward:

~~~lisp
(let ((h (let ((h (make-hash-table)))
           (setf (gethash 'a h) 1)
           (setf (gethash 'b h) 2)
           h)))
  (iter (for (k v) in-hashtable h)
    (print k)))
;; b
;; a
~~~

In fact, take a look [here](https://common-lisp.net/project/iterate/doc/Sequence-Iteration.html),
or `(display-iterate-clauses '(for))` to know about iterating over

- symbols in-package
- forms - or lines, or whatever-you-wish - in-file, or in-stream
- elements in-sequence - sequences can be vectors or lists

### Looping over a list

#### dolist

~~~lisp
(dolist (item '(1 2 3))
  (print item))
~~~

`dolist` returns `nil`.

#### loop

with `in`, no surprises:

~~~lisp
(loop for x in '(a b c)
      do (print x))
;; A
;; B
;; C
;; NIL
~~~

~~~lisp
(loop for x in '(a b c)
      collect x)
;; (A B C)
~~~

With `on`, we loop over the cdr of the list:

~~~lisp
(loop for i on '(1 2 3) do (print i))
;; (1 2 3)
;; (2 3)
;; (3)
~~~


#### mapcar

~~~lisp
(mapcar (lambda (x)
             (print (* x 10)))
         '(1 2 3))
10
20
30
(10 20 30)
~~~

`mapcar` returns the results of the lambda function as a list.

#### Series
~~~lisp
(iterate ((item (scan '(1 2 3))))
  (print item))
~~~

`scan-sublists` is the equivalent of `loop for ... on`:

~~~lisp
(iterate ((i (scan-sublists '(1 2 3))))
  (print i))
~~~

### Looping over a vector

#### loop: `across`

~~~lisp
(loop for i across #(1 2 3) do (print i))
~~~

#### Series

~~~lisp
(iterate ((i (scan #(1 2 3))))
  (print i))
~~~

### Looping over a hash-table

We create a hash-table:

~~~lisp
(defparameter h (make-hash-table))
(setf (gethash 'a h) 1)
(setf (gethash 'b h) 2)
~~~

#### loop

Looping over keys:

~~~lisp
(loop for k being the hash-key of h do (print k))
;; b
;; a
~~~

same with `hash-value`.

Looping over key-values pairs:

~~~lisp
(loop for k
    being the hash-key
    using (hash-value v) of h
    do (format t "~a ~a~%" k v))
b 2
a 1
~~~

#### iterate

Use `in-hashtable`:

~~~lisp
(iter (for (key value) in-hashtable h)
  (collect (list key value)))
~~~

#### for

the same with `for`:

~~~lisp
(for:for ((it over h))
    (print it))
(A 1)
(B 2)
NIL
~~~


#### maphash

The lambda function of `maphash` takes two arguments: the key and the
value:

~~~lisp
(maphash (lambda (key val)
             (format t "key: ~a val:~a~&" key val))
          h)
;; key: A val:1
;; key: B val:2
;; NIL
~~~

See also [with-hash-table-iterator](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_hash.htm).

#### dohash

Only because we like this topic, we introduce another library, [trivial-do](https://github.com/yitzchak/trivial-do/). It has the `dohash` macro, that ressembles `dolist`:

~~~lisp
(dohash (key value h)
  (format t "key: ~A, value: ~A~%" key value))
~~~

#### Series
~~~lisp
(iterate (((k v) (scan-hash h)))
  (format t "~&~a ~a~%" k v))
~~~

### Looping over two lists in parallel

#### loop

~~~lisp
(loop for x in '(a b c)
      for y in '(1 2 3)
      collect (list x y))
;; ((A 1) (B 2) (C 3))
~~~

To return a flat list, use `nconcing` instead of `collect`:

~~~lisp
(loop for x in '(a b c)
      for y in '(1 2 3)
      nconcing (list x y))
(A 1 B 2 C 3)
~~~

If a list is smaller than the other one, loop stops at the end of the small one:

~~~lisp
(loop for x in '(a b c)
      for y in '(1 2 3 4 5)
      collect (list x y))
;; ((A 1) (B 2) (C 3))
~~~

We could loop over the biggest list and manually access the elements
of the smaller one by index, but it would quickly be
inefficient. Instead, we can tell `loop` to extend the short list.

~~~lisp
(loop for y in '(1 2 3 4 5)
      for x-list = '(a b c) then (cdr x-list)
      for x = (or (car x-list) 'z)
      collect (list x y))
;; ((A 1) (B 2) (C 3) (Z 4) (Z 5))
~~~

The trick is that the notation `for … = … then (cdr …)` (note the `=`
and the role of `then`) shortens our intermediate list at each
iteration (thanks to `cdr`). It will first be `'(a b c)`, the initial
value, then we will get the `cdr`: `(2 3)`, then `(3)`, then
`NIL`. And both `(car NIL)` and `(cdr NIL)` return `NIL`, so we are
good.


#### mapcar
~~~lisp
(mapcar (lambda (x y)
          (list x y))
        '(a b c)
        '(1 2 3))
;; ((A 1) (B 2) (C 3))
~~~

or simply:

~~~lisp
(mapcar #'list
        '(a b c)
        '(1 2 3))
;; ((A 1) (B 2) (C 3))
~~~

Return a flat list:

~~~lisp
(mapcan (lambda (x y)
          (list x y))
        '(a b c)
        '(1 2 3))
;; (A 1 B 2 C 3)
~~~

#### Series
~~~lisp
(collect
  (#Mlist (scan '(a b c))
          (scan '(1 2 3))))
~~~

A more efficient way, when the lists are known to be of equal length:

~~~lisp
(collect
  (mapping (((x y) (scan-multiple 'list
                                  '(a b c)
                                  '(1 2 3))))
    (list x y)))
~~~

Return a flat list:

~~~lisp
(collect-append ; or collect-nconc
 (mapping (((x y) (scan-multiple 'list
                                 '(a b c)
                                 '(1 2 3))))
   (list x y)))
~~~


### Nested loops
#### loop

~~~lisp
(loop for x from 1 to 3
      collect (loop for y from 1 to x
		    collect y))
;; ((1) (1 2) (1 2 3))
~~~

To return a flat list, use `nconcing` instead of the first `collect`.

#### iterate

~~~lisp
(iter outer
   (for i below 2)
   (iter (for j below 3)
      (in outer (collect (list i j)))))
;; ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2))
~~~

#### Series

~~~lisp
(collect
  (mapping ((x (scan-range :from 1 :upto 3)))
    (collect (scan-range :from 1 :upto x))))
~~~


### Computing an intermediate value

Use `=`.

With `for`:

~~~lisp
(loop for x from 1 to 3
      for y = (* x 10)
      collect y)
;; (10 20 30)
~~~

With `with`, the difference being that the value is computed only
once:

~~~lisp
(loop for x from 1 to 3
      for y = (* x 10)
      with z = x
      collect (list x y z))
;; ((1 10 1) (2 20 1) (3 30 1))
~~~

The HyperSpec defines the `with` clause like this:

    with-clause::= with var1 [type-spec] [= form1] {and var2 [type-spec] [= form2]}*

so it turns out we can specify the type before the `=` and chain the `with` with `and`:

~~~lisp
(loop for x from 1 to 3
      for y integer = (* x 10)
      with z integer = x
      collect (list x y z))
~~~

~~~lisp
(loop for x upto 3
      with foo = :foo
      and bar = :bar
      collect (list x foo bar))
~~~

We can also give `for` a `then` clause that will be called at each iteration:

~~~lisp
(loop repeat 3
      for intermediate = 10 then (incf intermediate)
      do (print intermediate))
10
11
12
~~~

Here's a trick to alternate a boolean:

~~~lisp
(loop repeat 4
      for up = t then (not up)
      do (print up))

T
NIL
T
NIL
~~~

### Loop with a counter
#### loop
Iterate through a list, and have a counter iterate in parallel. The length of
the list determines when the iteration ends. Two sets of actions are defined,
one of which is executed conditionally.

~~~lisp
* (loop for x in '(a b c d e)
      for y from 1

      when (> y 1)
      do (format t ", ")

      do (format t "~A" x)
      )

A, B, C, D, E
NIL
~~~

We could also write the preceding loop using the IF construct.

~~~lisp
* (loop for x in '(a b c d e)
      for y from 1

      if (> y 1)
      do (format t ", ~A" x)
      else do (format t "~A" x)
      )

A, B, C, D, E
NIL
~~~

#### Series

By iterating on multiple series in parallel, and using an infinite
range, we can make a counter.

~~~lisp
(iterate ((x (scan '(a b c d e)))
          (y (scan-range :from 1)))
  (when (> y 1) (format t ", "))
  (format t "~A" x))
~~~

### Ascending, descending order, limits
#### loop

`from… to…`:

~~~lisp
(loop for i from 0 to 10
      do (print i))
;; 0 1 2 3 4 5 6 7 8 9 10
~~~

`from… below…`: this stops at 9:

~~~lisp
(loop for i from 0 below 10
      do (print i))
~~~

Similarly, use `from 10 downto 0` (10…0) and `from 10 above 0` (10…1).

#### Series

`:from ... :upto`, including the upper limit:

~~~lisp
(iterate ((i (scan-range :from 0 :upto 10)))
  (print i))
~~~

`:from ... :below`, excluding the upper limit:

~~~lisp
(iterate ((i (scan-range :from 0 :below 10)))
  (print i))
~~~


### Steps
#### loop

with `by`:

~~~lisp
(loop for i from 1 to 10 by 2
      do (print i))
~~~

if you use `by (1+ (random 3))`, the random is evaluated only once, as
if it was in a closure:

~~~lisp
(let ((step (random 3)))
   (loop for i from 1 to 10 by (+ 1 step)
      do (print i)))
~~~

The step must always be a positive number. If you want to count down, see above.

#### Series

with `:by`
~~~lisp
(iterate ((i (scan-range :from 1 :upto 10 :by 2)))
  (print i))
~~~


### Loop and conditionals
#### loop

with `if`, `else` and `finally`:

~~~lisp
;; https://riptutorial.com/common-lisp/example/11095/conditionally-executing-loop-clauses
(loop repeat 10
      for x = (random 100)
      if (evenp x)
        collect x into evens
      else
        collect x into odds
      finally (return (values evens odds)))
~~~

```
(42 82 24 92 92)
(55 89 59 13 49)
```

Combining multiple clauses in an if body requires special syntax (`and
do`, `and count`):

~~~lisp
 (loop repeat 10
       for x = (random 100)
       if (evenp x)
          collect x into evens
          and do (format t "~a is even!~%" x)
       else
          collect x into odds
          and count t into n-odds
       finally (return (values evens odds n-odds)))
~~~
```
46 is even!
8 is even!
76 is even!
58 is even!
0 is even!
(46 8 76 58 0)
(7 45 43 15 69)
5
```

#### iterate

Translating (or even writing!) the above example using iterate is straight-forward:

~~~lisp
(iter (repeat 10)
   (for x = (random 100))
   (if (evenp x)
       (progn
         (collect x into evens)
         (format t "~a is even!~%" x))
       (progn
         (collect x into odds)
         (count t into n-odds)))
   (finally (return (values evens odds n-odds))))
~~~

#### Series

The preceding loop would be done a bit differently in Series. `split`
sorts one series into multiple according to provided boolean series.

~~~lisp
(let* ((number (#M(lambda (n) (random 100))
                  (scan-range :below 10)))
       (parity (#Mevenp number)))
  (iterate ((n number) (p parity))
    (when p (format t "~a is even!~%" n)))
  (multiple-value-bind (evens odds) (split number parity)
    (values (collect evens)
            (collect odds)
            (collect-length odds))))
~~~

Note that although `iterate` and the three `collect` expressions are
written sequentially, only one iteration is performed, the same as the
example with loop.

### Begin the loop with a clause (initially)

~~~lisp
(loop initially
      (format t "~a " 'loop-begin)
      for x below 3
      do (format t "~a " x))
;; LOOP-BEGIN 0 1 2
~~~

`initially` also exists with `iterate`.


### Terminate the loop with a test (until, while)
#### loop

~~~lisp
(loop for x in '(1 2 3 4 5)
	until (> x 3)
	collect x)
;; (1 2 3)
~~~

the same, with `while`:

~~~lisp
(loop for x in '(1 2 3 4 5)
	while (< x 4)
	collect x)
~~~

#### Series

We truncate the series with `until-if`, then collect from its result.

~~~lisp
(collect
  (until-if (lambda (i) (> i 3))
            (scan '(1 2 3 4 5))))
~~~

### Loop, print and return a result
#### loop

`do` and `collect` can be combined in one expression

~~~lisp
(loop for x in '(1 2 3 4 5)
	while (< x 4)
        do (format t "x is ~a~&" x)
	collect x)
x is 1
x is 2
x is 3
(1 2 3)
~~~

#### Series
By mapping, we can perform a side effect and also collect items

~~~lisp
(collect
  (mapping ((x (until-if (complement (lambda (x) (< x 4)))
                         (scan '(1 2 3 4 5)))))
    (format t "x is ~a~&" x)
    x))
~~~


### Named loops and early exit
#### loop

The special `loop named` foo syntax allows you to create a loop that
you can exit early from. The exit is performed using `return-from`,
and can be used from within nested loops.


~~~lisp
;; useless example
(loop named loop-1
    for x from 0 to 10 by 2
    do (loop for y from 0 to 100 by (1+ (random 3))
            when (< x y)
            do (return-from loop-1 (values x y))))
0
2
~~~

Sometimes, you want to return early but execute the `finally` clause
anyways. Use [`loop-finish`](http://www.lispworks.com/documentation/HyperSpec/Body/m_loop_f.htm#loop-finish).

~~~lisp
(loop for x from 0 to 100
  do (print x)
  when (>= x 3)
  return x
  finally (print :done))  ;; <-- not printed
;; 0
;; 1
;; 2
;; 3
;; 3

(loop for x from 0 to 100
  do (print x)
  when (>= x 3)
  do (loop-finish)
  finally (print :done)
     (return x))
;; 0
;; 1
;; 2
;; 3
;; :DONE
;; 3
~~~

It is most needed when some computation must take place in the `finally` clause.

#### Loop shorthands for when/return

Several actions provide shorthands for combinations of when/return:

~~~lisp
* (loop for x in '(foo 2)
      thereis (numberp x))
T
~~~

~~~lisp
* (loop for x in '(foo 2)
      never (numberp x))
NIL
~~~

~~~lisp
* (loop for x in '(foo 2)
      always (numberp x))
NIL
~~~

They correspond to the functions `some`, `notany` and `every`:

~~~lisp
(some #'numberp '(foo 2))
(notany #'numberp '(foo 2))
(every #'numberp '(foo 2))
~~~


#### Series

A block is manually created and returned from.

~~~lisp
(block loop-1
  (iterate ((x (scan-range :from 0 :upto 10 :by 2)))
    (iterate ((y (scan-range :from 0 :upto 100 :by (1+ (random 3)))))
      (when (< x y)
        (return-from loop-1 (values x y))))))
~~~

### Count
#### loop
~~~lisp
(loop for i from 1 to 3 count (oddp i))
;; 2
~~~

#### Series
~~~lisp
(collect-length (choose-if #'oddp (scan-range :from 1 :upto 3)))
~~~

### Summation
#### loop

~~~lisp
(loop for i from 1 to 3 sum (* i i))
;; 14
~~~

Summing into a variable:

~~~lisp
(loop for i from 1 to 3
   sum (* i i) into total
   do (print i)
   finally (print total))
1
2
3
14
~~~


#### Series

~~~lisp
(collect-sum (#M(lambda (i) (* i i))
                (scan-range :from 1 :upto 3)))
~~~

### max, min
#### loop

~~~lisp
(loop for i from 1 to 3 maximize (mod i 3))
;; 2
~~~

and `minimize`.

#### Series
~~~lisp
(collect-max (#M(lambda (i) (mod i 3))
                (scan-range :from 1 :upto 3)))
~~~
and `collect-min`.

### Destructuring, aka pattern matching against the list or dotted pairs
#### loop

~~~lisp
(loop for (a b) in '((x 1) (y 2) (z 3))
      collect (list b a) )
;; ((1 X) (2 Y) (3 Z))
~~~

~~~lisp
(loop for (x . y) in '((1 . a) (2 . b) (3 . c)) collect y)
;; (A B C)
~~~

Use `nil` to ignore a term:

~~~lisp
(loop for (a nil) in '((x 1) (y 2) (z 3))
      collect a )
;; (X Y Z)
~~~

##### Iterating 2 by 2 over a list

To iterate over a list, 2 items at a time we use a combination of `on`, `by` and destructuring.

We use `on` to loop over the rest (the `cdr`) of the list.

~~~lisp
(loop for rest on '(a 2 b 2 c 3)
      collect rest)
;; ((A 2 B 2 C 3) (2 B 2 C 3) (B 2 C 3) (2 C 3) (C 3) (3))
~~~

We use `by` to skip one element at every iteration (`(cddr list)` is equivalent to `(rest (rest list))`)

~~~lisp
(loop for rest on '(a 2 b 2 c 3) by #'cddr
      collect rest)
;; ((A 2 B 2 C 3) (B 2 C 3) (C 3))
~~~

Then we add destructuring to bind only the first two items at each iteration:

~~~lisp
(loop for (key value) on '(a 2 b 2 c 3) by #'cddr
      collect (list key (* 2 value)))
;; ((A 2) (B 4) (C 6))
~~~


#### Series
In general, with `destructuring-bind`:

~~~lisp
(collect
  (mapping ((l (scan '((x 1) (y 2) (z 3)))))
    (destructuring-bind (a b) l
      (list b a))))
~~~

But for alists, `scan-alist` is provided:

~~~lisp
(collect
  (mapping (((a b) (scan-alist '((1 . a) (2 . b) (3 . c)))))
    b))
~~~

## Iterate unique features lacking in loop

`iterate` has some other things unique to it.

If you are a newcomer in Lisp, it's perfectly OK to keep you this section for
later. You could very well spend your career in Lisp without resorting
to those features… although they might turn out useful one day.


### No rigid order for clauses

`loop` requires that all `for` clauses appear before the loop body,
for example before a `while`. It's ok for `iter` to not follow this
order:

~~~lisp
(iter (for x in '(1 2 99)
  (while (< x 10))
  (for y = (print x))
  (collect (list x y)))
~~~

### Accumulating clauses can be nested

`collect`, `appending` and other accumulating clauses can appear anywhere:

~~~lisp
(iter (for x in '(1 2 3))
  (case x
    (1 (collect :a))
    ;;  ^^ iter keyword, nested in a s-expression.
    (2 (collect :b))))
~~~

### Finders: `finding`

`iterate` has [finders](https://common-lisp.net/project/iterate/doc/Finders.html#Finders).

> A finder is a clause whose value is an expression that meets some condition.

We can use `finding` followed by `maximizing`, `minimizing` or `such-that`.

Here's how to find the longest list in a list of lists:

~~~lisp
(iter (for elt in '((a) (b c d) (e f)))
      (finding elt maximizing (length elt)))
=> (B C D)
~~~

The rough equivalent in LOOP would be:

~~~lisp
(loop with max-elt = nil
      with max-key = 0
      for elt in '((a) (b c d) (e f))
      for key = (length elt)
      do
      (when (> key max-key)
        (setf max-elt elt
              max-key key))
      finally (return max-elt))
=> (B C D)
~~~

There could be more than one `such-that` clause:

~~~lisp
 (iter (for i in '(7 -4 2 -3))
       (if (plusp i)
    (finding i such-that (evenp i))
        (finding (- i) such-that (oddp i))))
;; => 2
~~~

We can also write `such-that #'evenp` and `such-that #'oddp`.


### Control flow: `next-iteration`

It is like "continue" and loop doesn't have it.

> Skips the remainder of the loop body and begins the next iteration of the loop.

`iterate` also has `first-iteration-p` and `(if-first-time then else)`.

See [control flow](https://common-lisp.net/project/iterate/doc/Control-Flow.html#Control-Flow


### Generators

Use `generate` and `next`. A generator is lazy, it goes to the next value when said explicitly.

~~~lisp
(iter (for i in '(1 2 3 4 5))
      (generate c in-string "black")
      (if (oddp i) (next c))
      (format t "~a " c))
;; b b l l a
;; NIL
~~~

### Variable backtracking (`previous`) VS parallel binding

~~~lisp
(iter (for el in '(a b c d e))
      (for prev-el previous el)
      (collect (list el prev-el)))
;; => ((A NIL) (B A) (C B) (D C) (E D))
~~~

although it is doable with `loop`'s parallel binding `and`, which is unsupported in `iterate`:

~~~lisp
(loop for el in '(a b c d e)
      and prev-el = nil then el
      collect (list el prev-el))
~~~

### More clauses

- `in-string` can be used explicitly to iterate character by character over a string. With loop, use `across`.

~~~lisp
(iter (for c in-string "hello")
      (collect c))
;; => (#\h #\e #\l #\l #\o)
~~~

- `loop` offers `collecting`, `nconcing`, and `appending`. `iterate` has these and also `adjoining`, `unioning`, `nunioning`, and `accumulating`.

~~~lisp
(iter (for el in '(a b c a d b))
      (adjoining el))
;; => (A B C D)
~~~

(`adjoin` is a set operation)

- `loop` has summing, counting, maximizing, and minimizing. `iterate` also includes `multiplying` and `reducing`. reducing is the generalized reduction builder:

~~~lisp
(iter (with dividend = 100)
      (for divisor in '(10 5 2))
      (reducing divisor by #'/ initial-value dividend))
;; => 1
~~~


### Iterate is extensible

~~~lisp
(defmacro dividing-by (num &keys (initial-value 0))
  `(reducing ,num by #'/ initial-value ,initial-value))

(iter (for i in '(10 5 2))
      (dividing-by i :initial-value 100))
=> 1
~~~

but [there is more to it, see the documentation](https://common-lisp.net/project/iterate/doc/Rolling-Your-Own.html#Rolling-Your-Own).

We saw libraries extending `loop`, for example CLSQL, but they are
full of feature flag checks (`#+(or allegro clisp-aloop cmu openmcl
sbcl scl)`) and they call internal modules
(`ansi-loop::add-loop-path`, `sb-loop::add-loop-path` etc).


## Custom series scanners

If we often scan the same type of object, we can write our own scanner
 for it: the iteration itself can be factored out. Taking the example
 above, of scanning a list of two-element lists, we'll write a scanner
 that returns a series of the first elements and a series of the
 second.

~~~lisp
(defun scan-listlist (listlist)
  (declare (optimizable-series-function 2))
  (map-fn '(values t t)
          (lambda (l)
            (destructuring-bind (a b) l
              (values a b)))
          (scan listlist)))

(collect
  (mapping (((a b) (scan-listlist '((x 1) (y 2) (z 3)))))
    (list b a)))
~~~

## Shorter series expressions

Consider this series expression:

~~~lisp

(collect-sum (mapping ((i (scan-range :length 5)))
                    (* i 2)))
~~~

It's a bit longer than it needs to be, the `mapping` form's only
purpose is to bind the variable `i`, and `i` is used in only one
place. Series has a "hidden feature" that allows us to simplify this
expression to the following:

~~~lisp
(collect-sum (* 2 (scan-range :length 5)))
~~~

This is called implicit mapping and can be enabled in the call to
`series::install`:

~~~lisp
(series::install :implicit-map t)
~~~

When using implicit mapping, the `#M` reader macro demonstrated above
becomes redundant.

## Loop gotchas

- the keyword `it`, often used in functional constructs, can be
  recognized as a loop keyword. Don't use it inside a loop.


## Appendix: list of loop keywords

**Name Clause**

```
named
```

**Variable Clauses**

```
initially finally for as with
```

**Main Clauses**

```
do collect collecting append
appending nconc nconcing into count
counting sum summing maximize return loop-finish
maximizing minimize minimizing doing
thereis always never if when
unless repeat while until
```

These don’t introduce clauses:

```
= and it else end from upfrom
above below to upto downto downfrom
in on then across being each the hash-key
hash-keys of using hash-value hash-values
symbol symbols present-symbol
present-symbols external-symbol
external-symbols fixnum float t nil of-type
```

But note that it’s the parsing that determines what is a keyword. For example in:

~~~lisp
(loop for key in hash-values)
~~~

Only `for` and `in` are keywords.


©Dan Robertson on [Stack Overflow](https://stackoverflow.com/questions/52236803/list-of-loop-keywords).

## Credit and references

### Loop

* [Tutorial for the Common Lisp Loop Macro](http://www.ai.sri.com/pkarp/loop.html) by Peter D. Karp
* [Common Lisp's Loop Macro Examples for Beginners](http://www.unixuser.org/~euske/doc/cl/loop.html) by Yusuke Shinyama
* [Section 6.1 The LOOP Facility, of the draft Common Lisp Standard (X3J13/94-101R)](https://gitlab.com/vancan1ty/clstandard_build) - the (draft) standard provides background information on Loop development, specification and examples. [Single PDF file available](https://gitlab.com/vancan1ty/clstandard_build/-/blob/master/cl-ansi-standard-draft-w-sidebar.pdf)
* [26. Loop by Jon L White, edited and expanded by Guy L. Steele Jr.](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node235.html) - from the book "Common Lisp the Language, 2nd Edition". Strong connection to the draft above, with supplementing comments and examples.

### Iterate

* [The Iterate Manual](https://common-lisp.net/project/iterate/doc/index.html) -by Jonathan Amsterdam and Luís Oliveira
* [iterate - Pseudocodic Iteration](https://common-lisp-libraries.readthedocs.io/iterate/) - by Shubhamkar Ayare
* [Loop v Iterate - SabraOnTheHill](https://sites.google.com/site/sabraonthehill/loop-v-iter)
* [Comparing loop and iterate](https://web.archive.org/web/20170713081006/https://items.sjbach.com/211/comparing-loop-and-iterate) - by Stephen Bach (web archive)

### Series

* [SERIES for Common Lisp - Richard C. Waters](http://series.sourceforge.net/)

### Others

* See also: [more functional constructs](https://lisp-journey.gitlab.io/blog/snippets-functional-style-more/) (do-repeat, take,…)
