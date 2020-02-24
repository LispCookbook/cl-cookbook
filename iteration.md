---
title: Loop, iteration, mapping
---

TODO: of-type

<!-- needs some text before the first heading -->

# Introduction: loop, iterate, for, mapcar

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
internal domain specific language that doesn't use s-expressions.
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

    (ql:quickload :iterate)
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

    (ql:quickload :for)


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
**[series](https://github.com/tokenrove/series/wiki/Documentation)**,
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

# Recipes

## Looping forever, return

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


## Looping a fixed number of times

### dotimes

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


### loop… repeat

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

### Series

~~~lisp
(iterate ((n (scan-range :below 10)))
  (print n))
~~~

## Iterate's for loop

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

## Looping over a list

### dolist

~~~lisp
(dolist (item '(1 2 3))
  (print item))
~~~

`dolist` returns `nil`.

### loop

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


### mapcar

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

### Series
~~~lisp
(iterate ((item (scan '(1 2 3))))
  (print item))
~~~

`scan-sublists` is the equivalent of `loop for ... on`:

~~~lisp
(iterate ((i (scan-sublists '(1 2 3))))
  (print i))
~~~

## Looping over a vector

### loop: `across`

~~~lisp
(loop for i across #(1 2 3) do (print i))
~~~

### Series

~~~lisp
(iterate ((i (scan #(1 2 3))))
  (print i))
~~~

## Looping over a hash-table

We create a hash-table:

~~~lisp
(setf h (make-hash-table))
(setf (gethash 'a h) 1)
(setf (gethash 'b h) 2)
~~~

### loop

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

### for

the same with `for`:

~~~lisp
(for:for ((it over h))
    (print it))
(A 1)
(B 2)
NIL
~~~


### maphash

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

### Series
~~~lisp
(iterate (((k v) (scan-hash h)))
  (format t "~&~a ~a~%" k v))
~~~

## Looping over two lists in parallel

### loop

~~~lisp
(loop for x in '(a b c)
      for y in '(1 2 3)
      collect (list x y))
;; ((A 1) (B 2) (C 3))
~~~

### mapcar
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

### Series
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


## Nested loops
### loop
~~~lisp
(loop for x from 1 to 3
      collect (loop for y from 1 to x
		    collect y))
;; ((1) (1 2) (1 2 3))
~~~

### iterate
~~~lisp
(iter outer
   (for i below 2)
   (iter (for j below 3)
      (in outer (collect (list i j)))))
;; ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2))
~~~

### Series
~~~lisp
(collect
  (mapping ((x (scan-range :from 1 :upto 3)))
    (collect (scan-range :from 1 :upto x))))
~~~


## Computing an intermediate value

with `=`:

~~~lisp
(loop for x from 1 to 3
      for y = (* x 10)
      collect y)
;; (10 20 30)
~~~

## Loop with a counter
### loop
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

### Series

By iterating on multiple series in parallel, and using an infinite
range, we can make a counter.

~~~lisp
(iterate ((x (scan '(a b c d e)))
          (y (scan-range :from 1)))
  (when (> y 1) (format t ", "))
  (format t "~A" x))
~~~

## Ascending, descending order, limits
### loop

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

### Series

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


## Steps
### loop

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
      do (print i))
~~~

### Series
with `:by`
~~~lisp
(iterate ((i (scan-range :from 1 :upto 10 :by 2)))
  (print i))
~~~


## Loop and conditionals
### loop

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

### iterate

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

### Series

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


## Terminate the loop with a test (until, while)
### loop

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

### Series

We truncate the series with `until-if`, then collect from its result.

~~~lisp
(collect
  (until-if (lambda (i) (> i 3))
            (scan '(1 2 3 4 5))))
~~~

## Loop, print and return a result
### loop

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

### Series
By mapping we can perform a side effect and also collect items
~~~lisp
(collect
  (mapping ((x (until-if (complement (lambda (x) (< x 4)))
                         (scan '(1 2 3 4 5)))))
    (format t "x is ~a~&" x)
    x))
~~~


## Named loops and early exit
### loop

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

### Loop shorthands for when/return

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

### Series

A block is manually created and returned from.

~~~lisp
(block loop-1
  (iterate ((x (scan-range :from 0 :upto 10 :by 2)))
    (iterate ((y (scan-range :from 0 :upto 100 :by (1+ (random 3)))))
      (when (< x y)
        (return-from loop-1 (values x y))))))
~~~

## Count
### loop
~~~lisp
(loop for i from 1 to 3 count (oddp i))
;; 2
~~~

### Series
~~~lisp
(collect-length (choose-if #'oddp (scan-range :from 1 :upto 3)))
~~~

## Summation
### loop

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


### Series

~~~lisp
(collect-sum (#M(lambda (i) (* i i))
                (scan-range :from 1 :upto 3)))
~~~

## max, min
### loop

~~~lisp
(loop for i from 1 to 3 maximize (mod i 3))
;; 2
~~~

and `minimize`.

### Series
~~~lisp
(collect-max (#M(lambda (i) (mod i 3))
                (scan-range :from 1 :upto 3)))
~~~
and `collect-min`.

## Destructuring, aka pattern matching against the list or dotted pairs
### loop

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

### Series
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

# Custom series scanners

If we often scan the same type of object, we can write our own scanner
 for it: the iteration itself can be factored out. Taking the example
 above, of scanning a list of two-element lists, we'll write a scanner
 that returns a series of the first elements, and a series of the
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

# Shorter series expressions

Consider this series expression:

~~~lisp

(collect-sum (mapping ((i (scan-range :length 5)))
                    (* i 2)))
~~~

It's a bit longer than it needs to be—the `mapping` form's only
purpose is to bind the variable `i`, and `i` is used in only one
place. Series has a "hidden feature" which allows us to simplify this
expression to the following:

~~~lisp
(collect-sum (* 2 (scan-range :length 5)))
~~~

This is called implicit mapping, and can be enabled in the call to
`series::install`:

~~~lisp
(series::install :implicit-map t)
~~~

When using implicit mapping, the `#M` reader macro demonstrated above
becomes redundant.

# Loop gotchas

- the keyword `it`, often used in functional constructs, can be
  recognized as a loop keyword. Don't use it inside a loop.


# Appendix: list of loop keywords

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
counting sum summing maximize return
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

# Credit and references

## Loop

* [Tutorial for the Common Lisp Loop Macro](http://www.ai.sri.com/~pkarp/loop.html) by Peter D. Karp
* [http://www.unixuser.org/~euske/doc/cl/loop.html](http://www.unixuser.org/~euske/doc/cl/loop.html)
* [riptutorial.com](https://riptutorial.com/common-lisp/)
*

## Iterate

* [The Iterate Manual](https://common-lisp.net/project/iterate/doc/index.html) -
* [iterate](https://digikar99.github.io/cl-iterate-docs/) - highlights at a glance and examples
* [Loop v Iterate - SabraOnTheHill](https://sites.google.com/site/sabraonthehill/loop-v-iter)

## Series

* [SERIES for Common Lisp - Richard C. Waters](http://series.sourceforge.net/)

## Others

* See also: [more functional constructs](https://lisp-journey.gitlab.io/blog/snippets-functional-style-more/) (do-repeat, take,…)
