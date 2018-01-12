Multidimensional arrays
=======================

Common Lisp has native support for multidimensional arrays, with some special treatment
for 1-D arrays, called `vectors`. Arrays can be *generalised* and contain any type (`element-type t`), or they
can be *specialised* to contain specific types such as `single-float` or `integer`. 

* [Practical Common Lisp Chapter 11, Collections](http://www.gigamonkeys.com/book/collections.html) by Peter Seibel

There are limitations to the native arrays, in particular

* Interoperabiltiy with foreign language arrays, for example when calling libraries such as BLAS, LAPACK or GSL.
* Extending arithmetic and other mathematical operators to handle arrays, for example so that `(+ a b)` works
  when `a` and/or `b` are arrays. 

Both of these problems can be solved by using CLOS to define an extended array class, with native arrays as a special case.
Some libraries available through `quicklisp` which take this approach are:

* [matlisp](https://github.com/matlisp/matlisp)
* [Antik](https://www.common-lisp.net/project/antik/)
* [cl-ana](https://github.com/ghollisjr/cl-ana/wiki)

Taking this approach further, full DSLs have been built on Common Lisp for treating mathematical objects.
At the time of writing the most widely used and supported of these are:

* [Maxima](http://maxima.sourceforge.net/documentation.html)
* [Axiom](https://github.com/daly/axiom)

Construction
------------

* [CHLS: make-array](http://clhs.lisp.se/Body/f_mk_ar.htm)


Array containing uniform random numbers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

```
(defun rand (dimensions &key (element-type 'single-float))
  "Makes an array of shape DIMENSIONS, filled with random numbers
   uniformly distributed between 0 and 1.

   Uses the built-in RANDOM function.
   
   (rand 3)  -> #(0.39319038 0.69693553 0.5021677)
   (rand '(2 2)) -> #2A((0.91003513 0.23208928) (0.5577954 0.94657767))
   "
  (let* ((arr (make-array dimensions :element-type element-type))
         (size (array-total-size arr)))
    (dotimes (i size)
      (setf (row-major-aref arr i) (coerce (random 1.0) element-type)))
    arr))
```

Array containing a range of values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Often called `linspace` in other languages, this can be defined as

```
(defun linspace (start stop n)
  "Make a vector of N elements with first element START and last element STOP
  
  The element type is set by the type of (STOP - START) / (N - 1)

  (linspace 0 4 5) -> #(0 1 2 3 4)
  (linspace 1 3 5) -> #(0 1/2 1 3/2 2)
  (linspace 0 4d0 3) -> #(0.0d0 2.0d0 4.0d0)
  "
  (let* ((delta (/ (- stop start) (- n 1))) ; Difference between values
         (type (case (type-of delta)
                 ('bit 'integer)   ; If delta is 1 then becomes bit rather than integer
                 (otherwise (type-of delta))))
         (result (make-array n :element-type type)))
    (dotimes (i n)
      (setf (aref result i) (* delta i)))
    result))
```


Foreign arrays
--------------

