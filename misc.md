---
title: Miscellaneous
---


<a name="opt"></a>

## Re-using complex data structures

Sometimes you want your functions to behave in a 'functional' way, i.e. return [fresh](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#fresh) results without side effects, sometimes you want them to re-use and modify existing data in a destructive way - consider the difference between [`append`](http://www.lispworks.com/documentation/HyperSpec/Body/f_append.htm) and [`nconc`](http://www.lispworks.com/documentation/HyperSpec/Body/f_nconc.htm) for an example.

Well, you can have your cake and eat it too, by using optional (or keyword) parameters. Here's an example: Let's assume you're writing a function `complex-matrix-stuff` which takes two matrices `m1` and `m2` as its arguments and computes and returns a resulting matrix the size of which depends on `m1` and `m2`, i.e. for a fresh result you'll need an empty matrix which'll be created by, say, `(make-appropriate-result-matrix-for m1 m2)`.

The classical textbook way to implement this function will more or less look like this:

~~~lisp
(defun complex-matrix-stuff (m1 m2)
  (let ((result (make-appropriate-result-matrix-for m1 m2)))
    ;; ... compute storing the results in RESULT
    result))
~~~

And you'll use it like this:

~~~lisp
(setq some-matrix (complex-matrix-stuff A B))
~~~

But why not write it like so:

~~~lisp
(defun complex-matrix-stuff (m1 m2
                             &optional
                             (result
                              (make-appropriate-result-matrix-for m1 m2)))
  ;; ... compute storing the results in RESULT
  result)
~~~

Now you have it both ways. You can still "make up results" on the fly as in:

~~~lisp
(setq some-matrix (complex-matrix-stuff A B))
~~~

But you can also (destructively) re-use previously allocated matrices:

~~~lisp
(complex-matrix-stuff A B some-appropriate-matrix-I-built-before)
~~~

Or use your function like this:

~~~lisp
(setq some-other-matrix
      (complex-matrix-stuff A B some-appropriate-matrix-I-built-before))
~~~

in which case you'll end up with:

~~~lisp
* (eq some-other-matrix some-appropriate-matrix-I-built-before)

T
~~~


<a name="adjust"></a>

## Using `ADJUST-ARRAY` instead of consing up new sequences with `SUBSEQ`

Most CL functions operating on sequences will accept `start` and `end` keywords so you can make them operate on a sub-sequence without actually creating it, i.e. instead of

~~~lisp
(count #\a (subseq long-string from to))
~~~

you should of course use

~~~lisp
(count #\a long-string :start from :end to)
~~~

which'll yield the same result but not create an unnecessary intermediate sub-sequence.

However, sometimes it looks like you can't avoid creating new data. Consider a hash table the keys of which are strings. If the key you're looking for is a sub-string of another string you'll most likely end up with

~~~lisp
(gethash (subseq original-string from to)
         hash-table)
~~~

But you don't have to. You can create _one_ [displaced](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#displaced_array) string and reuse it multiple times with [`adjust-array`](http://www.lispworks.com/documentation/HyperSpec/Body/f_adjust.htm):

~~~lisp
(let ((substring (make-array 0
                             :element-type 'character
                             :displaced-to ""
                             :displaced-index-offset 0)))
  ;; more code
  (gethash
   (adjust-array substring (- to from)
                 :displaced-to original-string
                 :displaced-index-offset from)
   hash-table)
  ;; even more code
  )
~~~
