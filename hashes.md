---
title: Hash Tables
---

<a name="intro"></a>

##Introduction

Hash Tables are a powerful data structure, associating keys with values in a very efficient way. Hash Tables are often preferred over association lists whenever performance is an issue, but they introduce a little overhead that makes assoc lists better if there are only a few key-value pairs to maintain.


<a name="create"></a>

##Creating a Hash Table

Hash Tables are created using the function [`make-hash-table`](http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_has.htm). It has no required argument. Its most used optional keyword argument is `:test`, specifying the function used to test the equality of keys.


<a name="get"></a>

##Getting a value from a Hash Table

The function [`gethash`](http://www.lispworks.com/documentation/HyperSpec/Body/f_gethas.htm) takes two required arguments: a key and a hash table. It returns two values: the value corresponding to the key in the hash table (or `nil` if not found), and a boolean indicating whether the key was found in the table. That second value is necessary since `nil` is a valid value in a key-value pair, so getting `nil` as first value from `gethash` does not necessarily mean that the key was not found in the table.


<a name="add"></a>

##Adding an Element to a Hash Table

If you want to add an element to a hash table, you can use `gethash`, the function to retrieve elements from the hash table, in conjunction with [`setf`](http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm).

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


<a name="test"></a>

##Testing for the Presence of a Key in a Hash Table

The first value returned by `gethash` is the object in the hash table that's associated with the key you provided as an argument to `gethash` or `nil` if no value exists for this key. This value can act as a [generalized boolean](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#generalized_boolean">generalized boolean) if you want to test for the presence of keys.

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

But note that this does _not_ work if `nil` is amongst the values that you want to store in the hash.

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

##Deleting from a Hash Table

Use [`remhash`](http://www.lispworks.com/documentation/HyperSpec/Body/f_remhas.htm) to delete a hash entry. Both the key and its associated value will be removed from the hash table. `remhash` returns T if there was such an entry, `nil` otherwise.

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


<a name="traverse"></a>

##Traversing a Hash Table

If you want to perform an action on each entry (i.e., each key-value pair) in a hash table, you have several options:

You can use [`maphash`](http://www.lispworks.com/documentation/HyperSpec/Body/f_maphas.htm) which iterates over all entries in the hash table. Its first argument must be a function which accepts _two_ arguments, the key and the value of each entry. Note that due to the nature of hash tables you _can't_ control the order in which the entries are provided by `maphash` (or other traversing constructs). `maphash` always returns `nil`.

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

You can also use [`with-hash-table-iterator`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_hash.htm), a macro which turns (via [`macrolet`](http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm)) its first argument into an iterator that on each invocation returns three values per hash table entry - a generalized boolean that's true if an entry is returned, the key of the entry, and the value of the entry. If there are no more entries, only one value is returned - `nil`.

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

Note the following caveat from the HyperSpec: "It is unspecified what happens if any of the implicit interior state of an iteration is returned outside the dynamic extent of the `with-hash-table-iterator` form such as by returning some closure over the invocation form."


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


<a name="count"></a>

##Counting the Entries in a Hash Table

No need to use your fingers - Common Lisp has a built-in function to do it for you: [`hash-table-count`](http://www.lispworks.com/documentation/HyperSpec/Body/f_hash_1.htm).

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


<a name="size"></a>

##Performance Issues: The Size of your Hash Table

The `make-hash-table` function has a couple of optional parameters which control the initial size of your hash table and how it'll grow if it needs to grow. This can be an important performance issue if you're working with large hash tables. Here's an (admittedly not very scientific) example with [CMUCL](http://www.cons.org/cmucl) pre-18d on Linux:

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

The values for [`hash-table-size`](http://www.lispworks.com/documentation/HyperSpec/Body/f_hash_4.htm) and [`hash-table-rehash-size`](http://www.lispworks.com/documentation/HyperSpec/Body/f_hash_2.htm) are implementation-dependent. In our case, CMUCL chooses and initial size of 65, and it will increase the size of the hash by 50 percent whenever it needs to grow. Let's see how often we have to re-size the hash until we reach the final size...

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

The hash has to be re-sized 19 times until it's big enough to hold 100,000 entries. That explains why we saw a lot of consing and why it took rather long to fill the hash table. It also explains why the second run was much faster - the hash table already had the correct size.

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

That's obviously much faster. And there was no consing involved because we didn't have to re-size at all. If we don't know the final size in advance but can guess the growth behaviour of our hash table we can also provide this value to `make-hash-table`. We can provide an integer to specify absolute growth or a float to specify relative growth.

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

Also rather fast (we only needed one re-size) but much more consing because almost the whole hash table (minus 65 initial elements) had to be built during the loop.

Note that you can also specify the `rehash-threshold` while creating a new hash table. One final remark: Your implementation is allowed to _completely ignore_ the values provided for `rehash-size` and `rehash-threshold`...
