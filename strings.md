---
title: Strings
---

The most important thing to know about strings in Common Lisp is probably that
they are arrays and thus also sequences. This implies that all concepts that are
applicable to arrays and sequences also apply to strings. If you can't find a
particular string function, make sure you've also searched for the more general
[array or sequence functions](http://www.gigamonkeys.com/book/collections.html). We'll only cover a fraction of what can be done
with and to strings here.

ASDF3, which is included with almost all Common Lisp implementations,
includes
[Utilities for Implementation- and OS- Portability (UIOP)](https://gitlab.common-lisp.net/asdf/asdf/blob/master/uiop/README.md),
which defines functions to work on strings (`strcat`,
`string-prefix-p`, `string-enclosed-p`, `first-char`, `last-char`,
`split-string`, `stripln`).

Some external libraries available on Quicklisp bring some more
functionality or some shorter ways to do.

- [str](https://github.com/vindarel/cl-str) defines `trim`, `words`,
  `unwords`, `lines`, `unlines`, `concat`, `split`, `shorten`, `repeat`,
  `replace-all`, `starts-with?`, `ends-with?`, `blankp`, `emptyp`, …
- [Serapeum](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#strings) is a large set of utilities with many string manipulation functions.
- [cl-change-case](https://github.com/rudolfochrist/cl-change-case)
  has functions to convert strings between camelCase, param-case,
  snake_case and more. They are also included into `str`.
- [mk-string-metrics](https://github.com/cbaggers/mk-string-metrics)
  has functions to calculate various string metrics efficiently
  (Damerau-Levenshtein, Hamming, Jaro, Jaro-Winkler, Levenshtein, etc),
- and `cl-ppcre` can come in handy, for example
  `ppcre:replace-regexp-all`. See the [regexp](regexp.html) section.


Last but not least, when you'll need to tackle the `format` construct,
don't miss the following resources:

* the official [CLHS documentation](http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm)
* a [quick reference](http://clqr.boundp.org/)
* a [CLHS summary on HexstreamSoft](https://www.hexstreamsoft.com/articles/common-lisp-format-reference/clhs-summary/#subsections-summary-table)
* plus a Slime tip: type `C-c C-d ~` plus a letter of a format directive to open up its documentation. Again more useful with `ivy-mode` or `helm-mode`.

# Creating strings

A string is created with double quotes, all right, but we can recall
these other ways:

- using `format nil` doesn't *print* but returns a new string (see
  more examples of `format` below):

~~~lisp
(defparameter *person* "you")
(format nil "hello ~a" *person*) ;; => "hello you"
~~~

- `make-string count` creates a string of the given length. The
  `:initial-element` character is repeated `count` times:

~~~lisp
(make-string 3 :initial-element #\♥) ;; => "♥♥♥"
~~~


# Accessing Substrings

As a string is a sequence, you can access substrings with the SUBSEQ
function. The index into the string is, as always, zero-based. The third,
optional, argument is the index of the first character which is not a part of
the substring, it is not the length of the substring.

~~~lisp
* (defparameter *my-string* (string "Groucho Marx"))
*MY-STRING*
* (subseq *my-string* 8)
"Marx"
* (subseq *my-string* 0 7)
"Groucho"
* (subseq *my-string* 1 5)
"rouc"
~~~

You can also manipulate the substring if you use SUBSEQ together with SETF.

~~~lisp
* (defparameter *my-string* (string "Harpo Marx"))
*MY-STRING*
* (subseq *my-string* 0 5)
"Harpo"
* (setf (subseq *my-string* 0 5) "Chico")
"Chico"
* *my-string*
"Chico Marx"
~~~

But note that the string isn't "stretchable". To cite from the HyperSpec: "If
the subsequence and the new sequence are not of equal length, the shorter length
determines the number of elements that are replaced." For example:

~~~lisp
* (defparameter *my-string* (string "Karl Marx"))
*MY-STRING*
* (subseq *my-string* 0 4)
"Karl"
* (setf (subseq *my-string* 0 4) "Harpo")
"Harpo"
* *my-string*
"Harp Marx"
* (subseq *my-string* 4)
" Marx"
* (setf (subseq *my-string* 4) "o Marx")
"o Marx"
* *my-string*
"Harpo Mar"
~~~

# Accessing Individual Characters

You can use the function CHAR to access individual characters of a string. CHAR
can also be used in conjunction with SETF.

~~~lisp
* (defparameter *my-string* (string "Groucho Marx"))
*MY-STRING*
* (char *my-string* 11)
#\x
* (char *my-string* 7)
#\Space
* (char *my-string* 6)
#\o
* (setf (char *my-string* 6) #\y)
#\y
* *my-string*
"Grouchy Marx"
~~~

Note that there's also SCHAR. If efficiency is important, SCHAR can be a bit
faster where appropriate.

Because strings are arrays and thus sequences, you can also use the more generic
functions AREF and ELT (which are more general while CHAR might be implemented
more efficiently).

~~~lisp
* (defparameter *my-string* (string "Groucho Marx"))
*MY-STRING*
* (aref *my-string* 3)
#\u
* (elt *my-string* 8)
#\M
~~~

Each character in a string has an integer code. The range of recognized codes
and Lisp's ability to print them is directed related to your implementation's
character set support, e.g. ISO-8859-1, or Unicode. Here are some examples in
SBCL of UTF-8 which encodes characters as 1 to 4 8 bit bytes. The first example
shows a character outside the first 128 chars, or what is considered the normal
Latin character set. The second example shows a multibyte encoding (beyond the
value 255). Notice the Lisp reader can round-trip characters by name.

~~~lisp
* (stream-external-format *standard-output*)

:UTF-8
* (code-char 200)

#\LATIN_CAPITAL_LETTER_E_WITH_GRAVE
* (char-code #\LATIN_CAPITAL_LETTER_E_WITH_GRAVE)

200
* (code-char 1488)
#\HEBREW_LETTER_ALEF

* (char-code #\HEBREW_LETTER_ALEF)
1488
~~~

Check out the UTF-8 Wikipedia article for the range of supported characters and
their encodings.

# Manipulating Parts of a String

There's a slew of (sequence) functions that can be used to manipulate a string
and we'll only provide some examples here. See the sequences dictionary in the
HyperSpec for more.

~~~lisp
* (remove #\o "Harpo Marx")
"Harp Marx"
* (remove #\a "Harpo Marx")
"Hrpo Mrx"
* (remove #\a "Harpo Marx" :start 2)
"Harpo Mrx"
* (remove-if #'upper-case-p "Harpo Marx")
"arpo arx"
* (substitute #\u #\o "Groucho Marx")
"Gruuchu Marx"
* (substitute-if #\_ #'upper-case-p "Groucho Marx")
"_roucho _arx"
* (defparameter *my-string* (string "Zeppo Marx"))
*MY-STRING*
* (replace *my-string* "Harpo" :end1 5)
"Harpo Marx"
* *my-string*
"Harpo Marx"
~~~

Another function that can be frequently used (but not part of the ANSI standard)
is replace-all. This function provides an easy functionality for search/replace
operations on a string, by returning a new string in which all the occurrences of
the 'part' in string is replaced with 'replacement'".

~~~lisp
* (replace-all "Groucho Marx Groucho" "Groucho" "ReplacementForGroucho")
"ReplacementForGroucho Marx ReplacementForGroucho"
~~~

One of the implementations of replace-all is as follows:

~~~lisp
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurrences of the part
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))
~~~

However, bear in mind that the above code is not optimized for long strings; if
you intend to perform such an operation on very long strings, files, etc. please
consider using cl-ppcre regular expressions and string processing library which
is heavily optimized.

# Concatenating Strings

The name says it all: CONCATENATE is your friend. Note that this is a generic
sequence function and you have to provide the result type as the first argument.

~~~lisp
* (concatenate 'string "Karl" " " "Marx")
"Karl Marx"
* (concatenate 'list "Karl" " " "Marx")
(#\K #\a #\r #\l #\Space #\M #\a #\r #\x)
~~~

With UIOP, use `strcat`:

~~~lisp
* (uiop:strcat "karl" " " marx")
~~~

or with the library `str`, use `concat`:

~~~lisp
* (str:concat "foo" "bar")
~~~

If you have to construct a string out of many parts, all of these calls to
CONCATENATE seem wasteful, though. There are at least three other good ways to
construct a string piecemeal, depending on what exactly your data is. If you
build your string one character at a time, make it an adjustable VECTOR (a
one-dimensional ARRAY) of type character with a fill-pointer of zero, then use
VECTOR-PUSH-EXTEND on it. That way, you can also give hints to the system if you
can estimate how long the string will be. (See the optional third argument to
VECTOR-PUSH-EXTEND.)

~~~lisp
* (defparameter *my-string* (make-array 0
                                        :element-type 'character
                                        :fill-pointer 0
                                        :adjustable t))
*MY-STRING*
* *my-string*
""
* (dolist (char '(#\Z #\a #\p #\p #\a))
    (vector-push-extend char *my-string*))
NIL
* *my-string*
"Zappa"
~~~

If the string will be constructed out of (the printed representations of)
arbitrary objects, (symbols, numbers, characters, strings, ...), you can use
FORMAT with an output stream argument of NIL. This directs FORMAT to return the
indicated output as a string.

~~~lisp
* (format nil "This is a string with a list ~A in it"
          '(1 2 3))
"This is a string with a list (1 2 3) in it"
~~~

We can use the looping constructs of the FORMAT mini language to emulate
CONCATENATE.

~~~lisp
* (format nil "The Marx brothers are:~{ ~A~}."
          '("Groucho" "Harpo" "Chico" "Zeppo" "Karl"))
"The Marx brothers are: Groucho Harpo Chico Zeppo Karl."
~~~

FORMAT can do a lot more processing but it has a relatively arcane syntax. After
this last example, you can find the details in the CLHS section about formatted
output.

~~~lisp
* (format nil "The Marx brothers are:~{ ~A~^,~}."
          '("Groucho" "Harpo" "Chico" "Zeppo" "Karl"))
"The Marx brothers are: Groucho, Harpo, Chico, Zeppo, Karl."
~~~

Another way to create a string out of the printed representation of various
object is using WITH-OUTPUT-TO-STRING. The value of this handy macro is a string
containing everything that was output to the string stream within the body to
the macro. This means you also have the full power of FORMAT at your disposal,
should you need it.

~~~lisp
* (with-output-to-string (stream)
    (dolist (char '(#\Z #\a #\p #\p #\a #\, #\Space))
      (princ char stream))
    (format stream "~S - ~S" 1940 1993))
"Zappa, 1940 - 1993"
~~~

# Processing a String One Character at a Time

Use the MAP function to process a string one character at a time.

~~~lisp
* (defparameter *my-string* (string "Groucho Marx"))
*MY-STRING*
* (map 'string #'(lambda (c) (print c)) *my-string*)
#\G
#\r
#\o
#\u
#\c
#\h
#\o
#\Space
#\M
#\a
#\r
#\x
"Groucho Marx"
~~~

Or do it with LOOP.

~~~lisp
* (loop for char across "Zeppo"
        collect char)
(#\Z #\e #\p #\p #\o)
~~~

# Reversing a String by Word or Character

Reversing a string by character is easy using the built-in REVERSE function (or
its destructive counterpart NREVERSE).

~~~lisp
*(defparameter *my-string* (string "DSL"))
*MY-STRING*
* (reverse *my-string*)
"LSD"
~~~

There's no one-liner in CL to reverse a string by word (like you would do it in
Perl with split and join). You either have to use functions from an external
library like SPLIT-SEQUENCE or you have to roll your own solution.

Here's an attempt with the `str` library:

~~~lisp
* (defparameter *singing* "singing in the rain")
*SINGING*
* (str:words *SINGING*)
("singing" "in" "the" "rain")
* (reverse *)
("rain" "the" "in" "singing")
* (str:unwords *)
"rain the in singing"
~~~

And here's another one with no external dependencies:

~~~lisp
* (defun split-by-one-space (string)
    "Returns a list of substrings of string
    divided by ONE space each.
    Note: Two consecutive spaces will be seen as
    if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))
SPLIT-BY-ONE-SPACE
* (split-by-one-space "Singing in the rain")
("Singing" "in" "the" "rain")
* (split-by-one-space "Singing in the  rain")
("Singing" "in" "the" "" "rain")
* (split-by-one-space "Cool")
("Cool")
* (split-by-one-space " Cool ")
("" "Cool" "")
* (defun join-string-list (string-list)
    "Concatenates a list of strings
and puts spaces between the elements."
    (format nil "~{~A~^ ~}" string-list))
JOIN-STRING-LIST
* (join-string-list '("We" "want" "better" "examples"))
"We want better examples"
* (join-string-list '("Really"))
"Really"
* (join-string-list '())
""
* (join-string-list
   (nreverse
    (split-by-one-space
     "Reverse this sentence by word")))
"word by sentence this Reverse"
~~~

# Controlling Case

Common Lisp has a couple of functions to control the case of a string.

~~~lisp
* (string-upcase "cool")
"COOL"
* (string-upcase "Cool")
"COOL"
* (string-downcase "COOL")
"cool"
* (string-downcase "Cool")
"cool"
* (string-capitalize "cool")
"Cool"
* (string-capitalize "cool example")
"Cool Example"
~~~

These functions take the `:start` and `:end` keyword arguments so you can optionally
only manipulate a part of the string. They also have destructive counterparts
whose names starts with "N".

~~~lisp
* (string-capitalize "cool example" :start 5)
"cool Example"
* (string-capitalize "cool example" :end 5)
"Cool example"
* (defparameter *my-string* (string "BIG"))
*MY-STRING*
* (defparameter *my-downcase-string* (nstring-downcase *my-string*))
*MY-DOWNCASE-STRING*
* *my-downcase-string*
"big"
* *my-string*
"big"
~~~

Note this potential caveat: according to the HyperSpec,

> for STRING-UPCASE, STRING-DOWNCASE, and STRING-CAPITALIZE, string is not modified. However, if no characters in string require conversion, the result may be either string or a copy of it, at the implementation's discretion.

This implies that the last result in
the following example is implementation-dependent - it may either be "BIG" or
"BUG". If you want to be sure, use COPY-SEQ.

~~~lisp
* (defparameter *my-string* (string "BIG"))
*MY-STRING*
* (defparameter *my-upcase-string* (string-upcase *my-string*))
*MY-UPCASE-STRING*
* (setf (char *my-string* 1) #\U)
#\U
* *my-string*
"BUG"
* *my-upcase-string*
"BIG"
~~~

## With the format function

The format function has directives to change the case of words:

### To lower case: ~( ~)

~~~lisp
(format t "~(~a~)" "HELLO WORLD")
;; => hello world
~~~


### Capitalize every word: ~:( ~)

~~~lisp
(format t "~:(~a~)" "HELLO WORLD")
Hello World
NIL
~~~

### Capitalize the first word: ~@( ~)

~~~lisp
(format t "~@(~a~)" "hello world")
Hello world
NIL
~~~

### To upper case: ~@:( ~)

Where we re-use the colon and the @:

~~~lisp
(format t "~@:(~a~)" "hello world")
HELLO WORLD
NIL
~~~


# Trimming Blanks from the Ends of a String

Not only can you trim blanks, but you can get rid of arbitrary characters. The
functions STRING-TRIM, STRING-LEFT-TRIM and STRING-RIGHT-TRIM return a substring
of their second argument where all characters that are in the first argument are
removed off the beginning and/or the end. The first argument can be any sequence
of characters.

~~~lisp
* (string-trim " " " trim me ")
"trim me"
* (string-trim " et" " trim me ")
"rim m"
* (string-left-trim " et" " trim me ")
"rim me "
* (string-right-trim " et" " trim me ")
" trim m"
* (string-right-trim '(#\Space #\e #\t) " trim me ")
" trim m"
* (string-right-trim '(#\Space #\e #\t #\m) " trim me ")
~~~

Note: The caveat mentioned in the section about Controlling Case also applies
here.

# Converting between Symbols and Strings

The function INTERN will "convert" a string to a symbol. Actually, it will check
whether the symbol denoted by the string (its first argument) is already
accessible in the package (its second, optional, argument which defaults to the
current package) and enter it, if necessary, into this package. It is beyond the
scope of this chapter to explain all the concepts involved and to address the
second return value of this function. See the CLHS chapter about packages for
details.

Note that the case of the string is relevant.

~~~lisp
* (in-package "COMMON-LISP-USER")
#<The COMMON-LISP-USER package, 35/44 internal, 0/9 external>
* (intern "MY-SYMBOL")
MY-SYMBOL
NIL
* (intern "MY-SYMBOL")
MY-SYMBOL
:INTERNAL
* (export 'MY-SYMBOL)
T
* (intern "MY-SYMBOL")
MY-SYMBOL
:EXTERNAL
* (intern "My-Symbol")
|My-Symbol|
NIL
* (intern "MY-SYMBOL" "KEYWORD")
:MY-SYMBOL
NIL
* (intern "MY-SYMBOL" "KEYWORD")
:MY-SYMBOL
:EXTERNAL
~~~

To do the opposite, convert from a symbol to a string, use SYMBOL-NAME or
STRING.

~~~lisp
* (symbol-name 'MY-SYMBOL)
"MY-SYMBOL"
* (symbol-name 'my-symbol)
"MY-SYMBOL"
* (symbol-name '|my-symbol|)
"my-symbol"
* (string 'howdy)
"HOWDY"
~~~

# Converting between Characters and Strings

You can use COERCE to convert a string of length 1 to a character. You can also
use COERCE to convert any sequence of characters into a string. You can not use
COERCE to convert a character to a string, though - you'll have to use STRING
instead.

~~~lisp
* (coerce "a" 'character)
#\a
* (coerce (subseq "cool" 2 3) 'character)
#\o
* (coerce "cool" 'list)
(#\c #\o #\o #\l)
* (coerce '(#\h #\e #\y) 'string)
"hey"
* (coerce (nth 2 '(#\h #\e #\y)) 'character)
#\y
* (defparameter *my-array* (make-array 5 :initial-element #\x))
*MY-ARRAY*
* *my-array*
#(#\x #\x #\x #\x #\x)
* (coerce *my-array* 'string)
"xxxxx"
* (string 'howdy)
"HOWDY"
* (string #\y)
"y"
* (coerce #\y 'string)
#\y can't be converted to type STRING.
   [Condition of type SIMPLE-TYPE-ERROR]
~~~

# Finding an Element of a String

Use FIND, POSITION, and their -IF counterparts to find characters in a string.

~~~lisp
* (find #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equal)
#\t
* (find #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equalp)
#\T
* (find #\z "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equalp)
NIL
* (find-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks.")
#\1
* (find-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks." :from-end t)
#\0
* (position #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equal)
17
* (position #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equalp)
0
* (position-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks.")
37
* (position-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks." :from-end t)
43
~~~

Or use COUNT and friends to count characters in a string.

~~~lisp
* (count #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equal)
2
* (count #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equalp)
3
* (count-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks.")
6
* (count-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks." :start 38)
5
~~~

# Finding a Substring of a String

The function SEARCH can find substrings of a string.

~~~lisp
* (search "we" "If we can't be free we can at least be cheap")
3
* (search "we" "If we can't be free we can at least be cheap" :from-end t)
20
* (search "we" "If we can't be free we can at least be cheap" :start2 4)
20
* (search "we" "If we can't be free we can at least be cheap" :end2 5 :from-end t)
3
* (search "FREE" "If we can't be free we can at least be cheap")
NIL
* (search "FREE" "If we can't be free we can at least be cheap" :test #'char-equal)
15
~~~

# Converting a String to a Number

## To an integer: parse-integer

CL provides the `parse-integer` function to convert a string representation of an integer
to the corresponding numeric value. The second return value is the index into
the string where the parsing stopped.

~~~lisp
* (parse-integer "42")
42
2
* (parse-integer "42" :start 1)
2
2
* (parse-integer "42" :end 1)
4
1
* (parse-integer "42" :radix 8)
34
2
* (parse-integer " 42 ")
42
3
* (parse-integer " 42 is forty-two" :junk-allowed t)
42
3
* (parse-integer " 42 is forty-two")

Error in function PARSE-INTEGER:
   There's junk in this string: " 42 is forty-two".
~~~

`parse-integer` doesn't understand radix specifiers like `#X`, nor is there a
built-in function to parse other numeric types. You could use `read-from-string`
in this case.


## To any number: read-from-string

Be aware that the full reader is in effect if you're using this
function. This can lead to vulnerability issues.

~~~lisp
* (read-from-string "#X23")
35
4
* (read-from-string "4.5")
4.5
3
* (read-from-string "6/8")
3/4
3
* (read-from-string "#C(6/8 1)")
#C(3/4 1)
9
* (read-from-string "1.2e2")
120.00001
5
* (read-from-string "symbol")
SYMBOL
6
* (defparameter *foo* 42)
*FOO*
* (read-from-string "#.(setq *foo* \"gotcha\")")
"gotcha"
23
* *foo*
"gotcha"
~~~

## To a float: the parse-float library

There is no built-in function similar to `parse-integer` to parse
other number types. The external library
[parse-float](https://github.com/soemraws/parse-float) does exactly
that. It doesn't use `read-from-string` so it is safe to use.

~~~lisp
(ql:quickload "parse-float")
(parse-float:parse-float "1.2e2")
;; 120.00001
;; 5
~~~

LispWorks also has a [parse-float](http://www.lispworks.com/documentation/lw51/LWRM/html/lwref-228.htm) function.

See also [parse-number](https://github.com/sharplispers/parse-number).


# Converting a Number to a String

The general function WRITE-TO-STRING or one of its simpler variants
PRIN1-TO-STRING or PRINC-TO-STRING may be used to convert a number to a
string. With WRITE-TO-STRING, the :base keyword argument may be used to change
the output base for a single call. To change the output base globally, set
*print-base* which defaults to 10. Remember in Lisp, rational numbers are
represented as quotients of two integers even when converted to strings.

~~~lisp
* (write-to-string 250)
"250"
* (write-to-string 250.02)
"250.02"
* (write-to-string 250 :base 5)
"2000"
* (write-to-string (/ 1 3))
"1/3"
*
~~~

# Comparing Strings

The general functions EQUAL and EQUALP can be used to test whether two strings
are equal. The strings are compared element-by-element, either in a
case-sensitive manner (EQUAL) or not (EQUALP). There's also a bunch of
string-specific comparison functions. You'll want to use these if you're
deploying implementation-defined attributes of characters. Check your vendor's
documentation in this case.

Here are a few examples. Note that all functions that test for inequality return the position of the first mismatch as a generalized boolean. You can also use the generic sequence function MISMATCH if you need more versatility.

~~~lisp
* (string= "Marx" "Marx")
T
* (string= "Marx" "marx")
NIL
* (string-equal "Marx" "marx")
T
* (string< "Groucho" "Zeppo")
0
* (string< "groucho" "Zeppo")
NIL
* (string-lessp "groucho" "Zeppo")
0
* (mismatch "Harpo Marx" "Zeppo Marx" :from-end t :test #'char=)
3
~~~

# String formatting

The `format` function has a lot of directives to print strings,
numbers, lists, going recursively, even calling Lisp functions,
etc. We'll focus here on a few things to print and format strings.

The need of our examples arise when we want to print many strings and
justify them. Let's work with this list of movies:

~~~lisp
(defparameter movies '(
    (1 "Matrix" 5)
    (10 "Matrix Trilogy swe sub" 3.3)
    ))
~~~

We want an aligned and justified result like this:

```
 1 Matrix                  5
10 Matrix Trilogy swe sub  3.3
```

We'll use `mapcar` to iterate over our movies and experiment with the
format constructs.

~~~lisp
(mapcar (lambda (it)
          (format t "~a ~a ~a~%" (first it) (second it) (third it)))
        movies)
~~~

which prints:

```
1 fooo baaar 5
10 last 3.3
```


## Structure of format

Format directives start with `~`. A final character like `A` or `a`
(they are case insensitive) defines the directive. In between, it can
accept coma-separated options and parameters.

Print a tilde with `~~`, or 10 with `~10~`.

Other directives include:

- `R`: Roman (e.g., prints in English): `(format t "~R" 20)` => "twenty".
- `$`: monetary: `(format t "~$" 21982)` => 21982.00
- `D`, `B`, `O`, `X`: Decimal, Binary, Octal, Hexadecimal.
- `F`: fixed-format Floating point.

## Basic primitive: ~A or ~a (Aesthetics)

`(format t "~a" movies)` is the most basic primitive.

~~~lisp
(format nil "~a" movies)
;; => "((1 Matrix 5) (10 Matrix Trilogy swe sub 3.3))"
~~~

## Newlines: ~% and ~&

`~%` is the newline character. `~10%` prints 10 newlines.

`~&` does not print a newline if the output stream is already at one.

## Tabs

with `~T`. Also `~10T` works.

Also `i` for indentation.


## Justifying text / add padding on the right

Use a number as parameter, like `~2a`:

~~~lisp
(format nil "~20a" "yo")
;; "yo                  "
~~~

~~~lisp
(mapcar (lambda (it)
           (format t "~2a ~a ~a~%" (first it) (second it) (third it)))
         movies)
~~~

```
1  Matrix 5
10 Matrix Trilogy swe sub 3.3
```

So, expanding:

~~~lisp
(mapcar (lambda (it)
          (format t "~2a ~25a ~2a~%" (first it) (second it) (third it)))
        movies)
~~~

```
1  Matrix                    5
10 Matrix Trilogy swe sub    3.3
```

text is justified on the right (this would be with option `:`).

### Justifying on the left: @

Use a `@` as in `~2@A`:

~~~lisp
(format nil "~20@a" "yo")
;; "                  yo"
~~~

~~~lisp
(mapcar (lambda (it)
           (format nil "~2@a ~25@a ~2a~%" (first it) (second it) (third it)))
        movies)
~~~

```
 1                    Matrix 5
10    Matrix Trilogy swe sub 3.3
```

## Justifying decimals

In `~,2F`, 2 is the number of decimals and F the floats directive:
`(format t "~,2F" 20.1)` => "20.10".

With `~2,2f`:

~~~lisp
(mapcar (lambda (it)
          (format t "~2@a ~25a ~2,2f~%" (first it) (second it) (third it)))
        movies)
~~~

```
 1 Matrix                    5.00
10 Matrix Trilogy swe sub    3.30
```

And we're happy with this result.

## Formatting a format string

Sometimes you want to justify a string, but the length is a variable
itself. You can't hardcode its value as in `(format nil "~30a"
"foo")`. Enters the `v` directive. We can use it in place of the
comma-separated prefix parameters:

~~~lisp
(let ((padding 30))
    (format nil "~va" padding "foo"))
;; "foo                           "
~~~

# Capturing what is is printed into a stream

Inside `(with-output-to-string (mystream) …)`, everything that is
printed into the stream `mystream` is captured and returned as a
string:

~~~lisp
(defun greet (name &key (stream t))
   ;; by default, print to standard output.
   (format stream "hello ~a" name))

(let ((output (with-output-to-string (stream)
                (greet "you" :stream stream))))
   (format t "Output is: '~a'. It is indeed a ~a, aka a string.~&" output (type-of output)))
;; Output is: 'hello you'. It is indeed a (SIMPLE-ARRAY CHARACTER (9)), aka a string.
;; NIL
~~~

# See also

* [Pretty printing table data](https://gist.github.com/WetHat/a49e6f2140b401a190d45d31e052af8f), in ASCII art, a tutorial as a Jupyter notebook.
