---
title: Regular Expressions
---

The [ANSI Common Lisp
standard](http://www.lispworks.com/documentation/HyperSpec/index.html)
does not include facilities for regular expressions, but a couple of
libraries exist for this task, for instance:
[cl-ppcre](https://github.com/edicl/cl-ppcre).

See also the respective [Cliki:
regexp](http://www.cliki.net/Regular%20Expression) page for more
links.

Note that some CL implementations include regexp facilities, notably
[CLISP](http://clisp.sourceforge.net/impnotes.html#regexp) and
[ALLEGRO
CL](https://franz.com/support/documentation/current/doc/regexp.htm). If
in doubt, check your manual or ask your vendor.

The description provided below is far from complete, so don't forget
to check the reference manual that comes along with the CL-PPCRE
library.

# PPCRE

## Using PPCRE

[CL-PPCRE](https://github.com/edicl/cl-ppcre) (abbreviation for
Portable Perl-compatible regular expressions) is a portable regular
expression library for Common Lisp with a broad set of features and
good performance. It has been ported to a number of Common Lisp
implementations and can be easily installed (or added as a dependency)
via Quicklisp:

~~~lisp
(ql:quickload "cl-ppcre")
~~~

Basic operations with the CL-PPCRE library functions are described
below.


## Looking for matching patterns

The `scan` function tries to match the given pattern and on success
returns four multiple-values values - the start of the match, the end
of the match, and two arrays denoting the beginnings and ends of
register matches. On failure returns `NIL`.

A regular expression pattern can be compiled with the `create-scanner`
function call. A "scanner" will be created that can be used by other
functions.

For example:

~~~lisp
(let ((ptrn (ppcre:create-scanner "(a)*b")))
  (ppcre:scan ptrn "xaaabd"))
~~~

will yield the same results as:

~~~lisp
(ppcre:scan "(a)*b" "xaaabd")
~~~

but will require less time for repeated `scan` calls as parsing the
expression and compiling it is done only once.


## Extracting information

CL-PPCRE provides a several ways to extract matching fragments, among
them: the `scan-to-strings` and `register-groups-bind` functions.

The `scan-to-strings` function is similar to `scan` but returns
substrings of target-string instead of positions. This function
returns two values on success: the whole match as a string plus an
array of substrings (or NILs) corresponding to the matched registers.

The `register-groups-bind` function tries to match the given pattern
against the target string and binds matching fragments with the given
variables.

~~~lisp
(ppcre:register-groups-bind (first second third fourth)
      ("((a)|(b)|(c))+" "abababc" :sharedp t)
    (list first second third fourth))
;; => ("c" "a" "b" "c")
~~~

CL-PPCRE also provides a shortcut for calling a function before
assigning the matching fragment to the variable:

~~~lisp
(ppcre:register-groups-bind (fname lname (#'parse-integer date month year))
      ("(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})" "Frank Zappa 21.12.1940")
    (list fname lname (encode-universal-time 0 0 0 date month year 0)))
;; => ("Frank" "Zappa" 1292889600)
~~~

## Syntactic sugar

It might be more convenient to use CL-PPCRE with the
[CL-INTERPOL](https://github.com/edicl/cl-interpol)
library. CL-INTERPOL is a library for Common Lisp which modifies the
reader in a way that introduces interpolation within strings similar
to Perl, Scala, or Unix Shell scripts.

In addition to loading the CL-INTERPOL library, initialization call
must be made to properly configure the Lisp reader. This is
accomplished by either calling the `enable-interpol-syntax` function
from the REPL or placing that call in the source file before using any
of its features:

~~~lisp
(interpol:enable-interpol-syntax)
~~~

A lot more syntax sugar is introduced by the [cl21](cl21.html) project
but in a somewhat more intrusive way.