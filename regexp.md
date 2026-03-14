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

## PPCRE

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


### Looking for matching patterns: scan, create-scanner

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


### Extracting information

CL-PPCRE provides several ways to extract matching fragments.

#### all-matches, all-matches-as-strings

The function `all-matches-as-strings` is very handy: it returns a list of matches:

~~~lisp
(ppcre:all-matches-as-strings "\\d+" "numbers: 1 10 42")
;; => ("1" "10" "42")
~~~

The function `all-matches` is similar, but it returns a list of positions:

~~~lisp
(ppcre:all-matches "\\d+" "numbers: 1 10 42")
;; => (9 10 11 13 14 16)
~~~

Look carefully: it actually return a list containing the start and end
positions of all matches: 9 and 10 are the start and end for the first
number (1), and so on.

If you wanted to extract integers from this example string, simply map
`parse-integer` to the result:

~~~lisp
CL-USER> (ppcre:all-matches-as-strings "\\d+" "numbers: 1 10 42")
;; ("1" "10" "42")
CL-USER> (mapcar #'parse-integer *)
(1 10 42)
~~~

The two functions accept the usual `:start` and `:end` key arguments. Additionally, `all-matches-as-strings` accepts a `:sharedp` argument:

> If SHAREDP is true, the substrings may share structure with TARGET-STRING.

#### count-matches (new in 2.1.2, April 2024)

`(count-matches regex target-string)` returns a count of all matches of `regex` against `target-string`:


~~~lisp
CL-USER> (ppcre:count-matches "a" "foo bar baz")
2

CL-USER> (ppcre:count-matches "\\w*" "foo bar baz")
6
~~~



#### scan-to-strings, register-groups-bind

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
(ppcre:register-groups-bind
  (fname lname (#'parse-integer date month year))
      ("(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
       "Frank Zappa 21.12.1940")
    (list fname lname date month year))
;; => ("Frank" "Zappa" 21 12 1940)
~~~

### Replacing text: regex-replace, regex-replace-all

~~~lisp
(ppcre:regex-replace "a" "abc" "A") ;; => "Abc"
;; or
(let ((pat (ppcre:create-scanner "a")))
  (ppcre:regex-replace pat "abc" "A"))
~~~

### S-expression syntax (parse trees)

cl-ppcre can also accept regular expressions as
s-expressions instead of strings. This is called the
"parse tree" syntax and avoids backslash escaping:

~~~lisp
;; Match one or more digits:
(ppcre:scan '(:greedy-repetition 1 nil :digit-class)
            "abc123def")
;; => 3, 6, #(), #()
~~~

You can convert between string and tree representations
with `parse-string`:

~~~lisp
(ppcre:parse-string "(\\d+)-(\\w+)")
;; => (:SEQUENCE
;;     (:REGISTER
;;      (:GREEDY-REPETITION 1 NIL :DIGIT-CLASS))
;;     #\-
;;     (:REGISTER
;;      (:GREEDY-REPETITION 1 NIL :WORD-CHAR-CLASS)))
~~~

The tree form is useful for building patterns
programmatically:

~~~lisp
(defun match-tag (tag-name)
  "Build a regex matching an XML opening tag."
  `(:sequence
    "<" ,tag-name
    (:greedy-repetition 0 nil
      (:inverted-char-class #\>))
    ">"))

(ppcre:scan (match-tag "div") "<div class='x'>")
;; => 0, 16, #(), #()
~~~

Common tree elements:

| Tree form | String equivalent |
|---|---|
| `:digit-class` | `\d` |
| `:word-char-class` | `\w` |
| `:whitespace-char-class` | `\s` |
| `(:greedy-repetition 0 nil x)` | `x*` |
| `(:greedy-repetition 1 nil x)` | `x+` |
| `(:greedy-repetition 0 1 x)` | `x?` |
| `(:register x)` | `(x)` |
| `(:alternation x y)` | `x\|y` |
| `(:sequence x y)` | `xy` |

See the [cl-ppcre docs](https://edicl.github.io/cl-ppcre/#create-scanner2)
for the full parse tree syntax.

## See more

- [cl-ppcre on common-lisp-libraries.readthedocs.io](https://common-lisp-libraries.readthedocs.io/cl-ppcre/) and read on: `do-matches`, `do-matches-as-strings`,
  `do-register-groups`, `do-scans`, `parse-string`, `regex-apropos`,
  `quote-meta-chars`, `split`…
