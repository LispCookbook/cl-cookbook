---
title: Documentation
---

This chapter covers how to document your Common Lisp code and
how to access documentation from the REPL and your editor.

## Writing docstrings

Docstrings are the primary way to document your code in
Common Lisp. They are supported on functions, macros,
generic functions, methods, packages, classes, slots,
variables, and types.

### Functions and macros

Place the docstring immediately after the parameter list:

~~~lisp
(defun square (n)
  "Return the square of N."
  (* n n))
~~~

For more complex functions, start with a one-line summary,
then elaborate:

~~~lisp
(defun parse-csv-line (line &key (separator #\,))
  "Parse LINE as a CSV row, returning a list of fields.
  SEPARATOR is the field delimiter (default: comma).
  Fields may be enclosed in double quotes, in which
  case embedded separators and newlines are preserved."
  ;; implementation
  )
~~~

### Good practices

- Start with a short summary sentence that describes
  what the function does, not how.
- Refer to parameters in UPPERCASE so they stand out.
- Document return values, especially when non-obvious.
- Mention signaled conditions if any.
- Keep lines under 64 characters for tools that render
  docstrings in fixed-width columns.

Here is a real-world example from Alexandria:

~~~lisp
(defun flatten (tree)
  "Traverses the TREE in order, collecting
  non-null leaves into a list."
  ;; ...
  )
~~~

### Classes and slots

Use the `:documentation` keyword:

~~~lisp
(defclass person ()
  ((name
    :initarg :name
    :accessor person-name
    :type string
    :documentation "The person's full name.")
   (age
    :initarg :age
    :accessor person-age
    :type (integer 0)
    :documentation "Age in years."))
  (:documentation "A person with a name and age."))
~~~

### Packages

~~~lisp
(defpackage :my-utils
  (:use :cl)
  (:documentation "Small utility functions used
  across the project.")
  (:export #:square #:cube))
~~~

### Variables

~~~lisp
(defvar *timeout* 30
  "Default timeout in seconds for network requests.")
~~~

## Retrieving documentation

### `documentation`

The `documentation` function retrieves the docstring
of any documented object:

~~~lisp
(documentation 'square 'function)
;; => "Return the square of N."

(documentation (find-class 'person) t)
;; => "A person with a name and age."

(documentation (find-package :my-utils) t)
;; => "Small utility functions used across the
;;     project."
~~~

### `describe`

`describe` prints a comprehensive description of any
object, including its type, value, and documentation:

~~~lisp
(describe 'square)
;; COMMON-LISP-USER::SQUARE
;;   [symbol]
;;
;; SQUARE names a compiled function:
;;   Lambda-list: (N)
;;   Derived type: (FUNCTION (T) ...)
;;   Documentation:
;;     Return the square of N.
;;   Source file: ...
~~~

It also works on classes, packages, and instances:

~~~lisp
(describe (make-instance 'person
                         :name "Ada"
                         :age 36))
~~~

### `inspect`

`inspect` provides an interactive, navigable inspector
for complex objects. In SBCL at the REPL:

~~~lisp
(inspect (make-instance 'person
                        :name "Ada"
                        :age 36))
;; The object is a STANDARD-OBJECT of type PERSON.
;; 0. NAME: "Ada"
;; 1. AGE: 36
~~~

In SLIME/Sly, `C-c I` (or `slime-inspect`) opens
a graphical inspector buffer that lets you click
through nested structures.

### `apropos`

When you can't remember a function's exact name,
`apropos` searches for symbols matching a substring:

~~~lisp
(apropos "HASH")
;; COMMON-LISP:HASH-TABLE
;; COMMON-LISP:HASH-TABLE-COUNT
;; COMMON-LISP:HASH-TABLE-P
;; COMMON-LISP:MAKE-HASH-TABLE
;; ...
~~~

`apropos-list` returns the results as a list instead
of printing them:

~~~lisp
(length (apropos-list "HASH"))
;; => 28  (varies by implementation)
~~~

## Editor integration

### SLIME (Emacs)

SLIME provides quick access to documentation:

| Key | Command | Purpose |
|---|---|---|
| `C-c C-d d` | `slime-describe-symbol` | Describe a symbol |
| `C-c C-d f` | `slime-describe-function` | Describe a function |
| `C-c C-d h` | `slime-hyperspec-lookup` | Open HyperSpec page |
| `C-c C-d a` | `slime-apropos` | Search symbols |
| `C-c I` | `slime-inspect` | Interactive inspector |

### Sly (Emacs)

Sly uses similar keybindings with `sly-` prefixed
commands and adds fuzzy completion by default.

### Other editors

Most Common Lisp editor integrations (Alive for
VS Code, Slimv for Vim) provide similar documentation
lookup features through their SWANK/LSP connections.

## Generating documentation for your project

Several tools can generate HTML documentation from
your code's docstrings.

### Staple

[Staple](https://github.com/Shinmera/staple) generates
documentation pages from your system's docstrings,
README, and cross-references. It is widely used in the
Common Lisp ecosystem (Shinmera's libraries all use it).

Add it to your project:

~~~lisp
(ql:quickload "staple-markless")
~~~

Generate documentation:

~~~lisp
(staple:generate :my-system)
~~~

This creates an HTML page in your project directory
with all exported symbols, their docstrings, and links
between them.

### MGL-PAX

[MGL-PAX](https://github.com/melisgl/mgl-pax) takes
an "inside-out" approach where you write documentation
as part of your source using special reference syntax.
It can generate GitHub-friendly Markdown and HTML.

## The HyperSpec

The [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
is the definitive reference for the language. It is
derived from the ANSI Common Lisp standard and covers
every function, macro, special form, and concept.

You can search it quickly with:

- **DuckDuckGo**: type `!clhs function-name`
- **SLIME**: `C-c C-d h` opens the page for a symbol
- **The [Common Lisp Community Spec](https://cl-community-spec.github.io/pages/index.html)**:
  a modern rendering of the same specification

## See also

- [The Common Lisp Style Guide](https://google.github.io/styleguide/lispguide.xml)
  for Google's documentation conventions
- [awesome-cl](https://github.com/CodyReichert/awesome-cl#documentation-builders)
  for a full list of documentation tools
