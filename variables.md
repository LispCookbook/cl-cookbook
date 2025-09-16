---
title: Variables
---

You are writing your first Common Lisp program (welcome!) and
you want to declare variables. What are your options?

When in doubt, use `defparameter` for top-level parameters.

Use `let` or `let*` for lexical scope:

```lisp
(let* ((a 2)
       (square (* a a)))
   (format t "the square of ~a is ~a" a square))
```

Use `setf` to change them.


## defparameter: top-level variables

Use `defparameter` to declare top-level variables, like this:

```lisp
(defparameter *name* "me")

(defun hello (&optional name)
  "Say hello."
  (format t "Hello ~a!" (or name *name*)))
```

`defparameter` accepts an optional third argument: the variable's docstring:

```lisp
(defparameter *name* "me"
   "Default name to say hello to.")
```

The inline docstrings are an important part of the Common Lisp
interactive experience. You will encounter them during your coding
sessions (and we lispers usually keep our Lisp running for a long
time). In Emacs and Slime, you can ask for a symbol's docstring with
`C-c C-d d`. You can also ask for a docstring programmatically:

~~~lisp
(documentation '*name* 'variable)
~~~

We ask the documentation of the `*name*` *symbol*, not what it holds,
hence the quote in `'*name*`. Another "doc-type" is `'function`. See:
in Common Lisp, variables and functions live in different "namespaces",
and it shows here.

We'll mention the `defparameter` form with no value below.

### redefining a defparameter

A Common Lisp coding session is usually long-lasting and very
interactive. We leave a Lisp running and we interact with it while we
work. This is done with Emacs and Slime, Vim, Atom and SLIMA, VSCode
and Alive, Lem… and more editors, or from the terminal.

That means that you can do this:

1. write a first defparameter

```lisp
(defparameter *name* "me")
```

either write this in the REPL, either write this in a .lisp file and
compile+load it with a shortcut (`C-c C-c` in Slime on this
expression, or `C-c C-k` to compile and load everything you have in
the current buffer). If you work from a simple terminal REPL, you can
`(load …)` a .lisp file.

Now the `*name*` variable exists in the running image.

2. edit the defparameter line:

```lisp
(defparameter *name* "you")
```

and load the changes the same way: either with the REPL, or with a
`C-c C-c`. Now, the `*name*` variable has a new value, "you".

A `defvar` wouldn't be redefined.


## defvar: no redefinition

`defvar` defines top-level *variables* and protects them from redefinition.

When you re-load a `defvar`, it doesn't erase the current value, you
must use `setf` for this.


```lisp
(defvar *names-cache* (list)
  "Store a list of names we said \"hello\" to.")

(defun hello (&optional (name *name*))
   (pushnew name *names-cache* :test #'string-equal)
   (format t "hello ~a!" name))
```

Let's see it in use:

```lisp
CL-USER> (hello)
hello you!
NIL
CIEL-USER> *names-cache*
("you")
CIEL-USER> (hello "lisper")
hello lisper!
NIL
CIEL-USER> *names-cache*
("lisper" "you")
```

What happens to `*names-cache*` if you redefine the `defvar` line
(with `C-c C-c`, or `C-c C-k`, or on the REPL…)?

It doesn't change and *that is a good thing*.

Indeed, this variable isn't a user-visible parameter, it doesn't have
an immediate use, but it is important for the program correctness, or
strength, etc. Imagine it holds the cache of your webserver: you don't
want to erase it when you load new code. During development, we hit a
lot `C-c C-k` to reload the current file, but there are certain things
we want untouched. If it is a database connection, you don't want to
set it back to nil, and connect again, everytime you compile your
code.

You must use `setf` to change a defvar's variable value.


## The "\*earmuff\*" convention

See how we wrote `*name*` in-between "\*earmuffs\*". That is an
important convention, that helps you not override top-level variables
in lexical scopes.

```lisp
(defparameter name "lisper")

;; later…
(let ((name "something else"))
   ;;  ^^^ overrides the top-level name. This will cause bugs.
   …)
```

This becomes a feature only when using earmuffs:

```lisp
(defparameter *db-name* "db.db")

(defun connect (&optional (db-name *db-name*))
  (connect db-name))

(let ((*db-name* "another.db"))
  (connect))
  ;;^^^^  its db-name optional parameter, which defaults to *db-name*, now sees "another.db".
```

By the way, for such a use-case, you will often find `with-…` macros
that abstract the `let` binding.

```lisp
(with-db "another.db"
  (connect))
```

By the way again, an
[earmuff](https://www.wordreference.com/definition/earmuff) is a thing
that covers the ears (but only the ears) in winter. You might have
seen it in movies more than in reality. The lasting word is: take care
of yourself, stay warm and use earmuffs.

## Global variables are created in the "dynamic scope"

Our top-level parameters and variables are created in the so-called
*dynamic scope*. They can be accessed from anywhere else: from
function definitions (as we did), in `let` bindings, etc.

In Lisp, we also say these are [*dynamic variables* or *special*](https://cl-community-spec.github.io/pages/Dynamic-Variables.html).

It could also be possible to create one from anywhere by *proclaiming*
it "special". It really isn't the thing you do everydays but, you
know, in Lisp everything's possible ;)

> A dynamic variable can be referenced outside the dynamic extent of a form that binds it. Such a variable is sometimes called a "global variable" but is still in all respects just a dynamic variable whose binding happens to exist in the global environment rather than in some dynamic environment. [Hyper Spec]


## setf: change values

Any variable can be changed with `setf`:

```lisp
(setf *name* "Alice")
;; => "Alice"
```

It returns the new value.

Actually, `setf` accepts *pairs* of value, variable:

~~~lisp
(setf *name* "Bob"
      *db-name* "app.db")
;; => "app.db"
~~~

Note that it returned the last value.

What happens if you `setf` a variable that wasn't declared yet? It
generally works but you have a warning:

```lisp
;; in SBCL 2.5.8
CL-USER> (setf *foo* "foo")
; in: SETF *FOO*
;     (SETF CL-USER::*FOO* "foo")
;
; caught WARNING:
;   undefined variable: CL-USER::*FOO*
;
; compilation unit finished
;   Undefined variable:
;     *FOO*
;   caught 1 WARNING condition
"foo"
```

We see the returned "foo", so it worked. Please declare variables with
`defparameter` or `defvar` first.

Let's read the full `setf` docstring because it's interesting:

```txt
Takes pairs of arguments like SETQ. The first is a place and the second
is the value that is supposed to go into that place. Returns the last
value. The place argument may be any of the access forms for which SETF
knows a corresponding setting form.
```

Note that `setq` is another macro, but now seldom used, because `setf`
works on more "places". You can setf functions and many things.


## let, let*: create lexical scopes

`let` lets you define variables in a limited scope, or override top-level variables temporarily.

Below, our two variables only exist in-between the parenthesis of the `let`:

```lisp
(let* ((a 2)
       (square (* a a)))
   (format t "the square of ~a is ~a" a square))
   ;; so far so good

(format t "the value of a is: ~a" a)
;; => ERROR: the variable A is unbound
```

"unbound" means the variable is bound to nothing, not even to NIL. It
doesn't exist.

Outside of the scope formed by the `let`, the variables `a` and `square` don't exist.

They can be accessed by any form inside the `let` binding. If we
create a second `let`, its *environment* inherits the previous one (we
see variables declared above, fortunately!).

```lisp
(defparameter *name* "test")

(defun log (square)
  (format t "name is ~s and square is ~a" *name* square))

(let* ((a 2)
       (square (* a a)))
  ;; inside first environment
  (let ((*name* "inside let"))
    ;; inside second environment,
    ;; we access the dynamic scope.
    (log square)))
;; name is "inside let" and square is 4
;; NIL

(print *name*)
;; => "test"
;;    ^^^^ outside the let, back to the dynamic scope's value.
```

We could also define a function inside a let, so that this function
definition "sees" a binding from a surrounding let at compile
time. This is a closure and it's for the chapter on functions.

A "lexical scope" is simply

> a scope that is limited to a spatial or textual region within the establishing form. "The names of parameters to a function normally are lexically scoped." [Hyper Spec]

In other words, the scope of a variable is determined by its position
in the source code. It's today's best practice. It's the least
surprising way of doing: you can *see* the scope by looking at the
source code.

### setf inside let

Let's make it even clearer: you can `setf` any value that is
*shadowed* in a `let` binding, once outside the let, the variables are
back to the value of the current *environment*.

We know this:

```lisp
(defparameter *name* "test")

(let ((*name* "inside let"))
  (format t "*name* inside let: ~s" *name*))
;; => *name* outside let: "inside let"

(format t "*name* outside let: ~s" *name*)
;; => *name* outside let: "test"
```

we setf a dynamic parameter that was shadowed by a let binding:

```lisp
(defparameter *name* "test")

(defun change-name ()
   ;; bad style though,
   ;; try to not mutate variables inside your functions,
   ;; but take arguments and return fresh data structures.
   (setf *name* "set!"))
   ;;    ^^^^^ from the dynamic environment, or from a let lexical scope.

(let ((*name* "inside let"))
  (change-name)
  (format t "*name* inside let: ~s" *name*))
;; => *name* outside let: "set!"

(format t "*name* outside let: ~s" *name*)
;; => *name* outside let: "test"
```


## Unbound variables

"unbound" variables were not bound to anything, not even nil. Their
symbol might exist, but they have no associated value.

You can create such variables like this:

```lisp
(defparameter *connection*)
```

This `defparameter` form is correct. You didn't give any default
value: the parameter is unbound.

You can check if a variable (or a function) is bound with `boundp` (or
`fboundp`). The `p` is for "predicate".

You can make a variable (or function) unbound with `makunbound` (or `fmakunbound`).

## Global vars are thread safe

Don't be afraid of accessing and set-ing global bindings in
threads. Each thread will have its own copy of the
variable. Consequently, you can bind them to other values with `let`
bindings, etc. That's good.

It's only if you want one single source of truth that you'll have to
share the variable between threads and where the danger lies. You can
use a lock (very easy), but that's all another topic.


## Lasting words

A few style guidelines:

- create all your top-level parameters at the top of a file
- define first parameters then variables
- use docstrings
- read your compiler's warnings
- it's better for your functions to accept arguments, rather than to rely on top-level parameters
- your functions shouldn't mutate (modify) a top-level binding. You should create a new data structure instead, and use your function's return value as the parameter to another function, and have data flow from one function to another.
- parameters are best for: a webserver port, a default value…
- the pattern where a function parameter is by default a global variable is typical and idiomatic:

```lisp
;; from the STR library.
(defvar *whitespaces* (list #\Backspace #\Tab #\Linefeed #\Newline #\Vt #\Page
                            #\Return #\Space #\Rubout
                            ;; edited for brevity
                            ))

(defun trim-left (s &key (char-bag *whitespaces*))
  "Removes all characters in `char-bag` (default: whitespaces) at the beginning of `s`."
  (when s
   (string-left-trim char-bag s)))
```

the default value can also be a function call:

```lisp
;; from the Lem editor
(defun buffer-modified-p (&optional (buffer (current-buffer)))
  "Return T if 'buffer' has been modified, NIL otherwise."
  (/= 0 (buffer-%modified-p buffer)))
```

- these let bindings over global variables are idiomatic too: `(let ((*name* "other")) …)`.

