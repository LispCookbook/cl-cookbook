---
title: Packages
---

See: [The Complete Idiot's Guide to Common Lisp Packages][guide]

# List all Symbols in a Package

Common Lisp provides some macros to iterate through the symbols of a
package. The two most interesting are:
[`DO-SYMBOLS` and `DO-EXTERNAL-SYMBOLS`][do-sym]. `DO-SYMBOLS` iterates over the
symbols accessible in the package and `DO-EXTERNAL-SYMBOLS` only iterates over
the external symbols (you can see them as the real package API).

To print all exported symbols of a package named "PACKAGE", you can write:

~~~lisp
(do-external-symbols (s (find-package "PACKAGE"))
  (print s))
~~~

You can also collect all these symbols in a list by writing:

~~~lisp
(let (symbols)
  (do-external-symbols (s (find-package "PACKAGE"))
    (push s symbols))
  symbols)
~~~

Or you can do it with [`LOOP`][loop].

~~~lisp
(loop for s being the external-symbols of (find-package "PACKAGE")
  collect s)
~~~

# Package nickname

## Nickname Provided by Packages

When defining a package, it is trivial to give it a nickname for better user
experience. The following example is a snippet of `PROVE` package:

~~~lisp
(defpackage prove
  (:nicknames :cl-test-more :test-more)
  (:export :run
           :is
           :ok)
~~~

Afterwards, a user may use nickname instead of the package name to refer to this
package. For example:

~~~lisp
(prove:run)
(cl-test-more:is)
(test-more:ok)
~~~

Please note that although Common Lisp allows defining multiple nicknames for
one package, too many nicknames may bring maintenance complexity to the
users. Thus the nicknames shall be meaningful and straightforward. For
example:

~~~lisp
(defpackage #:iterate
  (:nicknames #:iter))

(defpackage :cl-ppcre
  (:nicknames :ppcre)
~~~

## Give a Package a Local Nickname

Sometimes it is handy to give a local name to an imported package to save some
typing, especially when the imported package does not provide nice nicknames.

This can be achieved by using [`RENAME-PACKAGE`][rename-package]. For example:

~~~lisp
(asdf:load-system :cl-ppcre)

(defpackage :mypackage
  (:use :cl))
(in-package :mypackage)

;; Add nickname :RE to package :CL-PPCRE.
(rename-package :cl-ppcre :cl-ppcre '(re))

;; You can use :RE instead of :CL-PPCRE now.
(re:scan "a" "abc")
~~~

The function head of `RENAME-PACKAGE` is as follows:

~~~lisp
(rename-package package-designator name &optional (nicknames nil))
~~~

As you may guess, this function can be used to rename a package by giving it a
new *name* instead of a new *nickname*.

However, it is highly recommended **not** to do so. Because otherwise the
package will be modified and the original name (`:CL-PPCRE` in the example
above) will **not** be available any more after renaming. For example:

~~~lisp
;; DO NOT DO THIS!
(rename-package :cl-ppcre :re)

;; ERROR!
(cl-ppcre:scan "a" "abc")
~~~

This might cause problems if you want to load another package that relies on
the modified package.

## Package locks

The package `common-lisp` and SBCL internal implementation packages are locked
by default, including `sb-ext`.

In addition, any user-defined package can be declared to be locked so that it
cannot be modified by the user. Attempts to change its symbol table or
redefine functions which its symbols name result in an error.

More detailed information can be obtained from documents of
[SBCL][sbcl-package-lock] and [CLisp][clisp-package-lock].

For example, if you try the following code:

~~~lisp
(asdf:load-system :alexandria)
(rename-package :alexandria :alex)
~~~

You will get the following error (on SBCL):

~~~
Lock on package ALEXANDRIA violated when renaming as ALEX while
in package COMMON-LISP-USER.
   [Condition of type PACKAGE-LOCKED-ERROR]
See also:
  SBCL Manual, Package Locks [:node]

Restarts:
 0: [CONTINUE] Ignore the package lock.
 1: [IGNORE-ALL] Ignore all package locks in the context of this operation.
 2: [UNLOCK-PACKAGE] Unlock the package.
 3: [RETRY] Retry SLIME REPL evaluation request.
 4: [*ABORT] Return to SLIME's top level.
 5: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {10047A8433}>)

...
~~~

If a modification is required anyway, a package named
[cl-package-lock][cl-package-lock] can be used to ignore package locks. For
example:

~~~lisp
(cl-package-locks:without-package-locks
  (rename-package :alexandria :alex))
~~~

[guide]: http://www.flownet.com/gat/packages.pdf
[do-sym]: http://www.lispworks.com/documentation/HyperSpec/Body/m_do_sym.htm
[loop]: http://www.lispworks.com/documentation/HyperSpec/Body/06_a.htm
[rename-package]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rn_pkg.htm
[sbcl-package-lock]: http://www.sbcl.org/manual/#Package-Locks
[clisp-package-lock]: https://clisp.sourceforge.io/impnotes/pack-lock.html
[cl-package-lock]: https://www.cliki.net/CL-PACKAGE-LOCKS
