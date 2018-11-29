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

When defining a package, it is normal to give it a nickname for better user
experience. For example:

~~~lisp
(defpackage :my-package-with-a-long-name
  (:use :cl)
  (:nicknames :my-package :my)
  (:export :my-function
           :my-another-function))
~~~

Afterwards, a user may use nickname instead of the full name to refer to this
package. For example:

~~~lisp
(my-package:my-function)
(my:my-another-function)
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

(rename-package :cl-ppcre :re)

;; You can use RE instead of CL-PPCRE now.
(re:scan "a" "abc")
~~~

Please note that the original name (`:CL-PPCRE` in the example above) will
**not** be available any more after renaming.

## Package locks

A package can be declared to be locked so that it cannot be modified by the
user. Attempts to change its symbol table or redefine functions which its
symbols name result in an error.

More detailed information can be obtained from documents of
[SBCL][sbcl-package-lock] and [CLisp][clisp-package-lock].

For example, if you try the following code:

~~~lisp
(asdf:load-system :alexandria)
(rename-package :alexandria :alex)
~~~

You will get following error (on SBCL):

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

If modification is required anyway, a package named
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
