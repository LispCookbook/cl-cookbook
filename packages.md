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

# Give package local nickname

Sometimes it is handy to give imported a local name for saving some typing. 

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

[guide]: http://www.flownet.com/gat/packages.pdf
[do-sym]: http://www.lispworks.com/documentation/HyperSpec/Body/m_do_sym.htm
[loop]: http://www.lispworks.com/documentation/HyperSpec/Body/06_a.htm
[rename-package]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rn_pkg.htm
