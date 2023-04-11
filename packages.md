---
title: Packages
---

See: [The Complete Idiot's Guide to Common Lisp Packages][guide]

## Creating a package

Here's an example package definition. It takes a name, and you
probably want to `:use` the Common Lisp symbols and functions.

~~~lisp
(defpackage :my-package
  (:use :cl))
~~~

To start writing code for this package, go inside it:

~~~lisp
(in-package :my-package)
~~~

This `in-package` macro puts you "inside" a package:

- any new variable or function will be created in this package, aka in the "namespace" of this package.
- you can call all this package's symbols directly, without using the package prefix.

Just try!

We can also use `in-package` to try packages on the REPL. Note that on
a new Lisp REPL session, we are "inside" the CL-USER package. It is a
regular package.

Let's show you an example. We open a new .lisp file and we create a new
package with a function inside our package:

~~~lisp
;; in test-package.lisp
(defpackage :my-package
  (:use :cl))

(in-package :my-package)

(defun hello ()
  (print "Hello from my package."))
~~~

This "hello" function lives inside "my-package". It is not exported yet.

Continue below to see how to call it.

### Accessing symbols from a package

As soon as you have defined a package or loaded one (with Quicklisp,
or if it was defined as a dependency in your `.asd` system
definition), you can access its symbols with `package:a-symbol`, using
a colon as delimiter.

For example:

~~~lisp
(str:concat …)
~~~

When the symbol is not exported (it is "private"), use a double colon:

~~~lisp
(package::non-exported-symbol)
(my-package::hello)
~~~

Continuing our example: in the REPL, be sure to be in `my-package` and not in `CL-USER`. There you can call "hello" directly:

~~~lisp
CL-USER> (in-package :my-package)
#<PACKAGE "MY-PACKAGE">
;; ^^^ this creates a package object.
MY-PACKAGE> (hello)
;; ^^^^ the REPL shows you the current package.
"Hello from my package."
~~~

But now, come back to the CL-USER package and try to call "hello": we get an error.

~~~lisp
MY-PACKAGE> (in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
CL-USER> (hello)

=> you get the interactive debugger that says:

The function COMMON-LISP-USER::HELLO is undefined.

(quit)
~~~

We have to "namespace" our hello function with its package name:

~~~lisp
CL-USER> (my-package::hello)
"Hello from my package."
~~~

Let's export the function.

### Exporting symbols

Augment our `defpackage` declaration to export our "hello" function like so:

~~~lisp
(defpackage :my-package
  (:use :cl)
  (:export
   #:hello))
~~~

Compile this (`C-c C-c` in Slime), and now you can call

~~~lisp
CL-USER> (my-package:hello)
~~~

with a single colon.

You can also use the `export` function:

~~~lisp
(in-package :my-package)
(export #'hello)
~~~

Observation:

- exporting `:hello` without the sharpsign (`#:hello`) works too, but it will always create a new symbol. The `#:` notation does not create a new symbol. It's a detail and at this point, a personal preference to use it or not. It is helpful to not clutter our symbols namespace, specially when we import and re-export symbols from other libraries. So it is not useful for us at this point.

Now we might want to import individual symbols in order to access them right
away, without the package prefix.


### Importing symbols from another package

You can import exactly the symbols you need with `:import-from`:

~~~lisp
(defpackage :my-package
  (:import-from :ppcre #:regex-replace)
  (:use :cl))
~~~

Now you can call `regex-replace` from inside `my-package`, without the `ppcre` package prefix. `regex-replace` is a new symbol inside your package. It is not exported.

Sometimes, we see `(:import-from :ppcre)`, without an explicit
import. This helps people using ASDF's *package inferred system*.

You can also use the `import` function from outside a package definition:

~~~lisp
CL-USER> (import 'ppcre:regex-replace)
CL-USER> (regex-replace …)
~~~

### Importing all symbols

It is a better practice to carefully choose what symbols you import from another package (read below), but we can also import all symbols at once with `:use`:

~~~lisp
(defpackage :my-package
  (:use :cl :ppcre))
~~~

Now you can access all variables, functions and macros of `cl-ppcre` from your `my-package` package.

You can also use the `use-package` function:

~~~lisp
CL-USER> (use-package 'cl-ppcre)
~~~


### About "use"-ing packages being a bad practice

`:use` is a well spread idiom. You could do:

~~~lisp
(defpackage :my-package
  (:use :cl :ppcre))
~~~

and now, **all** symbols that are exported by `cl-ppcre` (aka `ppcre`)
are available to use directly in your package. However, this should be
considered bad practice, unless you `use` another package of your
project that you control. Indeed, if the external package adds a
symbol, it could conflict with one of yours, or you could add one
which will hide the external symbol and you might not see a warning.

To quote [this thorough explanation](https://gist.github.com/phoe/2b63f33a2a4727a437403eceb7a6b4a3) (a recommended read):

> USE is a bad idea in contemporary code except for internal packages that you fully control, where it is a decent idea until you forget that you mutate the symbol of some other package while making that brand new shiny DEFUN. USE is the reason why Alexandria cannot nowadays even add a new symbol to itself, because it might cause name collisions with other packages that already have a symbol with the same name from some external source.


## List all Symbols in a Package (do-external-symbols)

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

## Package nickname

### Nickname Provided by Packages

When defining a package, it is trivial to give it a nickname for better user
experience. The following example is a snippet of `PROVE` package:

~~~lisp
(defpackage prove
  (:nicknames :cl-test-more :test-more)
  (:export #:run
           #:is
           #:ok)
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

#### Package Local Nicknames (PLN)

Sometimes it is handy to give a local name to an imported package to
save some typing, especially when the imported package does not
provide nice nicknames.

Many implementations (SBCL, CCL, ECL, Clasp, ABCL, ACL, LispWorks >= 7.2…) support Package Local Nicknames (PLN).

~~~lisp
(defpackage :mypackage
  (:use :cl)
  (:local-nicknames (:nickname :original-package-name)
                    (:alex :alexandria)
                    (:re :cl-ppcre)))

(in-package :mypackage)

;; You can use :nickname instead of :original-package-name
(nickname:some-function "a" "b")
~~~

The effect of `PLN` is totally within `mypackage` i.e. the `nickname` won't work in other packages unless defined there too. So, you don't have to worry about unintended package name clash in other libraries.

Another facility exists for adding nicknames to packages. The function [`RENAME-PACKAGE`](http://www.lispworks.com/documentation/HyperSpec/Body/f_rn_pkg.htm) can be used to replace the name and nicknames of a package. But it's use would mean that other libraries may not be able to access the package using the original name or nicknames. There is rarely any situation to use this. Use Package Local Nicknames instead.


### Package locks

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

## See also

- [Package Local Nicknames in Common Lisp](https://gist.github.com/phoe/2b63f33a2a4727a437403eceb7a6b4a3) article.

[guide]: http://www.flownet.com/gat/packages.pdf
[do-sym]: http://www.lispworks.com/documentation/HyperSpec/Body/m_do_sym.htm
[loop]: http://www.lispworks.com/documentation/HyperSpec/Body/06_a.htm
[rename-package]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rn_pkg.htm
[sbcl-package-lock]: http://www.sbcl.org/manual/#Package-Locks
[clisp-package-lock]: https://clisp.sourceforge.io/impnotes/pack-lock.html
[cl-package-lock]: https://www.cliki.net/CL-PACKAGE-LOCKS
