---
title: Getting started
---

Easy steps to install a development environment and start a project.

Want a 2-clicks install ? Then get
[Portacle](https://shinmera.github.io/portacle/), *a portable and
multiplatform* Common Lisp environment. It ships Emacs25, SBCL (the
implementation), Quicklisp (package manager), Slime (IDE) and
Git. It's the most straightforward way to get going !

## Install an implementation

### With your package manager

TLDR;

    apt-get install sbcl

Common Lisp has been standardized via an ANSI document, so it can be
implemented in different ways: see
[Wikipedia's list of implementations](https://en.wikipedia.org/wiki/Common_Lisp#Implementations).

The following are packaged for Debian and probably for your distro:

* [SBCL](http://www.sbcl.org/)
* [ECL](https://gitlab.com/embeddable-common-lisp/ecl/)
  (compiles to C)
* [CMUCL](https://gitlab.common-lisp.net/cmucl/cmucl)
* [GCL](https://en.wikipedia.org/wiki/GNU_Common_Lisp)
* [CLISP](https://clisp.sourceforge.io/)

In doubt, just get SBCL.

See also

* [ABCL](http://abcl.org/) (for the JVM),
* [ClozureCL](https://ccl.clozure.com/)
* [CLASP](https://github.com/drmeister/clasp) (C++ and LLVM)
* [AllegroCL](https://franz.com/products/allegrocl/) (proprietary)
* [LispWorks](http://www.lispworks.com/) (proprietary)

and  this [Debian package for Clozure CL](http://mr.gy/blog/clozure-cl-deb.html).

### With Roswell

[Roswell](https://github.com/roswell/roswell/wiki) is:

* an implementation manager: it makes it easy to install a Common Lisp
  implementation (`ros install ecl`), an exact version of an
  implementation (`ros install sbcl/1.2.0`), to change the default one
  being used (`ros use ecl`),
* a scripting environment (helps to run Lisp from the shell, to get
  the command line arguments,…),
* a script installer,
* a testing environment (to run tests, including on popular Continuous
  Integration platforms),
* a building utility (to build images and executables in a portable way).

You'll several ways of installation on its wiki (Debian package,
Windows installer, Brew/Linux Brew,…).

## Start a REPL

Just launch the implementation executable on the command line to enter
the REPL (Read Eval Print Loop), i.e. the interactive
interpreter.

Quit with `(quit)` or `ctr-d` (on some implementations).

Here is a sample session:

```
user@debian:~$ sbcl
This is SBCL 1.3.14.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (+ 1 2)

3
* (quit)
user@debian:~$
```

You can slightly enhance the REPL (the arrow keys do not work,
it has no history,…) with `rlwrap`:

    apt-get install rlwrap

and:

    rlwrap sbcl

But we'll setup our editor to offer a better experience instead of
working in this REPL. See [editor-support](editor-support.html).

## Libraries

Common Lisp has hundreds of libraries available under a free software licence. See:

* [Quickdocs](http://quickdocs.org/) - the library documentation hosting for CL.
* the [Awesome-cl](https://github.com/CodyReichert/awesome-cl) list, a
  curated list of libraries.
* [Cliki](http://www.cliki.net/), the Common Lisp wiki.

### Some terminology

* In the Common Lisp world, a **package** is a way of grouping symbols
together and of providing encapsulation. It is similar to a C++
namespace, a Python module or a Java package.

* A **system** is a collection of CL source files bundled with an .asd
  file which tells how to compile and load them. There is often a
  one-to-one relationship between systems and packages, but this is in
  no way mandatory. A system may declare a dependency on other
  systems. Systems are managed by [ASDF](https://common-lisp.net/project/asdf/asdf.html) (Another System Definition
  Facility), which offers functionalities similar to those of make and
  ld.so, and has become a de facto standard.

* A Common Lisp library or project typically consists of one or
  several ASDF systems (and is distributed as one Quicklisp project).

### Install Quicklisp

[Quicklisp](https://www.quicklisp.org/beta/) is more than a package
manager, it is also a central repository (a *dist*) that ensures that
all libraries build together.

It provides its own *dist* but it is also possible to build our own.

To install it, we can either:

1- run this command, anywhere:

    curl -O https://beta.quicklisp.org/quicklisp.lisp

and enter a Lisp REPL and load this file:

    sbcl --load quicklisp.lisp

or

2- install the Debian package:

    apt-get install cl-quicklisp

and load it, from a REPL:

~~~lisp
(load "/usr/share/cl-quicklisp/quicklisp.lisp")
~~~

Then, in both cases, still from the REPL:

~~~lisp
(quicklisp-quickstart:install)
~~~

This will create the `~/quicklisp/` directory, where Quicklisp will
maintain its state and downloaded projects.

If you want Quicklisp to always be loaded in your Lisp sessions, run
`(ql:add-to-init-file)`: this adds the right stuff to the init file of
your CL implementation. Otherwise, you have to run `(load
"~/quicklisp/setup.lisp")` in every session if you want to use
Quicklisp or any of the libraries installed through it.

It adds the following in your (for example) `~/.sbclrc`:

~~~lisp
#-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
~~~

### Install libraries

In the REPL:

~~~lisp
(ql:quickload "package-name")
~~~

and voilà. See Quicklisp's documentation for more commands.


Note also that dozens of Common Lisp libraries are packaged in
Debian. The package names usually begin with the cl- prefix (use
`apt-cache search --names-only "^cl-.*"` to list them all).

For example, in order to use the CL-PPCRE library (for regular
expressions), one should first install the `cl-ppcre` package.

Then, in SBCL and ECL, it can be used with:

~~~lisp
(require "asdf")
(require "cl-ppcre")
(cl-ppcre:regex-replace "fo+" "foo bar" "frob")
~~~

See more: https://wiki.debian.org/CommonLisp

### Advanced dependencies management

Note that you don't need to know that to get started.

Quicklisp installs the libraries into `~/quicklisp/local-projects/`. A
library installed here is automatically available for every project.

#### Providing our own version of a library. Cloning projects.

Given the property above, we can clone any library into the
local-projects directory and it will be found by quicklisp and
available right-away:

~~~lisp
(ql:quickload "package")
~~~

#### How to work with local versions of libraries

If we need libraries to be installed locally, for only one project, or
in order to easily ship a list of dependencies with an application, we
can use [Qlot](https://github.com/fukamachi/qlot).

Quicklisp also provides
[Quicklisp bundles](https://www.quicklisp.org/beta/bundles.html). They
are self-contained sets of systems that are exported from Quicklisp
and loadable without involving Quicklisp.

At last, there's
[Quicklisp controller](https://github.com/quicklisp/quicklisp-controller)
to help us build *dists*.

## Working with projects

Now that we have Quicklisp and our editor ready, we can start writing
Lisp code in a file and interacting with the REPL.

But what if we want to work with an existing project or create a new
one, how do we proceed, what's the right sequence of `defpackage`,
what to put in the `.asd` file, how to load the project into the REPL ?

### Creating a new project

Some project builders help to scaffold the project structure. We like
[cl-project](https://github.com/fukamachi/cl-project) that also sets
up a tests skeleton.

In short:

~~~lisp
(ql:quickload "cl-project")
(cl-project:make-project #P"./path-to-project/root/")
~~~

it will create a directory structure like this:

```
|-- my-project.asd
|-- my-project-test.asd
|-- README.markdown
|-- README.org
|-- src
|   `-- my-project.lisp
`-- tests
    `-- my-project.lisp
```

Where `my-project.asd` ressembles this:

~~~lisp
(defsystem "my-project"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()  ;; <== list of Quicklisp dependencies
  :components ((:module "src"
                :components
                ((:file "my-project"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "my-project-test"))))
~~~

and `src/my-project.lisp` this:

~~~lisp
(defpackage footest
  (:use :cl))
(in-package :footest)
~~~

### How to load an existing project

You have created a new project, or you have an existing one, and you
want to work with it on the REPL, but Quicklisp doesn't know it. How
can you do ?

Well first, if you create it or clone it into
`~/quicklisp/local-projects`, you'll be able to `(ql:quickload …)` it
with no further ado.

Otherwise you'll need to compile and load its system definition
(`.asd`) first. In Slime, type `C-c C-k`
(*slime-compile-and-load-file*) in the `.asd`, then you can
`(ql:quickload …)` it.

Usually you want to "enter" the system in the REPL at this stage:

~~~lisp
(use-package :my-project)
~~~

Lastly, you can compile or eval the sources (`my-project.lisp`) with
`C-c C-k` or `C-c C-c` (*slime-compile-defun*) in a form, and see its
result in the REPL.

Another solution is to use ASDF's list of known projects:

~~~lisp
(pushnew "~/path-to-project/root/" asdf:*central-registry* :test #'equal)
~~~

and since ASDF is integrated into Quicklisp, we can `quickload` our project.

Happy hacking !


## Read more

* Source code organization, libraries and packages:  [https://lispmethods.com/libraries.html](https://lispmethods.com/libraries.html)

## Credits

* [https://wiki.debian.org/CommonLisp](https://wiki.debian.org/CommonLisp)
* [http://articulate-lisp.com/project/new-project.html](http://articulate-lisp.com/project/new-project.html)
