---
title: Getting started with Common Lisp
---

We'll begin with presenting easy steps to install a development environment and to start a new Common Lisp project.

Want a 2-clicks install? Then get
[Portacle](https://shinmera.github.io/portacle/), *a portable and
multi-platform* Common Lisp environment. It ships Emacs, SBCL (the
implementation), Quicklisp (package manager), SLIME (IDE) and
Git. It's the most straightforward way to get going!

## Install an implementation

### With your package manager

If you don't know which implementation of Common Lisp to use, try SBCL:

    apt-get install sbcl

Common Lisp has been standardized via an ANSI document, so it can be
implemented in different ways. See
[Wikipedia's list of implementations](https://en.wikipedia.org/wiki/Common_Lisp#Implementations).

The following implementations are packaged for Debian and most other popular Linux distributions:

* [Steel Bank Common Lisp (SBCL)](http://www.sbcl.org/)
* [Embeddable Common Lisp (ECL)](https://gitlab.com/embeddable-common-lisp/ecl/), which compiles to C,
* [CLISP](https://clisp.sourceforge.io/)

Other well-known implementations include:

* [ABCL](http://abcl.org/), to interface with the JVM,
* [ClozureCL](https://ccl.clozure.com/), a good implementation with very fast build times (see this [Debian package for Clozure CL](http://mr.gy/blog/clozure-cl-deb.html)),
* [CLASP](https://github.com/drmeister/clasp), that interoperates with C++ libraries using LLVM for compilation to native code,
* [AllegroCL](https://franz.com/products/allegrocl/) (proprietary)
* [LispWorks](http://www.lispworks.com/) (proprietary)

and older implementations:

* [CMUCL](https://gitlab.common-lisp.net/cmucl/cmucl), originally developed at Carnegie Mellon University, from which SBCL is derived, and
* [GNU Common Lisp](https://en.wikipedia.org/wiki/GNU_Common_Lisp)
* and there is more!


### With the asdf-vm package manager

The [asdf-vm](http://asdf-vm.com/) tool can be used to manage a large ecosystem of runtimes and tools.

* [Steel Bank Common Lisp (SBCL)](http://www.sbcl.org/) is available via [this plugin](https://github.com/smashedtoatoms/asdf-sbcl) for [asdf-vm](http://asdf-vm.com/)

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

You'll find several ways of installation on its wiki (Debian package,
Windows installer, Brew/Linux Brew,…).


### With Docker

If you already know [Docker](https://docs.docker.com), you can get
started with Common Lisp pretty quickly. The
[clfoundation/cl-devel](https://hub.docker.com/r/clfoundation/cl-devel)
image comes with recent versions of SBCL, CCL, ECL and ABCL, plus
Quicklisp installed in the home (`/home/cl`), so than we can
`ql:quickload` libraries straight away.

Docker works on GNU/Linux, Mac and Windows.

The following command will download the required image (around 1.0GB
compressed), put your local sources inside the Docker image where indicated,
and drop you into an SBCL REPL:

    docker run --rm -it -v /path/to/local/code:/home/cl/common-lisp/source clfoundation/cl-devel:latest sbcl

We still want to develop using Emacs and SLIME, so we need to connect SLIME to
the Lisp inside Docker. See
[slime-docker](https://gitlab.common-lisp.net/cl-docker-images/slime-docker),
which is a library that helps on setting that up.


### On Windows

All implementations above can be installed on Windows.

[Portacle](https://shinmera.github.io/portacle/) is multiplatform and works on Windows.

You can also try:

* [ρEmacs](https://rho-emacs.sourceforge.io/), a preconfigured distribution of GNU Emacs specifically for Microsoft Windows. It ships with many CL implementations: CCL, SBCL, CLISP, ABCL and ECL, and also has components for other programming languages (Python, Racket, Java, C++…).
* [Corman Lisp](https://github.com/sharplispers/cormanlisp), for Windows XP, Windows 2000, Windows ME or Windows NT. It is fully integrated with the Win32 API, and all the Windows API functions are readily available from Lisp.


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

Lisp is interactive by nature, so in case of an error we enter the
debugger. This can be annoying in certain cases, so you might want to
use SBCL's `--disable-debugger` option.

<div class="info" style="background-color: #e7f3fe; border-left: 6px solid #2196F3; padding: 17px;">
<!-- if inside a <p> then bootstrap adds 10px padding to the bottom -->
<strong>TIP:</strong> The CLISP implementation has a better default REPL for the
terminal (readline capabilities, completion of symbols). You can even
use <code>clisp -on-error abort</code> to have error messages without the
debugger. It's handy to try things out, but we recommend to set-up
your editor and to use SBCL or CCL.
</div>


<div class="info" style="background-color: #e7f3fe; border-left: 6px solid #2196F3; padding: 17px; margin-top:1em;">
<!-- if inside a <p> then bootstrap adds 10px padding to the bottom -->
<strong>TIP:</strong>
 By adding the <code>-c</code> switch to rlwrap, you can autocomplete file names.
</div>

## Libraries

Common Lisp has thousands of libraries available under a free software license. See:

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
  Facility), which offers functionalities similar to those of `make` and
  `ld.so`, and has become a de facto standard.

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
(load "/usr/share/common-lisp/source/quicklisp/quicklisp.lisp")
~~~

Then, in both cases, still from the REPL:

~~~lisp
(quicklisp-quickstart:install)
~~~

This will create the `~/quicklisp/` directory, where Quicklisp will
maintain its state and downloaded projects.

If you wish, you can install Quicklisp to a different location.  For instance,
to install it to a hidden folder on Unix systems:

~~~lisp
(quicklisp-quickstart:install :path "~/.quicklisp")
~~~

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
(ql:quickload "system-name")
~~~

For example, this installs the "[str](https://github.com/vindarel/cl-str/)" string manipulation library:

~~~lisp
(ql:quickload "str")
~~~

and voilà. You can use it right away:

~~~lisp
(str:title-case "HELLO LISP!")
~~~

<div class="info" style="background-color: #e7f3fe; border-left: 6px solid #2196F3; padding: 17px; margin: 1em;">
<strong>SEE MORE:</strong> To understand the <code>package:a-symbol</code> notation, read the <a href="packages.html">packages page</a>, section "Accessing symbols from a package".
</div>

We can install more than one library at once. Here we install
[cl-ppcre](https://edicl.github.io/cl-ppcre/) for regular-expressions, and
[Alexandria](https://alexandria.common-lisp.dev/draft/alexandria.html),
a utility library:

~~~lisp
(ql:quickload '("str" "cl-ppcre" "alexandria"))
~~~

Anytime you want to use a third-party library in your Lisp REPL, you
can run this `ql:quickload` command. It will not hit the network a second
time if it finds that the library is already installed on your file
system. Libraries are by default installed in
`~/quicklisp/dist/quicklisp/`.

Note also that dozens of Common Lisp libraries are packaged in
Debian. The package names usually begin with the cl- prefix (use
`apt-cache search --names-only "^cl-.*"` to list them all).

For example, in order to use the `cl-ppcre` library, one should first install the `cl-ppcre` package.

Then, in SBCL, it can be used like this:

~~~lisp
(require "asdf")
(require "cl-ppcre")
(cl-ppcre:regex-replace "fo+" "foo bar" "frob")
~~~

Here we pretend we don't have Quicklisp installed and we use `require` to load a module that is available on the file system. In doubt, you can use `ql:quickload`.

See Quicklisp's documentation for more commands. For instance, see how to upgrade or rollback your Quicklisp's distribution.


### Advanced dependencies management

You can drop Common Lisp projects into any of those folders:

- `~/quicklisp/local-projects`
- `~/common-lisp`,
- `~/.local/share/common-lisp/source`,

A library installed here is automatically available for every project.

For a complete list, see the values of:

~~~lisp
(asdf/source-registry:default-user-source-registry)
~~~

and

~~~lisp
asdf:*central-registry*
~~~

#### Providing our own version of a library. Cloning projects.

Given the property above, we can clone any library into the
`~/quicklisp/local-projects/` directory and it will be found by ASDF (and Quicklisp) and
available right-away:

~~~lisp
(asdf:load-system "system")
~~~

or

~~~lisp
(ql:quickload "system")
~~~

The practical different between the two is that `ql:quickload` first tries to
fetch the system from the Internet if it is not already installed.

Note that symlinks in local-projects to another location of your liking works too.

#### How to work with local versions of libraries

If we need libraries to be installed locally, for only one project, or
in order to easily ship a list of dependencies with an application, we
can use [Qlot](https://github.com/fukamachi/qlot) or [CLPM](https://clpm.dev).

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

Where `my-project.asd` resembles this:

~~~lisp
(asdf:defsystem "my-project"
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

- ASDF documentation: [defining a system with defsystem](https://common-lisp.net/project/asdf/asdf.html#Defining-systems-with-defsystem)

### How to load an existing project

You have created a new project, or you have an existing one, and you
want to work with it on the REPL, but Quicklisp doesn't know it. How
can you do ?

Well first, if you create it or clone it into
one of `~/common-lisp`, `~/.local/share/common-lisp/source/` or
`~/quicklisp/local-projects`, you'll be able to `(ql:quickload …)` it with no
further ado.

Otherwise you'll need to compile and load its system definition
(`.asd`) first. In SLIME with the `slime-asdf` contrib loaded, type `C-c C-k`
(*slime-compile-and-load-file*) in the `.asd`, then you can
`(ql:quickload …)` it.

You can use `(asdf:load-asd "my-project.asd")` programmatically instead of `C-c C-k`.

Usually you want to "enter" the system in the REPL at this stage:

~~~lisp
(in-package :my-project)
~~~

Lastly, you can compile or eval the sources (`my-project.lisp`) with
`C-c C-k` or `C-c C-c` (*slime-compile-defun*) in a form, and see its
result in the REPL.

Another solution is to use ASDF's list of known projects:

~~~lisp
;; startup file like ~/.sbclrc
(pushnew "~/path-to-project/root/" asdf:*central-registry* :test #'equal)
~~~

and since ASDF is integrated into Quicklisp, we can `quickload` our project right away.

Happy hacking !


## More settings

You might want to set SBCL's default encoding format to utf-8:

    (setf sb-impl::*default-external-format* :utf-8)

You can add this to your `~/.sbclrc`.

If you dislike the REPL to print all symbols upcase, add this:

    (setf *print-case* :downcase)

<div class="info-box warning">
<!-- if inside a <p> then bootstrap adds 10px padding to the bottom -->
<strong>Warning:</strong> This might break the behaviour of some packages like it happened with
<a href="https://github.com/fukamachi/mito/issues/45">Mito</a>.
Avoid doing this in production.
</div>


## See also

- [cl-cookieproject](https://github.com/vindarel/cl-cookieproject) - a project skeleton for a ready-to-use project with an entry point and unit tests. With a `src/` subdirectory, some more metadata, a 5AM test suite, a way to build a binary, an example CLI args parsing, Roswell integration.
- Source code organization, libraries and packages:  [https://lispmethods.com/libraries.html](https://lispmethods.com/libraries.html)

## Credits

* [https://wiki.debian.org/CommonLisp](https://wiki.debian.org/CommonLisp)
* [http://articulate-lisp.com/project/new-project.html](http://articulate-lisp.com/project/new-project.html)
