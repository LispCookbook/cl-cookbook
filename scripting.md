---
title: Scripting. Command line arguments. Executables.
---

Using a program from a REPL is fine and well, but once it's ready
we'll surely want to call it from the terminal. We can run Lisp
**scripts** for this.

Next, if we want to distribute our program easily, we'll want to build
an **executable**.

Lisp implementations differ in their processes, but they all create
**self-contained executables**, for the architecture they are built on. The
final user doesn't need to install a Lisp implementation, he can run
the software right away.

**Start-up times** are near to zero, specially with SBCL and CCL.

Binaries **size** are large-ish. They include the whole Lisp
including its libraries, the names of all symbols, information about
argument lists to functions, the compiler, the debugger, source code
location information, and more.

Note that we can similarly build self-contained executables for **web apps**.

## Scripting with Common Lisp

Create a file named `hello` (you can drop the .lisp extension) and add this:

```
#!/usr/bin/env -S sbcl --script
(require :uiop)
(format t "hello ~a!~&" (uiop:getenv "USER"))
```

Make the script executable (`chmod +x hello`) and run it:

```
$ ./hello
hello me!
```

Nice! We can use this to a great extent already.

In addition, the script was quite fast to start, 0.03s on my system.

However, we will get longer startup times as soon as we add
dependencies. The solution is to build a binary. They start even
faster, with all dependencies compiled.

### Quickloading dependencies from a script

Say you don't bother with an .asd project definition yet, you just
want to write a quick script, but you need to load a quicklisp
dependency. You'll need a bit more ceremony:

```lisp
#!/usr/bin/env -S sbcl --script

(require :uiop)

;; We want quicklisp, which is loaded from our initfile,
;; after a classical installation.
;; However the --script flag doesn't load our init file:
;; it implies --no-sysinit --no-userinit --disable-debugger --end-toplevel-options
;; So, please load it:
(load "~/.sbclrc")

;; Load a quicklisp dependency silently.
(ql:quickload "str" :silent t)

(princ (str:concat "hello " (uiop:getenv "USER") "!"))
```

Accordingly, you could only use `require`, if the quicklisp dependency is already installed:

~~~lisp
;; replace loading sbclrc and ql:quickload.
(require :str)
~~~

Also note that when you put a `ql:quickload` in the middle of your
code, you can't load the file anymore, you can't `C-c C-k` from your
editor. This is because the reader will see the "quickload" without
running it yet, then sees "str:concat", a call to a package that
doesn't exist (it wasn't loaded yet). Common Lisp has you covered,
with a form that executes code during the read phase:

~~~lisp
;; you shouldn't need this. Use an .asd system definition!
(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload "str" :silent t))
~~~

but ASDF project definitions are here for a reason. Find me another
language that makes you install dependencies in the middle of the
application code.


## Building a self-contained executable

### With SBCL - Images and Executables

How to build (self-contained) executables is, by default, implementation-specific (see
below for portable ways). With SBCL, as says
[its documentation](http://www.sbcl.org/manual/index.html#Function-sb_002dext_003asave_002dlisp_002dand_002ddie),
it is a matter of calling `save-lisp-and-die` with the `:executable` argument to T:

~~~lisp
(sb-ext:save-lisp-and-die #P"path/name-of-executable"
                         :toplevel #'my-app:main-function
                         :executable t)
~~~

`sb-ext` is an SBCL extension to run external processes. See other
[SBCL extensions](http://www.sbcl.org/manual/index.html#Extensions)
(many of them are made implementation-portable in other libraries).

`:executable  t`  tells  to  build  an  executable  instead  of  an
image. We  could build an  image to save  the state of  our current
Lisp image, to come back working with it later. This is especially useful if
we made a lot of work that is computing intensive.
In that case, we re-use the image with `sbcl --core name-of-image`.

`:toplevel` gives the program's entry point, here `my-app:main-function`. Don't forget to `export` the symbol, or use `my-app::main-function` (with two colons).

If you try to run this in Slime, you'll get an error about threads running:

> Cannot save core with multiple threads running.

We must run the command from a simple SBCL repl, from the terminal.

I suppose your project has Quicklisp dependencies. You must then:

* ensure Quicklisp is installed and loaded at the Lisp startup (you
  completed Quicklisp installation),
* `asdf:load-asd` the project's .asd (recommended instead of just `load`),
* install the dependencies,
* build the executable.

That gives:

~~~lisp
(asdf:load-asd "my-app.asd")
(ql:quickload "my-app")
(sb-ext:save-lisp-and-die #p"my-app-binary"
                          :toplevel #'my-app:main
                          :executable t)
~~~

From the command line, or from a Makefile, use `--load` and `--eval`:

```
build:
	sbcl --load my-app.asd \
	     --eval '(ql:quickload :my-app)' \
         --eval "(sb-ext:save-lisp-and-die #p\"my-app\" :toplevel #'my-app:main :executable t)"
```

### With ASDF

Now that we've seen the basics, we need a portable method. Since its
version 3.1, ASDF allows to do that. It introduces the [`make` command](https://common-lisp.net/project/asdf/asdf.html#Convenience-Functions),
that reads parameters from the .asd. Add this to your .asd declaration:

~~~
:build-operation "program-op" ;; leave as is
:build-pathname "<here your final binary name>"
:entry-point "<my-package:main-function>"
~~~

and call `asdf:make :my-package`.

So, in a Makefile:

~~~lisp
LISP ?= sbcl

build:
    $(LISP) --load my-app.asd \
    	--eval '(ql:quickload :my-app)' \
		--eval '(asdf:make :my-app)' \
		--eval '(quit)'
~~~

### With Deploy - ship foreign libraries dependencies

All this is good, you can create binaries that work on your machine…
but maybe not on someone else's or on your server. Your program
probably relies on C shared libraries that are defined somewhere on
your filesystem. For example, `libssl` might be located on

    /usr/lib/x86_64-linux-gnu/libssl.so.1.1

but on your VPS, maybe somewhere else.

[Deploy](https://github.com/Shinmera/deploy) to the rescue.

It will create a `bin/` directory with your binary and the required
foreign libraries. It will auto-discover the ones your program needs,
but you can also help it (or tell it to not do so much).

Its use is very close to the above recipe with `asdf:make` and the
`.asd` project configuration. Use this:

~~~lisp
:defsystem-depends-on (:deploy)  ;; (ql:quickload "deploy") before
:build-operation "deploy-op"     ;; instead of "program-op"
:build-pathname "my-application-name"  ;; doesn't change
:entry-point "my-package:my-start-function"  ;; doesn't change
~~~

and build your binary with `(asdf:make :my-app)` like before.

Now, ship the `bin/` directory to your users.

When you run the binary, you'll see it uses the shipped libraries:

~~~lisp
$ ./my-app
 ==> Performing warm boot.
   -> Runtime directory is /home/debian/projects/my-app/bin/
   -> Resource directory is /home/debian/projects/my-app/bin/
 ==> Running boot hooks.
 ==> Reloading foreign libraries.
   -> Loading foreign library #<LIBRARY LIBRT>.
   -> Loading foreign library #<LIBRARY LIBMAGIC>.
 ==> Launching application.
 […]
~~~

Success!

A note regarding `libssl`. It's easier, on Linux at least, to
rely on your OS' current installation, so we'll tell Deploy to not
bother shipping it (nor `libcrypto`):

~~~lisp
(require :cl+ssl)
#+linux (deploy:define-library cl+ssl::libssl :dont-deploy T)
#+linux (deploy:define-library cl+ssl::libcrypto :dont-deploy T)
~~~

The day you want to ship a foreign library that Deploy doesn't find, you can instruct it like this:

~~~lisp
(deploy:define-library cl+ssl::libcrypto
  ;;                   ^^^ CFFI system name.
  ;;                   Find it with a call to "apropos".
  :path "/usr/lib/x86_64-linux-gnu/libcrypto.so.1.1")
~~~

A last remark. Once you built your binary and you run it for the first
time, you might get a funny message from ASDF that tries to upgrade
itself, finds nothing into a `~/common-lisp/asdf/` repository, and
quits. To tell it to not upgrade itself, add this into your .asd:

~~~lisp
;; Tell ASDF to not update itself.
(deploy:define-hook (:deploy asdf) (directory)
  (declare (ignorable directory))
  #+asdf (asdf:clear-source-registry)
  #+asdf (defun asdf:upgrade-asdf () nil))
~~~

You can also silence Deploy's start-up messages by adding this in your build script, before `asdf:make` is called:

    (push :deploy-console *features*)

And there is more, so we refer you to Deploy's documentation.


### With Roswell or Buildapp

[Roswell](https://roswell.github.io), an implementation manager, script launcher and
much more, has the `ros build` command, that should work for many
implementations.

This is how we can make our application easily installable by others, with a `ros install
my-app`. See Roswell's documentation.

Be aware that `ros build` adds core compression by default. That adds
a significant startup overhead of the order of 150ms (for a simple
app, startup time went from about 30ms to 180ms). You can disable it
with `ros build <app.ros> --disable-compression`. Of course, core
compression reduces your binary size significantly. See the table
below, "Size and startup times of executables per implementation".

We'll finish with a word on
[Buildapp](http://www.xach.com/lisp/buildapp/), a battle-tested and
still popular "application for SBCL or CCL that configures and saves
an executable Common Lisp image".

Example usage:

~~~lisp
buildapp --output myapp \
         --asdf-path . \
         --asdf-tree ~/quicklisp/dists \
         --load-system my-app \
         --entry my-app:main
~~~

Many applications use it (for example,
[pgloader](https://github.com/dimitri/pgloader)),  it is available on
Debian: `apt install buildapp`, but you shouldn't need it now with asdf:make or Roswell.


### For web apps

We can similarly build a self-contained executable for our web appplication. It
would thus contain a web server and would be able to run on the
command line:

    $ ./my-web-app
    Hunchentoot server is started.
    Listening on localhost:9003.

Note that this runs the production webserver, not a development one,
so we can run the binary on our VPS right away and access the application from
the outside.

We have one thing to take care of, it is to find and put the thread of
the running web server on the foreground. In our `main` function, we
can do something like this:

~~~lisp
(defun main ()
  (handler-case
      (progn
        (start-app :port 9003) ;; our start-app, for example clack:clack-up
        ;; let the webserver run,
        ;; keep the server thread in the foreground:
        ;; sleep for ± a hundred billion years.
        (sleep most-positive-fixnum))

    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      () (progn
           (format *error-output* "Aborting.~&")
           (clack:stop *server*)
           (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))
~~~

We used the `bordeaux-threads` library (`(ql:quickload
"bordeaux-threads")`, alias `bt`) and `uiop`, which is part of ASDF so
already loaded, in order to exit in a portable way (`uiop:quit`, with
an optional return code, instead of `sb-ext:quit`).


### Size and startup times of executables per implementation

**SBCL** isn't the only Lisp implementation.
[**ECL**](https://gitlab.com/embeddable-common-lisp/ecl/), Embeddable
Common Lisp, transpiles Lisp programs to C.  That creates a smaller
executable.

According to
[this reddit source](https://www.reddit.com/r/lisp/comments/46k530/tackling_the_eternal_problem_of_lisp_image_size/), ECL produces indeed the smallest executables of all,
an order of magnitude smaller than SBCL, but with a longer startup time.

CCL's binaries seem to be as fast to start up as SBCL and nearly half the size.


```
| program size | implementation |  CPU | startup time |
|--------------+----------------+------+--------------|
|           28 | /bin/true      |  15% |        .0004 |
|         1005 | ecl            | 115% |        .5093 |
|        48151 | sbcl           |  91% |        .0064 |
|        27054 | ccl            |  93% |        .0060 |
|        10162 | clisp          |  96% |        .0170 |
|         4901 | ecl.big        | 113% |        .8223 |
|        70413 | sbcl.big       |  93% |        .0073 |
|        41713 | ccl.big        |  95% |        .0094 |
|        19948 | clisp.big      |  97% |        .0259 |
```

<!-- TODO: what about SBCL with maximum core compression? -->

Regarding compilation times, **CCL** is famous for being fast in that regards.
ECL is more involved and takes the longer to compile of these three implementations.

You'll also want to investigate the proprietary Lisps' **tree shakers** capabilities.
**LispWorks** can build a 8MB hello-world program, without compression but fully tree-shaken.
Such an executable is generated in about 1 second and the runtime is inferior to 0.02 seconds on an Apple M2 Pro CPU.


### Building a smaller binary with SBCL's core compression

Building with SBCL's core compression can dramatically reduce your
application binary's size. In our case, it reduced it from 120MB to 23MB,
for a loss of a dozen milliseconds of start-up time, which was still
under 50ms.

<div class="info-box info">
    <strong>Note:</strong> SBCL 2.2.6 switched to compression with zstd instead of zlib, which provides smaller binaries and faster compression and decompression times. Un-official numbers are: about 4x faster compression, 2x faster decompression, and smaller binaries by 10%.
</div>


Your SBCL must be built with core compression, see the documentation: [Saving-a-Core-Image](http://www.sbcl.org/manual/#Saving-a-Core-Image)

Is it the case ?

~~~lisp
(find :sb-core-compression *features*)
:SB-CORE-COMPRESSION
~~~

Yes, it is the case with this SBCL installed from Debian.

**With SBCL**

In SBCL, we would give an argument to `save-lisp-and-die`, where
`:compression`

> may be an integer from -7 to 22, corresponding to zstd compression levels, or t (which is equivalent to the default compression level, 9).

For a simple "Hello, world" program:

```
| Program size | Compression level   |
|--------------|---------------------|
| 46MB         | Without compression |
| 22MB         | -7                  |
| 12MB         | 9                   |
| 11MB         | 22                  |
```

For a bigger project like StumpWM, an X window manager written in Lisp:

```
| Program size | Compression level   |
|--------------|---------------------|
| 58MB         | Without compression |
| 27MB         | -7                  |
| 15MB         | 9                   |
| 13MB         | 22                  |
```

**With ASDF**

However, we prefer to do this with ASDF (or rather, UIOP). Add this in your .asd:

~~~lisp
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression t))
~~~

**With Deploy**

Also, the [Deploy](https://github.com/Shinmera/deploy/) library can be used
to build a fully standalone application. It will use compression if available.

Deploy is specifically geared towards applications with foreign
library dependencies. It collects all the foreign shared libraries of
dependencies, such as libssl.so in the `bin` subdirectory.

And voilà !


## Parsing command line arguments

SBCL stores the command line arguments into `sb-ext:*posix-argv*`.

But that variable name differs from implementations, so we want a
way to handle the differences for us.

We have `(uiop:command-line-arguments)`, shipped in ASDF and included in
nearly all implementations.
From anywhere in your code, you can simply check if a given string is present in this list:

~~~lisp
(member "-h" (uiop:command-line-arguments) :test #'string-equal)
~~~

That's good, but we also want to parse the arguments, have facilities to check short and long options, build a help message automatically, etc.

We chose the [Clingon](https://github.com/dnaeon/clingon) library,
because it may have the richest feature set:

- it handles subcommands,
- it supports various kinds of options (flags, integers, booleans, counters, enums…),
- it generates Bash and Zsh completion files as well as man pages,
- it is extensible in many ways,
- we can easily try it out on the REPL
- etc

Let's download it:

    (ql:quickload "clingon")

As often, work happens in two phases:

* we first declare the options that our application accepts, their
  kind (flag, string, integer…), their long and short names and the
  required ones.
* we ask Clingon to parse the command-line options and run our app.


### Declaring options

We want to represent a command-line tool with this possible usage:

    $ myscript [-h, --help] [-n, --name NAME]

Ultimately, we need to create a Clingon command (with
`clingon:make-command`) to represent our application. A command is
composed of options and of a handler function, to do the logic.

So first, let's create options. Clingon already handles "--help" for us, but not the short version. Here's how we use `clingon:make-option` to create an option:

~~~lisp
(clingon:make-option
 :flag                ;; <--- option kind. A "flag" does not expect a parameter on the CLI.
 :description "short help"
 ;; :long-name "help" ;; <--- long name, sans the "--" prefix, but here it's a duplicate.
 :short-name #\h      ;; <--- short name, a character
 ;; :required t       ;; <--- is this option always required? In our case, no.
 :key :help)          ;; <--- the internal reference to use with getopt, see later.
~~~

This is a **flag**: if "-h" is present on the command-line, the
option's value will be truthy, otherwise it will be falsy. A flag does
not expect an argument, it's here for itself.

Similar kind of options would be:

- `:boolean`: that one expects an argument, which can be "true" or 1 to be truthy. Anything else is considered falsy.
- `:counter`: a counter option counts how many times the option is provided on the command line. Typically, use it with `-v` / `--verbose`, so the user could use `-vvv` to have extra verbosity. In that case, the option value would be 3. When this option is not provided on the command line, Clingon sets its value to 0.

We'll create a second option ("--name" or "-n" with a parameter) and we put everything in a litle function.

~~~lisp
;; The naming with a "/" is just our convention.
(defun cli/options ()
  "Returns a list of options for our main command"
  (list
   (clingon:make-option
    :flag
    :description "short help."
    :short-name #\h
    :key :help)
   (clingon:make-option
    :string              ;; <--- string type: expects one parameter on the CLI.
    :description "Name to greet"
    :short-name #\n
    :long-name "name"
    :env-vars '("USER")     ;; <-- takes this default value if the env var exists.
    :initial-value "lisper" ;; <-- default value if nothing else is set.
    :key :name)))
~~~

The second option we created is of kind `:string`. This option expects one argument, which will be parsed as a string. There is also `:integer`, to parse the argument as an integer.

There are more option kinds of Clingon, which you will find on its good documentation: `:choice`, `:enum`, `:list`, `:filepath`, `:switch` and so on.

### Top-level command

We have to tell Clingon about our top-level command.
`clingon:make-command` accepts some descriptive fields, and two important ones:

- `:options` is a list of Clingon options, each created with `clingon:make-option`
- `:handler` is the function that will do the app's logic.

And finally, we'll use `clingon:run` in our main function (the entry
point of our binary) to parse the command-line arguments, and apply
our command's logic. During development, we can also manually call
`clingon:parse-command-line` to try things out.

Here's a minimal command. We'll define our handler function afterwards:

~~~lisp
(defun cli/command ()
  "A command to say hello to someone"
  (clingon:make-command
   :name "hello"
   :description "say hello"
   :version "0.1.0"
   :authors '("John Doe <john.doe@example.org")
   :license "BSD 2-Clause"
   :options (cli/options) ;; <-- our options
   :handler #'null))  ;; <--  to change. See below.
~~~

At this point, we can already test things out on the REPL.

### Testing options parsing on the REPL

Use `clingon:parse-command-line`: it wants a top-level command, and a list of command-line arguments (strings):

~~~lisp
CL-USER> (clingon:parse-command-line (cli/command) '("-h" "-n" "me"))
#<CLINGON.COMMAND:COMMAND name=hello options=5 sub-commands=0>
~~~

It works!

We can even `inspect` this command object, we would see its properties (name, hooks, description, context…), its list of options, etc.

Let's try again with an unknown option:

~~~lisp
CL-USER> (clingon:parse-command-line (cli/command) '("-x"))
;; => debugger: Unknown option -x of kind SHORT
~~~

In that case, we are dropped into the interactive debugger, which says

```
Unknown option -x of kind SHORT
   [Condition of type CLINGON.CONDITIONS:UNKNOWN-OPTION]
```

and we are provided a few restarts:

```
Restarts:
 0: [DISCARD-OPTION] Discard the unknown option
 1: [TREAT-AS-ARGUMENT] Treat the unknown option as a free argument
 2: [SUPPLY-NEW-VALUE] Supply a new value to be parsed
 3: [RETRY] Retry SLIME REPL evaluation request.
 4: [*ABORT] Return to SLIME's top level.
```

which are very practical. If we needed, we could create an `:around`
method for `parse-command-line`, handle Clingon's conditions with
`handler-bind` and use its restarts, to do something different with
unknown options. But we don't need that yet, if ever: we want our
command-line parsing engine to warn us on invalid options.

Last but not least, we can see how Clingon prints our CLI tool's usage information:

```
CL-USER> (clingon:print-usage (cli/command) t)
NAME:
  hello - say hello

USAGE:
  hello [options] [arguments ...]

OPTIONS:
      --help          display usage information and exit
      --version       display version and exit
  -h                  short help.
  -n, --name <VALUE>  Name to greet [default: lisper] [env: $USER]

AUTHORS:
  John Doe <john.doe@example.org

LICENSE:
  BSD 2-Clause
```

We can tweak the "USAGE" part with the `:usage` key parameter of the lop-level command.


### Handling options

When the parsing of command-line arguments succeeds, we need to do something with them. We introduce two new Clingon functions:

- `clingon:getopt` is used to get an option's value by its `:key`
- `clingon:command-arguments` gets use the free arguments remaining on the command-line.

Here's how to use them:

~~~lisp
CL-USER> (let ((command (clingon:parse-command-line (cli/command) '("-n" "you" "last"))))
           (format t "name is: ~a~&" (clingon:getopt command :name))
           (format t "free args are: ~s~&" (clingon:command-arguments command)))
name is: you
free args are: ("last")
NIL
~~~

It is with them that we will write the handler of our top-level command:

~~~lisp
(defun cli/handler (cmd)
  "The handler function of our top-level command"
  (let ((free-args (clingon:command-arguments cmd))
        (name (clingon:getopt cmd :name)))  ;; <-- using the option's :key
    (format t "Hello, ~a!~%" name)
    (format t "You have provided ~a more free arguments~%"
            (length free-args))
    (format t "Bye!~%")))
~~~

We must tell our top-level command to use this handler:

~~~lisp
;; from above:
(defun cli/command ()
  "A command to say hello to someone"
  (clingon:make-command
   ...
   :handler #'cli/handler))  ;; <-- changed.
~~~

We now only have to write the main entry point of our binary and we're done.

By the way, `clingon:getopt` returns 3 values:

- the option's value
- a boolean, indicating wether this option was provided on the command-line
- the command which provided the option for this value.

See also `clingon:opt-is-set-p`.


### Main entry point

This can be any function, but to use Clingon, use its `run` function:

~~~lisp
(defun main ()
  "The main entrypoint of our CLI program"
  (clingon:run (cli/command)))
~~~

To use this main function as your binary entry point, see above how to build a Common Lisp binary. A reminder: set it in your .asd system declaration:

~~~lisp
:entry-point "my-package::main"
~~~

And that's about it. Congratulations, you can now properly parse command-line arguments!

Go check Clingon's documentation, because there is much more to it: sub-commands, contexts, hooks, handling a C-c, developing new options such as an email kind, Bash and Zsh completion…


## Catching a C-c termination signal

By default, **Clingon provides a handler for SIGINT signals**. It makes the
application to immediately exit with the status code 130.

If your application needs some clean-up logic, you can use an `unwind-protect` form. However, it might not be appropriate for all cases, so Clingon advertises to use the [with-user-abort](https://github.com/compufox/with-user-abort) helper library.

Below we show how to catch a C-c manually. Because by default, you would get a Lisp stacktrace.

We built a simple binary, we ran it and pressed `C-c`. Let's read the stacktrace:

~~~
$ ./my-app
sleep…
^C
debugger invoked on a SB-SYS:INTERACTIVE-INTERRUPT in thread   <== condition name
#<THREAD "main thread" RUNNING {1003156A03}>:
  Interactive interrupt at #x7FFFF6C6C170.

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [CONTINUE     ] Return from SB-UNIX:SIGINT.               <== it was a SIGINT indeed
  1: [RETRY-REQUEST] Retry the same request.
~~~

The signaled condition is named after our implementation:
`sb-sys:interactive-interrupt`. We just have to surround our
application code with a `handler-case`:

~~~lisp
(handler-case
    (run-my-app free-args)
  (sb-sys:interactive-interrupt ()
      (progn
        (format *error-output* "Abort.~&")
        (opts:exit))))
~~~

This code is only for SBCL though. We know about
[trivial-signal](https://github.com/guicho271828/trivial-signal/),
but we were not satisfied with our test yet. So we can use something
like this:

~~~lisp
(handler-case
    (run-my-app free-args)
  (#+sbcl sb-sys:interactive-interrupt
   #+ccl  ccl:interrupt-signal-condition
   #+clisp system::simple-interrupt-condition
   #+ecl ext:interactive-interrupt
   #+allegro excl:interrupt-signal
   ()
   (opts:exit)))
~~~

here `#+` includes the line at compile time depending on
the  implementation.  There's  also `#-`.  What `#+` does is to look for
symbols in the `*features*` list.  We can also combine symbols with
`and`, `or` and `not`.

## Continuous delivery of executables

We can make a Continuous Integration system (Travis CI, Gitlab CI,…)
build binaries for us at every commit, or at every tag pushed or at
whichever other policy.

See [Continuous Integration](testing.html#continuous-integration).

## See also

* [SBCL-GOODIES](https://github.com/sionescu/sbcl-goodies) - Allows to distribute SBCL binaries with foreign libraries: `libssl`, `libcrypto` and `libfixposix` are statically baked in. This removes the need of Deploy, when only these three foreign libraries are used.
  * it was released on February, 2023.

## Credit

* [cl-torrents' tutorial](https://vindarel.github.io/cl-torrents/tutorial.html)
* [lisp-journey/web-dev](https://lisp-journey.gitlab.io/web-dev/)
