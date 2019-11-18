---
title: Scripting. Command line arguments. Executables.
---

Using a program from a REPL is fine and well, but if we want to
distribute our program easily, we'll want to build an executable.

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


# Building a self-contained executable

## With SBCL

How to build (self-contained) executables is implementation-specific (see
below Buildapp and Rowsell). With SBCL, as says
[its documentation](http://www.sbcl.org/manual/index.html#Function-sb_002dext_003asave_002dlisp_002dand_002ddie),
it is a matter of:

~~~lisp
(sb-ext:save-lisp-and-die #P"path/name-of-executable" :toplevel #'my-app:main-function :executable t)
~~~

`sb-ext` is an SBCL extension to run external processes.  See other
[SBCL extensions](http://www.sbcl.org/manual/index.html#Extensions)
(many of them are made implementation-portable in other libraries).

`:executable  t`  tells  to  build  an  executable  instead  of  an
image. We  could build an  image to save  the state of  our current
Lisp image, to come back working with it later. Specially useful if
we made a lot of work that is computing intensive.

If you try to run this in Slime, you'll get an error about threads running:

> Cannot save core with multiple threads running.

Run the command from a simple SBCL repl.

I suppose your project has Quicklisp dependencies. You must then:

* ensure Quicklisp is installed and loaded at Lisp startup (you
  completed Quicklisp installation)
* `load` the project's .asd
* install dependencies
* build the executable.

That gives:

~~~lisp
(load "my-app.asd")
(ql:quickload :my-app)
(sb-ext:save-lisp-and-die #p"my-app-binary" :toplevel #'my-app:main :executable t)
~~~

From the command line, or from a Makefile, use `--load` and `--eval`:

```
build:
	sbcl --load my-app.asd \
	     --eval '(ql:quickload :my-app)' \
         --eval "(sb-ext:save-lisp-and-die #p\"my-app\" :toplevel #'my-app:main :executable t)"
```

## With ASDF

Now that we've seen the basics, we need a portable method. Since its
version 3.1, ASDF allows to do that. It introduces the [`make` command](https://common-lisp.net/project/asdf/asdf.html#Convenience-Functions),
that reads parameters from the .asd. Add this to your .asd declaration:

~~~
:build-operation "program-op" ;; leave as is
:build-pathname "<binary-name>"
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


## With Roswell or Buildapp

[Roswell](https://roswell.github.io), an implementation manager and much
more, also has the `ros build` command, that should work for many
implementations.

We can also make our app installable with Roswell by a `ros install
my-app`. See its documentation.

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


## For web apps

We can similarly build a self-contained executable for our web-app. It
would thus contain a web server and would be able to run on the
command line:

    $ ./my-web-app
    Hunchentoot server is started.
    Listening on localhost:9003.

Note that this runs the production webserver, not a development one,
so we can run the binary on our VPS right away and access the app from
outside.

We have one thing to take care of, it is to find and put the thread of
the running web server on the foreground. In our `main` function, we
can do something like this:

~~~lisp
(defun main ()
  (start-app :port 9003) ;; our start-app, for example clack:clack-up
  ;; let the webserver run.
  ;; warning: hardcoded "hunchentoot".
  (handler-case (bt:join-thread (find-if (lambda (th)
                                            (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
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


## Size and startup times of executables per implementation

SBCL isn't the only Lisp implementation.
[ECL](https://gitlab.com/embeddable-common-lisp/ecl/), Embeddable
Common Lisp, transpiles Lisp programs to C.  That creates a smaller
executable.

According to
[this reddit source](https://www.reddit.com/r/lisp/comments/46k530/tackling_the_eternal_problem_of_lisp_image_size/), ECL produces indeed the smallest executables of all,
an order of magnitude smaller than SBCL, but with a longer startup time.

CCL's binaries seem to be as fast as SBCL and nearly half the size.

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

## Building a smaller binary with SBCL's core compression

Building with SBCL's core compression can dramatically reduce your
application binary's size. In our case, we passed from 120MB to 23MB,
for a loss of a dozen milliseconds of start-up time, which was still
under 50ms!

Your SBCL must be built with core compression, see the documentation: [http://www.sbcl.org/manual/#Saving-a-Core-Image](http://www.sbcl.org/manual/#Saving-a-Core-Image)

Is it the case ?

~~~lisp
(find :sb-core-compression *features*)
:SB-CORE-COMPRESSION
~~~

Yes, it is the case with this SBCL installed from Debian.

<!-- In case you want to build from scratch, you can use `./make.sh --fancy`. -->

**With SBCL**

In SBCL, we would give an argument to `save-lisp-and-die`, where
`:compression`

> may be an integer from -1 to 9, corresponding to zlib compression levels, or t (which is equivalent to the default compression level, -1).

We experienced a 1MB difference between levels -1 and 9.

**With ASDF**

However, we prefer to do this with ASDF (or rather, UIOP). Add this in your .asd:

~~~lisp
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
~~~

**With Deploy**

Also, the [Deploy](https://github.com/Shinmera/deploy/) library can be used
to build a fully standalone application. It will use compression if available.

Deploy is specifically geared towards applications with foreign
library dependencies. It collects all the foreign shared libraries of
dependencies, such as libssl.so in the `bin` subdirectory.

And voilà !


# Parsing command line arguments

SBCL stores the command line arguments into `sb-ext:*posix-argv*`.

But that variable name differs from implementations, so we may want a
library to handle the differences for us.

We also want to parse the arguments.

A quick look at the
[awesome-cl#scripting](https://github.com/CodyReichert/awesome-cl#scripting)
list and we'll do that with the
[unix-opts](https://github.com/mrkkrp/unix-opts) library.

    (ql:quickload "unix-opts")

We can call it with its `opts` alias (nickname).

As often work happens in two phases:

* declaring the options our app accepts, their optional argument, defining their type
  (string, integer,…), long and short names, and the required ones,
* parsing them (and handling missing or malformed parameters).


## Declaring arguments

We define the arguments with `opts:define-opts`:

~~~lisp
(opts:define-opts
    (:name :help
           :description "print this help text"
           :short #\h
           :long "help")
    (:name :nb
           :description "here we want a number argument"
           :short #\n
           :long "nb"
           :arg-parser #'parse-integer) ;; <- takes an argument
    (:name :info
           :description "info"
           :short #\i
           :long "info"))
~~~

Here `parse-integer` is a built-in CL function.

Example output on the command line (auto-generated help text):

~~~
$ my-app -h
my-app. Usage:

Available options:
  -h, --help               print this help text
  -n, --nb ARG             here we want a number argument
  -i, --info               info
~~~


## Parsing

We parse and get the arguments with `opts:get-opts`, which returns two
values: the list of valid options and the remaining free arguments. We
then must use `multiple-value-bind` to assign both into variables:

~~~lisp
  (multiple-value-bind (options free-args)
      ;; There is no error handling yet.
      (opts:get-opts)
      ...
~~~

We can test this by giving a list of strings to `get-opts`:

~~~lisp
(multiple-value-bind (options free-args)
                   (opts:get-opts '("hello" "-h" "-n" "1"))
                 (format t "Options: ~a~&" options)
                 (format t "free args: ~a~&" free-args))
Options: (HELP T NB-RESULTS 1)
free args: (hello)
NIL
~~~

If we  put an unknown option,  we get into the  debugger. We'll see
error handling in a moment.

So `options` is a
[property list](https://lispcookbook.github.io/cl-cookbook/data-structures.html#plist). We
use `getf` and `setf` with plists, so that's how we do our
logic. Below we print the help with `opts:describe` and then `exit`
(in a portable way).

~~~lisp
  (multiple-value-bind (options free-args)
      (opts:get-opts)

    (if (getf options :help)
        (progn
          (opts:describe
           :prefix "You're in my-app. Usage:"
           :args "[keywords]") ;; to replace "ARG" in "--nb ARG"
          (opts:exit))) ;; <= optional return status.
    (if (getf options :nb)
       ...)
~~~

For a full example, see its
[official example](https://github.com/mrkkrp/unix-opts/blob/master/example/example.lisp)
and
[cl-torrents' tutorial](https://vindarel.github.io/cl-torrents/tutorial.html).

The  example in  the unix-opts  repository suggests  a macro  to do
slightly better. Now to error handling.


### Handling malformed or missing arguments

There are 4 situations that unix-opts doesn't handle, but signals
conditions for us to take care of:

* for an unknown argument: an `unknown-option` condition is signaled
* also `missing-arg`
* `arg-parser-failed` when, for example, it expected an integer but got text
* `missing-required-option`

So, we must create simple functions to handle those conditions, and
surround the parsing of the options with an `handler-bind`:

~~~lisp
  (multiple-value-bind (options free-args)
      (handler-bind ((opts:unknown-option #'unknown-option) ;; the condition / our function
                     (opts:missing-arg #'missing-arg)
                     (opts:arg-parser-failed #'arg-parser-failed)
                     (opts:missing-required-option))
         (opts:get-opts))
    …
    ;; use "options" and "free-args"
~~~

Here we suppose we want one function to handle each case, but it could
be a simple one. They take the condition as argument.

~~~lisp
(defun handle-arg-parser-condition (condition)
  (format t "Problem while parsing option ~s: ~a .~%" (opts:option condition) ;; reader to get the option from the condition.
                                                       condition)
  (opts:describe) ;; print help
  (opts:exit)) ;; portable exit
~~~

For more about condition handling, see [error and condition handling](error_handling.html).

### Catching a C-c termination signal

Let's build a simple binary, run it, try a `C-c` and read the stacktrace:

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
  (sb-sys:interactive-interrupt () (progn
                                     (format *error-output* "Abort.~&")
                                     (opts:exit))))
~~~

This code only for SBCL though. We know about
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

# Continuous delivery of executables

We can make a Continuous Integration system (Travis CI, Gitlab CI,…)
build binaries for us at every commit, or at every tag pushed or at
whichever other policy.

See [Continuous Integration](testing.html#continuous-integration).

# Credit

* [cl-torrents' tutorial](https://vindarel.github.io/cl-torrents/tutorial.html)
* [lisp-journey/web-dev](https://lisp-journey.gitlab.io/web-dev/)
