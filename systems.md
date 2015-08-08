---
title: Defining Systems
---

A **system** is a collection of Lisp files that together constitute an application or a library, and that should therefore be managed as a whole. A **system definition** describes which source files make up the system, what the dependencies among them are, and the order they should be compiled and loaded in. You can think of a system definition as an enhanced version of a Unix makefile, one that uses Lisp objects to define the systems instead of text files. Unfortunately, system definition is one of the areas that the CL ANSI standard does not address, and therefore almost every lisp version has its own version of a defsystem tool; they all have approximately the same functionality, but with slightly different syntax.

<a name="example"></a>

## An example

This example shows how to use the defsystem facility available in Allegro Common Lisp 6.x. Suppose you are working on an HTML generation library, that is composed of three files: "top.cl", that defines a package for this library, "html.cl" that implements the basic HTML-generating operations, and "library.cl" that defines high-level functions and macros to generate HTML pages. A system definition for your library might look like this:

~~~lisp
(defsystem :html
    (:pretty-name "HTML generation library"
     :default-pathname #P"/code/lisp/html/")
  (:serial "top"
           "html"
           "library"))
~~~

The above form defines a system whose name is the symbol `:html`. After the system name comes a list containing optional information: in this case, we specified a human-readable name for the package, and the directory where the source files are located. Finally, the three files that compose the system are listed, with the `:serial` keyword indicating that they should be compiled and loaded in the specified order (possibly because each one depends on definitions in the previous ones).

Having defined a system like the above, you can then operate on it. For example, calling:

~~~lisp
(load-system :html :compile t)
~~~

will cause the three files in the system to be compiled (if needed) and loaded.

Note that a system can include other systems as components, and the included systems will be recursively compiled and loaded. For example, if we wanted the definitions in the `:util` system to be available to the `:html` system, we would change the above definition to:

~~~lisp
(defsystem :html
    (:pretty-name "HTML generation library"
     :default-pathname #P"/code/lisp/html/")
  (:serial :util
           "top"
           "html"
           "library"))
~~~

<a name="cross"></a>

## Cross-platform defsystems

The most advanced attempt at building a cross-platform system definition tool is probably **MK-DEFSYSTEM**, available from [CLOCC](http://sourceforge.net/projects/clocc). Please refer to that site for more details on how to install it and use it.

<a name="begin"></a>

## System construction for beginners

Organizing a large Lisp system is not trivial: managing the interdependencies among a large number of source files and the interactions between multiple packages can easily turn into a complex undertaking that requires careful planning. Combine this with the fact that ANSI CL provides an alternative, although much more limited, way of managing code ([`provide`](http://www.lispworks.com/documentation/HyperSpec/Body/f_provid.htm) and [`require`](http://www.lispworks.com/documentation/HyperSpec/Body/f_provid.htm)), and the whole issue can quickly become a nightmare for beginners. So here is a suggestion on a possible way to organize your code, one that works best for small, self-contained packages with a well-defined API, and that provide functions that might be useful to other parts of a larger program. Just follow these three easy steps...

1. Put the following in your Lisp initialization file:

   ~~~lisp
   (eval-when (:load-toplevel :execute)
	 (shadow 'cl:require))

   (defun require (system)
	 (excl:load-system system :compile t)  ; *** ACL specific
	 (unless (member system *modules*)
	   (push system *modules*)
	   (when (find-package system)
		 (use-package system))))
   ~~~

   The `load-system` call is specific to ACL's defsystem, so you should replace it with the equivalent call for the defsystem you are using.

2. Write your code so that it creates its own package, and exports all public symbols from it. Using the `:html` system defined above as an example:

   ~~~lisp
   (defpackage :html
	 (:use :lisp)
	 (:export "ANCHOR" "HEADER" ...))
   ~~~

3. Write a system definition for your code, giving the system the same name you used for the package (like the `defsystem` form for `:html` at the beginning of this chapter).

Now you can simply call

~~~lisp
* (require :html)
~~~

and the system will be compiled (if needed) and loaded; moreover, all of its external symbols will be imported into the current package. If you later modify your code, you just have to call [`require`](http://www.lispworks.com/documentation/HyperSpec/Body/f_provid.htm) again, and the system will be recompiled and reloaded. This behaves approximately like the `use` command in Perl or the `import` statement in Java: it allows you to easily access the functionality provided by a Lisp package with a single command.
