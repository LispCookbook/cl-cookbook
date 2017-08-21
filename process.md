---
title: Threads
---


<a name="intro"></a>

## Introduction

By _threads_, I mean separate execution strands within a single Lisp process, sharing the same address space. Typically, execution is automatically switched between these strands by the system (either by the lisp kernel or by the operating system) so that tasks appear to be completed in parallel (asynchronously). This page discusses the creation and management of threads and some aspects of interactions between them. For information about the interaction between lisp and other _processes_, see [Interfacing with your OS](os.html).

Unfortunately, an instant pitfall for the unwary is that most implementations refer (in nomenclature) to threads as _processes_ - this is a historical feature of a language which has been around for much longer than the term _thread_. Call this maturity a sign of stable implementations, if you will.

The ANSI Common Lisp standard doesn't mention this topic. So almost everything that can be said here depends on your OS and your implementation. It's not just a question of different symbols in different packages (although that might be enough to get you started, so see the [acl-compat](http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/portableaserve/portableaserve/acl-compat/) package at Sourceforge and in particular the files whose names match "acl-mp-*.lisp") - concepts can vary too.

Speaking of implementations, the following discussion currently refers only to [LispWorks](http://www.lispworks.com/). It has been tested on LispWorks for Windows Professional version 4.2.6 running on Windows NT (SP 6). All the examples below (apart from those few which require resaving the lisp image) should work without difficulty on the (free) Personal edition running under either Linux or Windows.

__Note__: the
[Bordeaux Threads](https://github.com/sionescu/bordeaux-threads)
library (in Quicklisp) supports all major Common Lisp
implementations. See also this
[comprehensive blog post](https://z0ltan.wordpress.com/2016/09/02/basic-concurrency-and-parallelism-in-common-lisp-part-3-concurrency-using-bordeaux-and-sbcl-threads/).

Your mileage may vary. I will add "support" for other implementations and operating systems to this page later, if I have the time and access to the appropriate materials.

In any case, read the manual! In this instance, the manual concerned is the chapter on "The MP Package" in the "LispWorks Reference Manual".

Pre-requisite: I assume familiarity with Common Lisp, in particular with closures, the loop macro, and lambda forms.


<a name="why_bother"></a>

## Why bother?

The first question to resolve is: why bother with threads? Sometimes your answer will simply be that your application is so straightforward that you need not concern yourself with threads at all. But in many other cases it's difficult to imagine how a sophisticated application can be written without multi-threading. For example:

*   you might be writing a server which needs to be able to respond to more than one user / connection at a time (for instance: a web server - see [this example](./sockets.html#server) on the Sockets page);
*   you might want to perform some background activity, without halting the main application while this is going on;
*   you might want your application to be notified when a certain time has elapsed;
*   you might want to keep the application running and active while waiting for some system resource to become available;
*   you might need to interface with some other system which requires multithreading (for example, "windows" under Windows which generally run in their own threads);
*   you might want to associate different contexts (e.g. different dynamic bindings) with different parts of the application;
*   you might even have the simple need to do two things at once.


<a name="emergency"></a>

