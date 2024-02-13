---
title: Home
---

> Cookbook, n.
> a book containing recipes and other information about the preparation and cooking of food.

A Cookbook is an invaluable resource, as it shows how to do various things in a clear fashion without all the theoretical context. Sometimes you just need to look things up. While cookbooks can never replace proper documentation such as the HyperSpec or books such as Practical Common Lisp, every language deserves a good cookbook, Common Lisp included.

The CL Cookbook aims to tackle all sort of topics, for the beginner as for the more advanced developer.


# Content

### Getting started

* [License](license.html)
* [Getting started](getting-started.html)
  * How to install a Common Lisp implementation
  * How to start a Lisp REPL
  * How to install third-party libraries with Quicklisp
  * How to work with projects
* [Editor support](editor-support.html)
  * [Using Emacs as an IDE](emacs-ide.html)
  * [The LispWorks IDE](lispworks.html)
  * [Using VSCode with Alive](vscode-alive.html)

### Language basics

<p id="two-cols"></p>

* [Functions](functions.html)
* [Data Structures](data-structures.html)
* [Strings](strings.html)
  + [Regular Expressions](regexp.html)
* [Numbers](numbers.html)
* [Loops, iteration, mapping](iteration.html)
* [Multidimensional Arrays](arrays.html)
* [Dates and Times](dates_and_times.html)
* [Pattern Matching](pattern_matching.html)
* [Input/Output](io.html)
* [Files and Directories](files.html)
* [CLOS (the Common Lisp Object System)](clos.html)

### Advanced topics

<p id="two-cols"></p>

* [Packages](packages.html)
* [Defining Systems](systems.html)
* [Error and condition handling](error_handling.html)
* [Debugging](debugging.html)
* [Macros and Backquote](macros.html)
* [Type System](type.html)
* [Concurrency and Parallelism](process.html)
* [Performance Tuning](performance.html)
* [Testing and Continuous Integration](testing.html)
* [Scripting. Building executables](scripting.html)
<!-- epub-exclude-start -->
* [Miscellaneous](misc.html)
<!-- epub-exclude-end -->

### Outside world

<p id="two-cols"></p>

* [Interfacing with your OS](os.html)
* [Databases](databases.html)
* [Foreign Function Interfaces](ffi.html)
* [GUI programming](gui.html)
* [Sockets](sockets.html)
* [WebSockets](websockets.html)
* [Web development](web.html)
* [Web Scraping](web-scraping.html)
<!-- epub-exclude-start -->
* [Using the Win32 API](win32.html)
<!-- epub-exclude-end -->


## Download in EPUB

The Cookbook is also available in EPUB (and PDF) format.

You can [download it directly in EPUB](https://github.com/LispCookbook/cl-cookbook/releases/download/2023-12-13/common-lisp-cookbook.epub) and [PDF](https://github.com/LispCookbook/cl-cookbook/releases/download/2023-12-13/common-lisp-cookbook.pdf), and you can **pay what you want** to further support its development:


<!-- epub-exclude-start -->
<br>
<!-- epub-exclude-end -->

<a style="font-size: 16px; background-color: #4CAF50; color: white; padding: 16px; cursor: pointer;" href="https://ko-fi.com/s/01fee22a32">
  Donate and download the EPUB version
</a>

<!-- epub-exclude-start -->
<br>
<!-- epub-exclude-end -->

Thank you!


## Translations

The Cookbook has been translated to:

* [Chinese simplified](https://oneforalone.github.io/cl-cookbook-cn/#/) ([Github](https://github.com/oneforalone/cl-cookbook-cn))
* [Portuguese (Brazilian)](https://book.lisp.com.br/) ([Github](https://github.com/commonlispbr/cl-cookbook))

# Other CL Resources

<p id="two-cols"></p>

* [lisp-lang.org](http://lisp-lang.org/): success stories, tutorials and style guide
* [Awesome-cl](https://github.com/CodyReichert/awesome-cl), a curated list of libraries
* [List of Lisp Communities](https://github.com/CodyReichert/awesome-cl#community)
* [Lisp Koans](https://github.com/google/lisp-koans/) - a language learning exercise, which guides the learner progressively through many language features.
* [Learn X in Y minutes - Where X = Common Lisp](https://learnxinyminutes.com/docs/common-lisp/) - Small Common Lisp tutorial covering the essentials.
* [Common Lisp Libraries Read the Docs](https://common-lisp-libraries.readthedocs.io/) - the documentation of popular libraries ported to the modern and good looking Read The Docs style.
* [lisp-tips](https://github.com/lisp-tips/lisp-tips/issues/)
* [Articulate Common Lisp](http://articulate-lisp.com/), an initiation manual for the uninitiated
* [Lisp and Elements of Style](http://web.archive.org/web/20190316190256/https://www.nicklevine.org/declarative/lectures/) by Nick Levine
* Pascal Costanza's [Highly Opinionated Guide to Lisp](http://www.p-cos.net/lisp/guide.html)
* [Cliki](http://www.cliki.net/), Common Lisp's wiki
* 📹 [Common Lisp programming: from novice to effective developer](https://www.udemy.com/course/common-lisp-programming/?referralCode=2F3D698BBC4326F94358), a video course on the Udemy platform (paywall), by one of the main Cookbook contributor. *"Thanks for supporting my work on Udemy. You can ask me for a free coupon if you are a student."* vindarel

and also: [Common Lisp Pitfalls](https://github.com/LispCookbook/cl-cookbook/issues/479) by Jeff Dalton.



Books

* [Practical Common Lisp](http://www.gigamonkeys.com/book/) by Peter Seibel
* [Common Lisp Recipes](http://weitz.de/cl-recipes/) by Edmund Weitz, published in 2016,
* [Common Lisp: A Gentle Introduction to Symbolic Computation](http://www-2.cs.cmu.edu/~dst/LispBook/) by David S. Touretzky
* [Successful Lisp: How to Understand and Use Common Lisp](https://successful-lisp.blogspot.com/p/httpsdrive.html) by David B. Lamkins
* [On Lisp](http://www.paulgraham.com/onlisptext.html) by Paul Graham
* [Common Lisp the Language, 2nd Edition](http://www-2.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html) by Guy L. Steele
* [A Tutorial on Good Lisp Style](https://www.cs.umd.edu/%7Enau/cmsc421/norvig-lisp-style.pdf) by Peter Norvig and Kent Pitman

Advanced books

* [Loving Lisp - the Savy Programmer's Secret Weapon](https://leanpub.com/lovinglisp/) by Mark Watson
* [Programming Algorithms](https://leanpub.com/progalgs) - A comprehensive guide to writing efficient programs with examples in Lisp.


Specifications

* [The Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm) by Kent M. Pitman (also available in [Dash](https://kapeli.com/dash), [Zeal](https://zealdocs.org/) and [Velocity](https://velocity.silverlakesoftware.com/))
* [The Common Lisp Community Spec](https://cl-community-spec.github.io/pages/index.html) - a new rendering produced from the ANSI specification draft, that everyone has the right to edit.

# Further remarks

This is a collaborative project that aims to provide for Common Lisp something
similar to the [Perl Cookbook][perl] published by O'Reilly. More details about
what it is and what it isn't can be found in this [thread][thread] from
[comp.lang.lisp][cll].

If you want to contribute to the CL Cookbook, please send a pull request in or
file a ticket!

Yes, we're talking to you! We need contributors - write a chapter that's missing
and add it, find an open question and provide an answer, find bugs and report
them, (If you have no idea what might be missing but would like to help, take a
look at the [table of contents][toc] of the Perl Cookbook.) Don't worry about
the formatting, just send plain text if you like - we'll take care about that
later.

Thanks in advance for your help!

The pages here on Github are kept up to date. You can also download a
[up to date zip file][zip] for offline browsing. More info can be found at the
[Github project page][gh].


<!-- epub-exclude-start -->
<div style="text-align: center">
    <img src="orly-cover.png" alt="Oh, really? book cover"/>
</div>
<!-- epub-exclude-end -->

[cll]: news:comp.lang.lisp
[perl]: http://www.oreilly.com/catalog/cookbook/
[thread]: http://groups.google.com/groups?threadm=m3it9soz3m.fsf%40bird.agharta.de
[toc]: http://www.oreilly.com/catalog/cookbook/
[zip]: https://github.com/LispCookbook/cl-cookbook/archive/master.zip
[gh]: https://github.com/LispCookbook/cl-cookbook
