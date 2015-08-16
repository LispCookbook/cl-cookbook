---
title: Common Lisp Cookbook
---

This is a collaborative project that aims to provide for Common Lisp something
similar to the [Perl Cookbook][perl] published by O'Reilly. More details about
what it is and what it isn't can be found in this [thread][thread] from
[comp.lang.lisp][cll].

The credit for finally giving birth to the project probably goes to
"dj_special_ed" who posted [this message][msg] to [comp.lang.lisp][cll].

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
[Github project page][gh]. There's also a [CHANGELOG][clog] available.

# Content

* [License](license.html)
* [Strings](strings.html)
* [Dates and Times](dates_and_times.html)
* [Hash Tables](hashes.html)
* [Pattern Matching / Regular Expressions](pattern_matching.html)
* [Functions](functions.html)
* [Loop](loop.html)
* [Input/Output](io.html)
* [Files and Directories](files.html)
* [Packages](packages.html)
* [Macros and Backquote](macros.html)
* [CLOS (the Common Lisp Object System)](clos-tutorial/index.html)
* [Sockets](sockets.html)
* [Interfacing with your OS](os.html)
* [Foreign Function Interfaces](ffi.html)
* [Threads](process.html)
* [Defining Systems](systems.html)
* [Setting up an IDE with Emacs on Windows or Mac OS X](windows.html)
* [Using Emacs as a Lisp IDE](emacs-ide.html)
* [Using the Win32 API](win32.html)
* [Testing](testing.html)
* [Miscellaneous](misc.html)

## Contributors

* Marco Antoniotti
* [Zach Beane](mailto:xach@xach.com)
* Pierpaolo Bernardi
* [Christopher Brown](mailto:skeptomai@mac.com)
* [Frederic Brunel](mailto:brunel@mail.dotcom.fr)
* [Jeff Caldwell](mailto:jdcal@yahoo.com)
* [Bill Clementson](mailto:bill_clementson@yahoo.com)
* Martin Cracauer
* [Gerald Doussot](mailto:gdoussot@yahoo.com)
* [Paul Foley](mailto:mycroft@actrix.gen.nz)
* JÃ¶rg-Cyril HÃ¶hle
* [Nick Levine](mailto:ndl@ravenbrook.com)
* [Austin King](mailto:shout@ozten.com)
* [Lieven Marchand](mailto:mal@wyrd.be)
* [Drew McDermott](mailto:drew.mcdermott@yale.edu)
* [Kalman Reti](mailto:reti@ai.mit.edu)
* [Alberto Riva](mailto:alb@chip.org)
* [Rudi Schlatte](mailto:rschlatte@ist.tu-graz.ac.at)
* [Emre SevinÃ§](mailto:emres@bilgi.edu.tr)
* Paul Tarvydas
* Kenny Tilton
* [Reini Urban](mailto:rurban@x-ray.at)
* [Matthieu Villeneuve](mailto:matthieu@matthieu-villeneuve.net)
* [Edi Weitz](mailto:edi@agharta.de)
* Fernando Borretti

## Other CL Resources

* [The Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm) by Kent M. Pitman
* [Practical Common Lisp](http://www.gigamonkeys.com/book/) by Peter Seibel
* [Programming:Common Lisp](http://en.wikibooks.org/wiki/Programming:Common_Lisp) from Wikibooks
* Daniel Barlow's [CLiki](http://ww.telent.net/cliki)
* [The new comp.lang.lisp FAQ](http://www-jcsu.jesus.cam.ac.uk/~csr21/lispfaq.html) by Christophe Rhodes
* [The old comp.lang.lisp FAQ](http://www-2.cs.cmu.edu/Groups//AI/html/faqs/lang/lisp/top.html) by Mark Kantrowitz
* [The Association of Lisp Users](http://www.lisp.org/)
* [Common Lisp: A Gentle Introduction to Symbolic Computation](http://www-2.cs.cmu.edu/~dst/LispBook/) by David S. Touretzky
* [Successful Lisp: How to Understand and Use Common Lisp](http://www.psg.com/~dlamkins/sl/cover.html) by David B. Lamkins
* [On Lisp](http://www.paulgraham.com/onlisptext.html) by Paul Graham
* [Common Lisp the Language, 2nd Edition](http://www-2.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html) by Guy L. Steele
* [An Introduction and Tutorial for Common Lisp](http://www.apl.jhu.edu/~hall/lisp.html) by Marty Hall
* [Common Lisp Hints](http://www.n-a-n-o.com/lisp/cmucl-tutorials/LISP-tutorial.html) by Geoffrey J. Gordon
* [A Guide to CLOS](http://www.aiai.ed.ac.uk/~jeff/clos-guide.html) by Jeff Dalton
* [Common Lisp Pitfalls](http://www.aiai.ed.ac.uk/~jeff/lisp/cl-pitfalls) by Jeff Dalton
* [Tutorial for the Common Lisp Loop Macro](http://www.ai.sri.com/~pkarp/loop.html) by Peter D. Karp
* [Portable Utilities for Common Lisp](http://iris.usc.edu/home/raycharles/price/lisp/doc/lisp-utilities.ps) by Mark Kantrowitz
* [A Guide to Good Lisp Style](http://www.cc.gatech.edu/computing/classes/cs2360/ghall/style/Good-Lisp-Style.ps) by Peter Norvig and Kent M. Pitman
* [Lisp and Elements of Style](http://www.nicklevine.org/declarative/lectures/) by Nick Levine
* Pascal Costanza's [Highly Opinionated Guide to Lisp](http://www.p-cos.net/lisp/guide.html)
* [Loving Lisp - the Savy Programmer's Secret Weapon](http://www.markwatson.com/opencontent/lisp_lic.htm) by Mark Watson
* [The Dynamic Learning Center](http://www.dynamiclearningcenter.com/)

[perl]: http://www.oreilly.com/catalog/cookbook/
[thread]: http://groups.google.com/groups?threadm=m3it9soz3m.fsf%40bird.agharta.de
[cll]: news:comp.lang.lisp
[msg]: http://groups.google.com/groups?selm=76be8851.0201222259.70ecbcb1%40posting.google.com
[toc]: http://www.oreilly.com/catalog/cookbook/toc.html
[zip]: https://github.com/LispCookbook/cl-cookbook/archive/master.zip
[gh]: https://github.com/LispCookbook/cl-cookbook
[clog]: http://localhost:4000/cl-cookbook/CHANGELOG
