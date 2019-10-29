---
title: Using Emacs as an IDE
---

This page is meant to provide an introduction to using Emacs as a Lisp IDE. The key bindings used in the example code snippets assume an Emacs configuration similar to that provided by the [.emacs](https://github.com/LispCookbook/cl-cookbook/blob/master/.emacs) file that is included as part of the [Setting up an IDE with Emacs on Windows or Mac OS X](windows.html) page.


**Note**: [Portacle](https://shinmera.github.io/portacle/) is a
portable and multiplatform CL development environment, a
straightforward way to get going.


<a name="Slide-2"></a>

## Why Use Emacs?

*   Emacs has fantastic support for working with Lisp code
*   Not tying yourself into a single CL vendor's editor
*   Runs on virtually every OS and CL implementation
*   Extensible
*   Can be customized to do many common tasks
*   Built-in support for different source code version control systems
*   Vast number of add-on packages
*   Emacs will probably always be around
*   Emacs works well either with a mouse or without a mouse
*   Emacs has a large user base with multiple newsgroups
*   Benefits of using Emacs far outweigh the effort spent in learning it


<a name="Slide-3"></a>

## Emacs Lisp vs Common Lisp

*   Learning Emacs Lisp is useful and similar (but different from CL):
    *   Dynamic scope is everywhere
    *   No package system
    *   There are no reader (or reader-related) functions
    *   Does not support all the types that are supported in CL
    *   Incomplete implementation of CLOS (with the add-on EIEIO package)
    *   Not all of CL is supported
    *   No numerical tower support

*   Some good Emacs Lisp learning resources:
    *   [An Introduction to Programming in Emacs Lisp](http://www.gnu.org/manual/emacs-lisp-intro/emacs-lisp-intro.html)
    *   [Writing GNU Emacs Extensions](http://www.oreilly.com/catalog/gnuext/)
    *   [Wikemacs](http://wikemacs.org/wiki/Category:Emacs_Lisp)


<a name="Slide-slime"></a>

### SLIME: Superior Lisp Interaction Mode for Emacs

[SLIME](http://common-lisp.net/project/slime/) is the goto major mode
for CL programming.

*   Pros:
    *   Provides REPL which is hooked to implementation directly in Emacs
    *   Has integrated Common Lisp debugger with Emacs interface
    *   Interactive object-inspector in Emacs buffer
    *   Has own minor mode which enhances lisp-mode in many ways
    *   Supports every common Common Lisp implementation
    *   Readily available from MELPA
    *   Actively maintained
    *   Symbol completion
    *   Cross-referencing
    *   Can perform macroexpansions

*   Cons:
    *   Installing SLIME without MELPA can be tricky

*   Setup:
    *   Installing it from [MELPA](http://wikemacs.org/wiki/Melpa) is straightforward. Search package-list-packages for 'slime' and click to install. If MELPA is configured correctly, it will install itself and all dependencies.
    *   Run slime with `M-x slime`.


<a name="Slide-9"></a>

## Working with Lisp Code

In this short tutorial we'll see how to:

*   edit Lisp code
*   evaluate and compile Lisp code
*   search Lisp code
*   Note: Example code assumes you are using a setup similar to what is defined in the [.emacs file](https://github.com/LispCookbook/cl-cookbook/blob/master/.emacs) from the [CL Cookbook](windows.html) site.

In addition to the built-in Emacs commands, you have several packages at your disposal
that will help to keep the parens and/or the indentation balanced.
The list below is somewhat sorted by age of the
extension, according to the
[history of Lisp editing](https://github.com/shaunlebron/history-of-lisp-editing):

- [Paredit](https://www.emacswiki.org/emacs/ParEdit) - Paredit is a
  classic. It defines the must-have commands (move, kill, split, join
  a sexp,…).
  ([visual tutorial](http://danmidwood.com/content/2014/11/21/animated-paredit.html))
- [Smartparens](https://github.com/Fuco1/smartparens) - Smartparens
  not only deals with parens but with everything that comes in pairs
  (html tags,…) and thus has features for non-lispy languages.
- [Lispy](https://github.com/abo-abo/lispy) - Lispy reimagines Paredit
  with the goal to have the shortest bindings (mostly one key) that
  only act depending on the point position.
- [Paxedit](https://github.com/promethial/paxedit) - Paxedit adds
  commands based on the context (in a symbol, a sexp,… ) and puts
  efforts on whitespace cleanup and context refactoring.
- [Parinfer](http://shaunlebron.github.io/parinfer/) - Parinfer
  automatically fixes the parens depending on the indentation, or the
  other way round (or both !).

We personally advice to try Parinfer and the famous Paredit, then to
go up the list. See explanations and even more on
[Wikemacs](http://wikemacs.org/wiki/Lisp_editing).


<a name="Slide-10"></a>

### Editing

#### Forward/Backward/Up/Down movement and selection by s-expressions ([s1.lisp](s1.lisp) )

~~~lisp
{% include code/s1.lisp %}
~~~

#### Deleting s-expressions ( [s2.lisp](s2.lisp) )

~~~lisp
{% include code/s2.lisp %}
~~~

#### Indenting s-expressions ( [s3.lisp](s3.lisp) )

~~~lisp
{% include code/s3.lisp %}
~~~


#### Support for parenthesis ( [s4.lisp](s4.lisp) )

~~~lisp
{% include code/s4.lisp %}
~~~

#### Automatic code indentation (CL vs Elisp) ( [s5.lisp](s5.lisp) )

~~~lisp
{% include code/s5.lisp %}
~~~

<!-- close all parenthesis example ? s6.lisp -->

#### Code completion ( [s7.lisp](s7.lisp) )

Use the built-in `C-c TAB` to complete symbols. You can get tooltips
with [company-mode](http://wikemacs.org/wiki/Company-mode).

~~~lisp
{% include code/s7.lisp %}
~~~

#### Hiding/showing code ( [s8.lisp](s8.lisp) )

See also [code folding](http://wikemacs.org/wiki/Folding).

~~~lisp
{% include code/s8.lisp %}
~~~

#### Comments ( [s9.lisp](s9.lisp) )

~~~lisp
{% include code/s9.lisp %}
~~~


<a name="Slide-11"></a>

### Evaluating and Compiling Lisp

*   buffer
*   region
*   defun
*   sexp (previous/next)
*   DWIM
*   Example code ( [s11.lisp](s11.lisp) )

~~~lisp
{% include code/s11.lisp %}
~~~


<a name="Slide-12"></a>

### Searching Lisp Code

#### Standard Emacs text search (isearch forward/backward, regexp searches, search/replace) ( [s12.lisp](s12.lisp) )

~~~lisp
{% include code/s12.lisp %}
~~~

#### Finding occurrences (occur, grep) ( [s13.lisp](s13.lisp) )

~~~lisp
{% include code/s13.lisp %}
~~~

See also interactive versions with
[helm-swoop](http://wikemacs.org/wiki/Helm-swoop), helm-occur,
[ag.el](https://github.com/Wilfred/ag.el).

#### Lisp symbols in current source (imenu) ( [s14.lisp](s14.lisp) )

~~~lisp
{% include code/s14.lisp %}
~~~

See also helm-imenu and [imenu-anywhere](https://github.com/vspinu/imenu-anywhere).

#### Go to definition

Put the cursor on any symbol and press "M-." to go to its
definition. Press "M-," to come back.

#### Find who's calling, referencing, setting a symbol

See Slime's help menu. You can search and list "who" is *calling*,
*referencing*, *setting*, *binding*, *macroexpanding* symbols, and
more.

#### Lisp symbols in multiple source files (etags) ( [s16.lisp](s16.lisp) )

~~~lisp
{% include code/s16.lisp %}
~~~

#### Lisp symbols using [ECB](http://ecb.sourceforge.net/), the Emacs Code Browser ( [s17.lisp](s17.lisp) )

~~~lisp
{% include code/s17.lisp %}
~~~


<a name="Slide-13"></a>

## Lisp Documentation in Emacs - Learning About Lisp Symbols

### Argument lists

When you put the cursor on a function, Slime will show its signature
in the minibuffer.


### Documentation ( [s19.lisp](s19.lisp) )

~~~lisp
{% include code/s19.lisp %}
~~~


### Inspect ( [s21.lisp](s21.lisp) )

~~~lisp
{% include code/s21.lisp %}
~~~

### Macroexpand ( [s22.lisp](s22.lisp) )

~~~lisp
{% include code/s22.lisp %}
~~~


<a name="Slide-14"></a>

## Lisp Documentation in Emacs - Lisp Documentation

*   [CL HyperSpec](ftp://ftp.lispworks.com/pub/software_tools/documentation/HyperSpec-7-0.tar.gz)
*   [CLtL2](http://www-2.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/lang/lisp/doc/cltl/cltl_ht.tgz)
*   [ACL Documentation](https://franz.com/support/documentation/)
*   Example code ( [s23.lisp](s23.lisp) )

~~~lisp
{% include code/s23.lisp %}
~~~


<a name="Slide-15"></a>

## Miscellaneous

### Send to the REPL ( [s24.lisp](s24.lisp) )

~~~lisp
{% include code/s24.lisp %}
~~~

See also [eval-in-repl](https://github.com/kaz-yos/eval-in-repl) to send any form to the repl.

### Project Management

ASDF is the de-facto build facility. It is shipped in most Common Lisp implementations.

  * [ASDF](https://common-lisp.net/project/asdf/)
  * [ASDF best practices](https://gitlab.common-lisp.net/asdf/asdf/blob/master/doc/best_practices.md)

### Comparing versions of code ( [s10.lisp](s10.lisp) , [s10a.lisp](s10a.lisp) , [s10b.lisp](s10b.lisp) )

~~~lisp
{% include code/s10.lisp %}
~~~


<a name="Slide-16"></a>

## Questions/Answers

### Standard shell

*I switch between UNIX® and Windows environments and, although
Emacs makes this switch a lot easier, I find it inconvenient having to
use different Shell environments on different operating systems.*

On Windows, the [Cygwin tools](http://www.cygwin.com/) provide a
lot of the same tools that are available under UNIX® as well as a BASH
shell. Alternatively, you might want to consider using [eshell](http://wikemacs.org/wiki/Eshell), a shell
written in Emacs Lisp that comes as a standard feature in  Emacs.
If you use the given Emacs configuration, you can access eshell by pressing "F12".


### Using ACL tools with Emacs

*I would like to use Emacs with Franz's ACL but find that I use the
Franz tools so much that I can't afford to not load their IDE.*

It doesn't have to be an either/or decision. On Windows, Franz
allows you to specify (under Options) that Emacs is to be the default
editor in place of their built-in editor. On UNIX®, Emacs also works
very well together with the Franz tools.*

### Windows-style cut/copy/paste

*I want to use Emacs on a Windows machine. Unfortunately, I have
the Windows cut/copy/paste key bindings burned into my fingertips and
would find it very difficult to switch back and forth between the
Windows standard for these shortcut keys and the Emacs standard.*

Luckily, you don't have to! Download [cua.el](http://www.emacswiki.org/cgi-bin/wiki.pl?CuaMode) and you can continue to use the Windows
defaults. In fact, you may find that the following commands in your .emacs file will make Emacs more
Windows-like:

~~~lisp
;; Windows-like mouse/arrow movement & selection (pc-selection-mode)
(delete-selection-mode t)
;; C-z=Undo, C-c=Copy, C-x=Cut, C-v=Paste (needs cua.el)
(require 'cua) (CUA-mode t)
~~~


### Simplified Emacs setup

*There was a lot of Emacs Lisp code presented in this paper. Do I
really have to type in all this stuff to get started with Emacs and
Lisp?*

No, you can add yourself just what's needed to get Slime working.

You can try [Portacle](https://shinmera.github.io/portacle/) which has
everything ready.

There is also a
[sample .emacs file](https://github.com/LispCookbook/cl-cookbook/blob/master/.emacs)
that can be used to get started. It contains all of the configurations
that have been described in this page and (hopefully) should work with
some minor tweaking. See the
[CL-Cookbook](http://lispcookbook.github.io/cl-cookbook/) page on
"[Setting up an IDE with Emacs on Windows or Mac OS X](windows.html)".



### Alternatives to Emacs for CL programming

*I've tried out Emacs and I just can't get used to it. What other
Lisp-friendly alternative are there?*

  * Vim can be used to edit Lisp code. See some
    [vim plugins](https://github.com/CodyReichert/awesome-cl#vim).
  * There is an [Atom package](https://atom.io/packages/atom-slime).
  * The [Franz](https://franz.com/), [LispWorks](http://www.lispworks.com/), [Corman](http://www.cormanlisp.com/), and [Digitool](http://www.digitool.com/) commercial Lisp
offerings all have Lisp-aware editors.
  * Lastly, for true masochists, notepad on Windows or ed on UNIX® can also be used. ;-)



## Disclaimer

The original material on this page was originally presented at the [ILC 2003 conference](http://www.international-lisp-conference.org/index.html). A paper with more in-depth coverage of some of the material on this page can be found on [Bill Clementson's ILC2003](https://web.archive.org/web/20040213103100/http://home.comcast.net/~b.clementson/ilc_2003.htm) page, which is now archived.

It was edited in 2017.
