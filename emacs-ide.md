---
title: Using Emacs as an IDE
---

This page is meant to provide an introduction to using Emacs as a Lisp IDE. The key bindings used in the example code snippets assume an Emacs configuration similar to that provided by the [.emacs](https://github.com/LispCookbook/cl-cookbook/blob/master/.emacs) file that is included as part of the [Setting up an IDE with Emacs on Windows or Mac OS X](windows.html) page.


**Note**: [Portacle](https://shinmera.github.io/portacle/) is a
portable and multi-platform CL development environment, a
straightforward way to get going.


<a name="Slide-2"></a>

## Why Use Emacs?

*   Emacs has fantastic support for working with Lisp code
*   Not tying yourself into a single CL vendor's editor
*   Runs on virtually every OS and CL implementation
*   Extensible: [awesome-emacs](https://github.com/emacs-tw/awesome-emacs).
*   Can be customized to do many common tasks
*   Built-in support for different source code version control systems
*   Vast number of add-on packages
*   Emacs will probably always be around
*   Emacs works well either with a mouse or without a mouse
*   Emacs has a large user base with multiple newsgroups
*   Benefits of using Emacs far outweigh the effort spent in learning it
*   [org-mode](http://orgmode.org)
*   Because [Emacs Rocks !](http://emacsrocks.com)


<a name="Slide-3"></a>

## Emacs Lisp vs Common Lisp

*   Learning Emacs Lisp is useful and similar (but different from CL):
    *   Dynamic scope is everywhere
    *   There are no reader (or reader-related) functions
    *   Does not support all the types that are supported in CL
    *   Incomplete implementation of CLOS (with the add-on EIEIO package)
    *   Not all of CL is supported
    *   No numerical tower support

*   Some good Emacs Lisp learning resources:
    *   [An Introduction to Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/eintr.html)
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
    *   Has its own minor mode which enhances lisp-mode in many ways
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
    *   Enable the desired contribs (SLIME does very little by defaults), e.g. `(slime-setup '(slime-fancy slime-quicklisp slime-asdf))`.
    *   Run SLIME with `M-x slime`.

Check out this **[video tutorial](https://www.youtube.com/watch?v=sBcPNr1CKKw)** ! (and the author's channel, full of great stuff)

SLIME also has some nice extensions like
[Helm-SLIME](https://github.com/emacs-helm/helm-slime) which features, among
others
- Fuzzy completion,
- REPL and connection listing,
- Fuzzy-search of the REPL history,
- Fuzzy-search of the _apropos_ documentation.

#### REPL interactions

From the SLIME REPL, press `,` to prompt for commands.  There is completion
over the available systems and packages.  Examples:

- `,load-system`
- `,reload-system`
- `,in-package`
- `,restart-inferior-lisp`

and many more.

With the `slime-quicklisp` contrib, you can also `,ql` to list all systems
available for installation.

### SLY: Sylvester the Cat's Common Lisp IDE

[SLY](https://github.com/joaotavora/sly) is a SLIME fork that contains
the following improvements:

* Completely redesigned REPL based on Emacs's own full-featured comint.el
* Live code annotations via a new [sly-stickers](https://joaotavora.github.io/sly/#Stickers) contrib
* Consistent interactive button interface. Everything can be copied to the REPL.
* Multiple inspectors with independent history
* Regexp-capable M-x sly-apropos
* Contribs are first class SLY citizens, enabled by default, loaded with ASDF on demand.
* Support for [NAMED-READTABLES](https://github.com/joaotavora/sly-named-readtables), [macrostep.el](https://github.com/joaotavora/sly-macrostep) and [quicklisp](https://github.com/joaotavora/sly-quicklisp).

<a name="Slide-9"></a>

## Working with Lisp Code

In this short tutorial we'll see how to:

*   edit Lisp code
*   evaluate and compile Lisp code
*   search Lisp code
*   Note: Example code assumes you are using a setup similar to what is defined in the [.emacs file](https://github.com/LispCookbook/cl-cookbook/blob/master/.emacs) from the [CL Cookbook](windows.html) site.

### Packages for structured editing

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

#### Forward/Backward/Up/Down movement and selection by s-expressions

~~~lisp
{% include code/s1.lisp %}
~~~

#### Deleting s-expressions

With `C-M-k` and `C-M-backspace` (which may restart the system on gnu/linux):

~~~lisp
{% include code/s2.lisp %}
~~~

#### Indenting s-expressions

With `C-M-q`:

~~~lisp
{% include code/s3.lisp %}
~~~


#### Support for parenthesis

`M-(` insets a pair, `M-x check-parens` to spot malformed sexps, `C-u <n> M-(` to enclose sexps with parens:

~~~lisp
{% include code/s4.lisp %}
~~~

#### Automatic code indentation

~~~lisp
{% include code/s5.lisp %}
~~~

<!-- close all parenthesis example ? s6.lisp -->

#### Code completion

Use the built-in `C-c TAB` to complete symbols in SLIME. You can get tooltips
with [company-mode](http://wikemacs.org/wiki/Company-mode).

~~~lisp
{% include code/s7.lisp %}
~~~

#### Hiding/showing code

With `C-x n n` (narrow) and `C-x n w` to widen back.

See also [code folding](http://wikemacs.org/wiki/Folding).

~~~lisp
{% include code/s8.lisp %}
~~~

#### Comments

Insert a comment, comment a region with `M-;`, adjust text with `M-q`.

~~~lisp
{% include code/s9.lisp %}
~~~


<a name="Slide-11"></a>

### Evaluating and Compiling Lisp in SLIME

Compile the entire **buffer** by pressing `C-c C-k`.

Compile a **region** by selecting the first 2 forms in test-all and
running `M-x slime-compile-region`.

Compile a **defun** by putting the cursor inside the "test-format"
defun and pressing `C-c C-e`.

Compile the **sexp** before the point by putting the cursor after the
closing paren of `(test-format)` and pressing `C-c C-e`.

To **evaluate** rather than compile:
- evaluate a region with `C-c C-r`,
- evaluate a defun with `C-M-x`,
- evaluate the sexp before the point with `C-x C-e`.
See also other commands in the menu.

<a name="Slide-12"></a>

### Searching Lisp Code

#### Standard Emacs text search (isearch forward/backward, regexp searches, search/replace)

`C-s` does an incremental search forward (e.g. - as each key is
the search string is entered, the source file is searched for the
first match. This can make finding specific text much quicker as
you only need to type in the unique characters. Repeat searches
(using the same search characters) can be done by repeatedly
pressing `C-s`

`C-r` does an incremental search backward

`C-s RET` and `C-r RET` both do conventional string searches
(forward and backward respectively)

`C-M-s` and `C-M-r` both do regular expression searches (forward
and backward respectively)

`M-%` does a search/replace while `C-M-%` does a regular
expression search/replace


#### Finding occurrences (occur, grep)

With `M-x grep`, `rgrep`, `occur`,…

~~~lisp
{% include code/s13.lisp %}
~~~

See also interactive versions with
[helm-swoop](http://wikemacs.org/wiki/Helm-swoop), helm-occur,
[ag.el](https://github.com/Wilfred/ag.el).

#### Lisp symbols in current source (imenu)

~~~lisp
{% include code/s14.lisp %}
~~~

See also helm-imenu and [imenu-anywhere](https://github.com/vspinu/imenu-anywhere).

#### Go to definition

Put the cursor on any symbol and press `M-.` to go to its
definition. Press `M-,` to come back.

#### Crossreferencing: find who's calling, referencing, setting a symbol

Slime has a nice cross referencing facility, for example, you can see
what calls a particular function or expands a macro.  It presents a
list of places which reference a particular entity, from there you can
recompile the thing which references by pressing **C-c C-c** on that
line. **C-c C-k** will recompile all the references. This is useful when
modifying macros, inline functions, or constants.

The following bindings are also shown in Slime's menu:

- **C-c C-w c** *slime-who-calls* callers of a function
- **C-c C-w m** *slime-who-macroexpands* places where a macro is expanded
- **C-c C-w r** *slime-who-references* global variable references
- **C-c C-w b** *slime-who-bind* global variable bindings
- **C-c C-w s** *slime-who-sets* global variable setters
- **C-c C-w a** *slime-who-specializes* methods specialized on a symbol

And when the `slime-asdf` contrib is enabled,
**C-c C-w d** *slime-who-depends-on* lists dependent ASDF systems

And a general binding: **M-? or M-_** *slime-edit-uses** combines all
of the above, it lists every kind of references.

(thanks to [Slime tips](https://slime-tips.tumblr.com/page/2))


#### Lisp symbols in multiple source files (etags)

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

When you put the cursor on a function, SLIME will show its signature
in the minibuffer.

### Documentation lookup

- **C-c C-d h**  looks up documentation in CLHS. But it works only on symbols, so there are two more bindings:
- **C-c C-d #** for reader macros
- **C-c C-d ~**  for format directives

Other bindings which may be useful:

- **C-c C-d d**  describes a symbol using `describe`
- **C-c C-d f**  describes a function using `describe`

### Documentation

~~~lisp
{% include code/s19.lisp %}
~~~


### Inspect

~~~lisp
{% include code/s21.lisp %}
~~~

### Macroexpand

~~~lisp
{% include code/s22.lisp %}
~~~


<a name="Slide-14"></a>

## Lisp Documentation in Emacs - Lisp Documentation

*   [CL HyperSpec (online)](http://www.lispworks.com/documentation/HyperSpec/Front/)
*   [CL HyperSpec (tarball)](http://macports.mirror.ac.za/distfiles/lisp-hyperspec/HyperSpec-7-0.tar.gz)
*   [CLtL2](http://www-2.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/lang/lisp/doc/cltl/cltl_ht.tgz)
*   [ACL Documentation](https://franz.com/support/documentation/)
*   Example code ( [s23.lisp](s23.lisp) )

~~~lisp
{% include code/s23.lisp %}
~~~


<a name="Slide-15"></a>

### Consult the CLHS offline

~~~lisp
(ql:quickload "clhs")
~~~

Then add this to your Emacs configuration:

~~~lisp
(load "~/.quicklisp/clhs-use-local.el" 'noerror)
~~~

## Miscellaneous

### Synchronizing packages

**C-c ~** (*slime-sync-package-and-default-directory*): When run in a
buffer with a lisp file it will change the current package of the REPL
to the package of that file and also set the current directory of the REPL
to the parent directory of the file.

### Calling code

**C-c C-y** (*slime-call-defun*): When the point is inside a defun and
C-c C-y is pressed,

(I’ll use [] as an indication where the cursor is)


~~~lisp
(defun foo ()
 nil[])
~~~


then `(foo [])` will be inserted into the REPL, so that you can write
additional arguments and run it.


If `foo` was in a different package than the package of the REPL,
`(package:foo )` or `(package::foo )` will be inserted.

This feature is very useful for testing a function you just wrote.

That works not only for defun, but also for defgeneric, defmethod,
defmacro, and define-compiler-macro in the same fashion as for defun.

For defvar, defparameter, defconstant: `[] *foo*` will be inserted
(the cursor is positioned before the symbol so that you can easily
wrap it into a function call).

For defclass: `(make-instance ‘class-name )`.

**Inserting calls to frames in the debugger**

**C-y** in SLDB on a frame will insert a call to that frame into the REPL, e.g.,

```
(/ 0) =>
…
1: (CCL::INTEGER-/-INTEGER 1 0)
…
```

**C-y** will insert `(CCL::INTEGER-/-INTEGER 1 0)`.

(thanks to [Slime tips](https://slime-tips.tumblr.com/page/2))

### Send to the REPL

~~~lisp
{% include code/s24.lisp %}
~~~

See also [eval-in-repl](https://github.com/kaz-yos/eval-in-repl) to send any form to the repl.

### Exporting symbols

**C-c x** (*slime-export-symbol-at-point*) from the `slime-package-fu`
contrib: takes the symbol at point and modifies the `:export` clause of
the corresponding defpackage form. It also exports the symbol.  When
called with a negative argument (C-u C-c x) it will remove the symbol
from `:export` and unexport it.

**M-x slime-export-class** does the same but with symbols defined
by a structure or a class, like accessors, constructors, and so on.
It works on structures only on SBCL and Clozure CL so far.
Classes should work everywhere with MOP.

Customization

There are different styles of how symbols are presented in
`defpackage`, the default is to use uninterned symbols (`#:foo`).
This can be changed:

to use keywords:


~~~lisp
(setq slime-export-symbol-representation-function
      (lambda (n) (format ":%s" n)))
~~~

or strings:

~~~lisp
(setq slime-export-symbol-representation-function
 (lambda (n) (format "\"%s\"" (upcase n))))
~~~


### Project Management

ASDF is the de-facto build facility. It is shipped in most Common Lisp implementations.

  * [ASDF](https://common-lisp.net/project/asdf/)
  * [ASDF best practices](https://gitlab.common-lisp.net/asdf/asdf/blob/master/doc/best_practices.md)

### Comparing versions of code (ediff)

Start the ediff utility by entering `M-x ediff`. Enter two file names, press
the space bar to step through the changes, and `q`
to exit.

Of course, see also [magit](https://magit.vc/) for a wonderful git integration into Emacs.

<a name="Slide-16"></a>

## Questions/Answers

### utf-8 encoding

You might want to set this to your init file:

~~~lisp
(set-language-environment "UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
~~~

and for Sly:

~~~lisp
(setq sly-lisp-implementations
          '((sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)
            ))
~~~

This will avoid getting `ascii stream decoding error`s when you have
non-ascii characters in files you evaluate with SLIME.


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

No, you can add yourself just what's needed to get SLIME working.

You can try [Portacle](https://shinmera.github.io/portacle/) which has
everything ready.

There is also a
[sample .emacs file](https://github.com/LispCookbook/cl-cookbook/blob/master/.emacs)
that can be used to get started. It contains all of the configurations
that have been described in this page and (hopefully) should work with
some minor tweaking. See the
[CL-Cookbook](http://lispcookbook.github.io/cl-cookbook/) page on
"[Setting up an IDE with Emacs on Windows or Mac OS X](windows.html)".


## Disclaimer

The original material on this page was originally presented at the [ILC 2003 conference](http://www.international-lisp-conference.org/index.html). A paper with more in-depth coverage of some of the material on this page can be found on [Bill Clementson's ILC2003](https://web.archive.org/web/20040213103100/http://home.comcast.net/~b.clementson/ilc_2003.htm) page, which is now archived.

It was edited in 2017.
