---
title: Using Emacs as an IDE
---

This page is meant to provide an introduction to using [Emacs](https://www.gnu.org/software/emacs/) as a Lisp IDE.

We divided it roughly into 2 sections: how to use Slime or Sly, and complementary information on built-in Emacs commands to work with Lisp code.

We want to bring [Portacle](https://shinmera.github.io/portacle/) to your attention. It is a portable and
multi-platform CL development environment shipping Emacs, Slime, SBCL, git and
necessary extensions. It is a straightforward way to get going.

![](assets/emacs-teaser.png)

<!-- todo: C-u M-x slime and its configuration to work with multiple implementations -->


## Why Use Emacs?

*   Emacs has fantastic support for working with Lisp code
*   Not tying yourself into a single CL vendor's editor
*   Runs on virtually every OS and CL implementation
*   Emacs will probably always be around
*   Emacs works well either with a mouse or without a mouse
*   Emacs works well either in GUI mode or in the terminal
*   Emacs has a large user base with multiple newsgroups
*   Built-in tree-sitter and LSP support
*   Excellent vim mode
*   Because [Org-mode](http://orgmode.org)
*   Because [Magit](https://magit.vc/)
*   Because [Emacs Rocks !](http://emacsrocks.com)
*   Vast number of extensions: [awesome-emacs](https://github.com/emacs-tw/awesome-emacs).


## SLIME: Superior Lisp Interaction Mode for Emacs

[SLIME](http://common-lisp.net/project/slime/) is the goto major mode
for CL programming. It has a lot of features that make it a powerful, integrated and very interactive development environment.

* it provides a REPL which is hooked to the running image, directly in Emacs,
* it integrates the Common Lisp debugger with an Emacs interface
* it provides symbol completion,
* code evaluation, compilation, macroexpansion
* cross-referencing,
* breaking, stepping, tracing,
* go to definition,
* online documentation,
* fuzzy searching functions and symbols, system names, documentation,
* an interactive object inspector,
* it supports every common Common Lisp implementation,
* multiple connections and multiple listener buffers (mrepl)
* it is readily available from MELPA
* it is actively maintained.


## SLY: Sylvester the Cat's Common Lisp IDE

[SLY](https://github.com/joaotavora/sly) is a SLIME fork that contains
the following improvements:

* Completely redesigned REPL based on Emacs's own full-featured comint.el.
* Live code annotations via the [Stickers](https://joaotavora.github.io/sly/#Stickers) feature.
* Consistent interactive button interface. Everything can be copied to the REPL.
* Multiple REPLs.
* Multiple inspectors with independent history.
* Regexp-capable `M-x sly-apropos`.
* Contribs are first class SLY citizens, enabled by default, loaded with ASDF on demand:
  - [NAMED-READTABLES](https://github.com/joaotavora/sly-named-readtables) support
  - [macrostep.el](https://github.com/joaotavora/sly-macrostep)
  - [Quicklisp](https://github.com/joaotavora/sly-quicklisp)
  - [ASDF](https://github.com/mmgeorge/sly-asdf)
  - [Evaluation Overlays](https://git.sr.ht/~fosskers/sly-overlay)

Sly is shipped by default in [Doom Emacs](https://github.com/doomemacs/doomemacs/).

## Installing SLIME or SLY

### Manually

On Ubuntu, SLIME is easily installed alongside Emacs and SBCL:

    sudo apt install emacs slime sbcl

Otherwise, install SLIME by adding this code to your `~/.emacs.d/init.el` file:

~~~lisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(dolist (package '(slime))
  (unless (package-installed-p package)
    (package-install package)))
(require 'slime)
~~~

assuming you've also instealled Emacs and SBCL.

Since SLIME is heavily modular and the defaults only do the bare minimum (not
even the SLIME REPL), you might want to enable more features with

~~~lisp
(require 'slime)
(slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-mrepl))
~~~

After this you can press Alt-X on your keyboard and type `slime` and try Common Lisp!

(Alt-X is often written `M-x` in Emacs-world.)

For more details, consult the
[documentation](https://common-lisp.net/project/slime/doc/html/) (also available
as an Info page).

Now you can run SLIME with, as mentioned, `M-x slime` and/or `M-x slime-connect`.

See also:

* [https://github.com/susam/emacs4cl](https://github.com/susam/emacs4cl) - a minimal Emacs configuration to get new users up and running quickly, with a tutorial.
* [https://wikemacs.org/wiki/SLIME](https://wikemacs.org/wiki/SLIME) - configuration examples and extensions.

### Portacle

[Portacle](https://shinmera.github.io/portacle/) is a portable and
multi-platform CL development environment with which you can start
developping in Lisp in a few clicks. Portacle includes Emacs, SBCL, Slime, git, and useful Emacs extensions (which-key, treeview…).

It is a straightforward way to get going.


### Doom Emacs

[Doom Emacs](https://github.com/doomemacs/doomemacs/) is a popular Emacs configuration. You can easily enable its Sly integration.


### SLIME fancy and contrib packages

SLIME's functionalities live in packages and so-called [contrib
modules](https://common-lisp.net/project/slime/doc/html/Contributed-Packages.html)
must be loaded to add further functionalities. The afored mentioned
`slime-fancy` includes:


- slime-autodoc
- slime-c-p-c
- slime-editing-commands
- slime-fancy-inspector
- slime-fancy-trace
- slime-fontifying-fu
- slime-fuzzy
- slime-mdot-fu
- slime-macrostep
- slime-presentations
- slime-references
- slime-repl
- slime-scratch
- slime-package-fu
- slime-trace-dialog
- [slime-mrepl](https://slime.common-lisp.dev/doc/html/slime_002dmrepl.html#slime_002dmrepl) (multiple REPLs)

SLIME also has some nice extensions like
[Helm-SLIME](https://github.com/emacs-helm/helm-slime) which features, among
others:

- Fuzzy completion,
- REPL and connection listing,
- Fuzzy-search of the REPL history,
- Fuzzy-search of the _apropos_ documentation.


## Working with SLIME (or SLY)

One of the first things you might want to do is to compile and load some Lisp code. Use `C-c C-c` on a function or `C-c C-k` to compile a whole file. But that's not all, read on.

Note that we give function names for SLIME. They are most of the time similar with SLY.

### Code completion

Use the built-in `C-c TAB` to complete symbols in SLIME. You can get tooltips
with [company-mode](http://company-mode.github.io/).

![](assets/emacs-company-elisp.png "Lisp symbols completion with company-mode tooltips.")

In the **REPL**, it's simply **TAB**.

Use Emacs' hippie-expand, bound to `M-/`, to complete any string present in other open buffers.

### Evaluating and Compiling Lisp in SLIME

Compile the entire **buffer** by pressing `C-c C-k` (`slime-compile-and-load-file`).

Compile a **region** with `M-x slime-compile-region`.

Compile a **defun** by putting the cursor inside it and pressing `C-c C-c` (`slime-compile-defun`).

Once you compiled some code, you can try it, for example on the REPL.


To **evaluate** rather than compile:

- evaluate the **sexp** before the point by putting the cursor after
  its closing paren and pressing `C-x C-e`
  (`slime-eval-last-expression`). The result is printed in the minibuffer.
- similarly, use `C-c C-p` (`slime-pprint-eval-last-expression`) to eval and pretty-print the expression before point. It shows the result in a new "slime-description" buffer.
- use `M-x slime-eval-print-last-expression` (unbound by default) to print the result in the same file, under the cursor.
- evaluate a region with `C-c C-r`,
- evaluate a defun with `C-M-x`,
- type `C-c C-e` (`slime-interactive-eval`) to get a prompt that asks for code to eval in the current context. It prints the result in the minibuffer. With a prefix argument, insert the result into the current buffer.
- type `C-c C-j` (`slime-eval-last-expression-in-repl`), when the cursor is after the closing parenthesis of an expression, to send this expression to the REPL and evaluate it.

See also other commands in the menu.

But what's the difference between evaluating and compiling some code?

### evaluation VS compilation

There are a couple of pragmatic differences when choosing between compiling or evaluating.
In general, it is better to *compile* top-level forms, for two reasons:

* Compiling a top-level form highlights warnings and errors in the editor, whereas evaluation does not.
* SLIME keeps track of line-numbers of compiled forms, but when a top-level form is evaluated, the file line number information is lost. That's problematic for code navigation afterwards.

`eval` is still useful to observe results from individual non top-level forms. For example, say you have this function:


~~~lisp
(defun foo ()
  (let ((f (open "/home/mariano/test.lisp")))
    ...))
~~~

Go to the end of the OPEN expression and evaluate it (`C-x C-e`), to observe the result:

```
=> #<SB-SYS:FD-STREAM for "file /mnt/e6b00b8f-9dad-4bf4-bd40-34b1e6d31f0a/home/marian/test.lisp" {1003AAAB53}>
```

Or on this example, with the cursor on the last parentheses, press `C-x C-e` to evaluate the `let`:

~~~lisp
(let ((n 20))
  (loop for i from 0 below n
     do (print i)))
~~~

You should see numbers printed in the REPL.


See also [eval-in-repl](https://github.com/kaz-yos/eval-in-repl) to send any form to the repl.


### Debugging

We cover debugging commands in its own [debugging](debugging.html) chapter.

### Go to definition

Put the cursor on any symbol and press `M-.` (`slime-edit-definition`) to go to its
definition. Press `M-,` to come back.

### Go to any symbol, list symbols in current source

Use `C-u M-.` (`slime-edit-definition` with a prefix argument, also available as `M-- M-.`) to autocomplete the symbol and navigate to it.

This command always asks for a symbol even if the cursor is on one. It works with any loaded definition. Here's a little [demonstration video](https://www.youtube.com/watch?v=ZAEt73JHup8).

You can think of it as a `imenu` completion that always work for any Lisp symbol. Add in [Slime's fuzzy completion][slime-fuzzy] for maximum powerness!

### Argument lists

When you put the cursor on a function, SLIME will show its signature
in the minibuffer.

If you want to see them better, try `C-c C-s` after a function name.

For example, you forgot how to use `with-open-file`. Write it:

```lisp
(with-open-file
```

now press `C-c C-s` (`slime-complete-form`) and you'll get:

```lisp
(with-open-file (stream filespec :direction direction
                                 :element-type element-type
                                 :if-exists if-exists
                                 :if-does-not-exist if-does-not-exist
                                 :external-format external-format
                                 :class class
                         )
           body...)
```

written in your source file (or in the REPL).

The minibuffer will show you the default values of the arguments.

### Documentation lookup

The main shortcut to know is:

- **C-c C-d d**  shows the symbols' documentation on a new window (same result as using `describe`).

Other bindings which may be useful:

- **C-c C-d f**  describes a function
- **C-c C-d h**  looks up the symbol documentation in CLHS by opening the web browser. But it works only on symbols, so there are two more bindings:
- **C-c C-d #** for reader macros
- **C-c C-d ~**  for format directives

You can enhance the help buffer with the Slime extension [slime-doc-contribs](https://github.com/mmontone/slime-doc-contribs). It will show more information in a nice looking buffer, and it will add choices to the documentation command:

* **slime-help-package** will display information about a CL package: it will nicely show its exported variables, conditions, classes, generic functions, functions and macros, with their documentation. It is a great way to see at a glance what a package provides.
* **slime-help-system** does the same for a *system*.
* **slime-help-apropos-documentation** will show symbols whose documentation contains matches for "PATTERN", which is a great way to lookup for functions.
* and more.

![](https://github.com/mmontone/slime-doc-contribs/raw/master/slime-help.png)


### Inspector

You can call `(inspect 'symbol)` from the REPL or call it with `C-c I` from a source file.

Learn to use with [its documentation](https://slime.common-lisp.dev/doc/html/Inspector.html#Inspector): use `l` to come back to the previous object, `*` to copy the object at point… and more.

### Macroexpand

Use `C-c M-m` to macroexpand a macro call


### Crossreferencing: find who's calling, referencing, setting a symbol

Slime has nice cross-referencing facilities. For example, you can ask
what calls a particular function, what expands a macro, or where a global variable is being used.

Results are presented in a new buffer, listing the places which reference a particular entity.
From there, we can press Enter to go to the corresponding source line,
or more interestingly we can recompile the place at point by pressing **C-c C-c** on that
line. Likewise, **C-c C-k** will recompile all the references. This is useful when
modifying macros, inline functions, or constants.

The bindings are the following (they are also shown in Slime's menu):

- **C-c C-w c** (`slime-who-calls`) callers of a function
- **C-c C-w m** (`slime-who-macroexpands`) places where a macro is expanded
- **C-c C-w r** (`slime-who-references`) global variable references
- **C-c C-w b** (`slime-who-bind`) global variable bindings
- **C-c C-w s** (`slime-who-sets`) global variable setters
- **C-c C-w a** (`slime-who-specializes`) methods specialized on a symbol

And when the `slime-asdf` contrib is enabled,
**C-c C-w d** (`slime-who-depends-on`) lists dependent ASDF systems

And a general binding: **M-?** or **M-_** (`slime-edit-uses`) combines all
of the above, it lists every kind of references.

### Systems interactions

In Slime, you can use the usual `C-c C-k` in an .asd file to compile and load it, then `ql:quickload` (or `asdf:load-system`) to effectively load the system. SLIME offers more interactive commands to interact with Lisp systems:

- `M-x slime-load-system`: offers a prompt to **select an ASDF system**, with **autocompletion** of projects collected from where ASDF sees Common Lisp projects, then compile and load the system. The default system name is taken from the first file matching *.asd in the current buffer's working directory.
  - note that the system name is inferred from the .asd file name. The real system name defined inside may be different.
  - to understand where ASDF looks for Lisp systems, read the [getting started](getting-started.html) page, section "How to load an existing project".
- `M-x slime-open-system`: this opens a new buffer for all source files of a given system.
- `M-x slime-browse-system`: this command opens a Dired buffer to browse the files of a system.
- `M-x slime-rgrep-system`: run `rgrep` on the base directory of a system.
- `M-x slime-isearch-system`: run `isearch` on the files of a system.
- `M-x slime-query-replace-system`: run `query-replace` on an ASDF system.
- `M-x slime-save-system`: save all files belongign to a system.
- `M-x slime-delete-system-fasls`: this deletes the cached .fasl files for this system.

Sly users have a more featureful `sly-load-system` command that will search the .asd file on the current directory and in parent directories.


### REPL interactions

From the SLIME REPL, press `,` to prompt for commands.  There is completion
over the available systems and packages.  Examples:

- `,load-system`
- `,reload-system`
- `,in-package`
- `,restart-inferior-lisp`

and many more. Usually the interactive commands given in the previous section have a REPL shortcut.

With the `slime-quicklisp` contrib, we can use `,ql` to
autocomplete a system to install, from all systems available for
installation.

In addition, we can use the [Quicklisp-systems](https://github.com/mmontone/quicklisp-systems) Slime extension to search, browse and load Quicklisp systems from Emacs.

### Sending code to the REPL

You can write code in the REPL, but you can also interact with code directly from the source file.

We saw **C-c C-j**, that sends the expression at point to the REPL and evaluates it.

**C-c C-y** (`slime-call-defun`): send code to the REPL.

When the point is inside a defun and C-c C-y is pressed (below I’ll use [] as an indication where the cursor is)

~~~lisp
(defun foo ()
 nil[])
~~~


then `(foo [])` will be inserted into the REPL, so that you can write
additional arguments and run it.


If `foo` was in a different package than the package of the REPL,
`(package:foo )` or `(package::foo )` will be inserted.

This feature is very useful for testing a function you just wrote.

That works not only for a `defun`, but also for `defgeneric`, `defmethod`,
`defmacro`, and `define-compiler-macro` in the same fashion as for defun.

For `defvar`, `defparameter`, `defconstant`: `[] *foo*` will be inserted
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


### Synchronizing packages

**C-c ~** (`slime-sync-package-and-default-directory`): When run in a
buffer with a lisp file it will change the current package of the REPL
to the package of that file and also set the current directory of the REPL
to the parent directory of the file.

### Exporting symbols

Slime provides a shortcut to add export declarations to your package, effectively exporting one or many symbol(s), or on the contrary un-exporting it.

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

to use keywords, add this to your Emacs init file:


~~~lisp
(setq slime-export-symbol-representation-function
      (lambda (n) (format ":%s" n)))
~~~

or strings:

~~~lisp
(setq slime-export-symbol-representation-function
 (lambda (n) (format "\"%s\"" (upcase n))))
~~~


### (optional) Consult the Hyper Spec (CLHS) offline

The [Common Lisp Hyper Spec](http://www.lispworks.com/documentation/common-lisp.html) is the
official online version of the ANSI Common Lisp standard. We can start
browsing it from [starting points](http://www.lispworks.com/documentation/HyperSpec/Front/StartPts.htm):
a shortened [table of contents of highlights](http://www.lispworks.com/documentation/HyperSpec/Front/Hilights.htm),
a [symbols index](http://www.lispworks.com/documentation/HyperSpec/Front/Hilights.htm),
a glossary, a master index.

Since January of 2023, we have the Common Lisp Community Spec: [https://cl-community-spec.github.io/pages/index.html](https://cl-community-spec.github.io/pages/index.html), a new web rendering of the specification. It is a more modern rendering:

* it has a *search box*
* it has *syntax highlihgting*
* it is hosted on GitHub and we have the right to modify it: https://github.com/fonol/cl-community-spec

If you want other tools to do a quick look-up of symbols on the CLHS,
since the official website doesn't have a search bar, you can use:
* Xach's website search utility: [https://www.xach.com/clhs?q=with-open-file](https://www.xach.com/clhs?q=with-open-file)
* the l1sp.org website: [http://l1sp.org/search?q=with-open-file](http://l1sp.org/search?q=with-open-file),
* and we can use Duckduckgo's or Brave Search's `!clhs` "bang".

We can **browse the CLHS offline** with [Dash](https://kapeli.com/dash) on MacOS, [Zeal](https://zealdocs.org/) on GNU/Linux and [Velocity](https://velocity.silverlakesoftware.com/) on Windows.

But we can also browse it offline from Emacs. We have to install a CL package and to configure the Emacs side with one command:

~~~lisp
(ql:quickload "clhs")
(clhs:install-clhs-use-local)
~~~

Then add this to your Emacs configuration:

~~~lisp
(load "~/quicklisp/clhs-use-local.el" 'noerror)
~~~

Now, you can use `C-c C-d h` to look-up the symbol at point in the
HyperSpec. This will open your browser, but look at its URL starting
with "file://home/": it opens a local file.

Other commands are available:

* when you want to look-up a reader macro, such as `#'`
  (sharpsign-quote) or `(` (left-parenthesis), use
  `M-x common-lisp-hyperspec-lookup-reader-macro`, bound to `C-c C-d #`.
* to look-up a `format` directive, such as `~A`, use `M-x
  common-lisp-hyperspec-format`, bound to `C-c C-d ~`.
  * of course, you can TAB-complete on Emacs' minibuffer prompt to see all the available format directives.
* you can also look-up glossary terms (for example, you can look-up "function" instead of "defun"), use `M-x common-lisp-hyperspec-glossary-term`, bound to `C-c C-d g`.


## Working with Emacs

In this section we'll learn the most useful Emacs commands to work with Lisp code in general, or to perform common actions.

We'll start by how to find your way into Emacs' built-in documentation. If there is a skill you should learn, that is the one.

Don't forget that Emacs both GUI and terminal interfaces have menus, they help in discovering all available commands. If you don't see one, ensure that your emacs configuration doesn't hide it. Display the menu with `M-x menu-bar-mode`.

### Built-in documentation

Emacs comes with built-in tutorials and documentation. Moreover, it is
a self-documented and self-discoverable editor, capable of introspection to let you
know about the current keybindings, to let you search about function documentation,
available variables,source code, tutorials, etc. Whenever you ask yourself questions like
"what are the available shortcuts to do x" or "what does this
keybinding really do", the answer is most probably a keystroke away,
right inside Emacs. You should learn a few keybindings to be able to
discover Emacs with Emacs flawlessly.

The help on the topic is here:

- [Help page: commands for asking Emacs about its commands](https://www.gnu.org/software/emacs/manual/html_node/emacs/Help.html#Help)

The help keybindings start with either `C-h` or `F1`. Important ones are:

- `C-h k <keybinding>`: what function does this keybinding call?
- `C-h f <function name>`: what keybinding is linked to this function?
- `C-h a <topic>`: show a list of commands whose name match the given *topic*. It accepts a keyword, a list of keywords or a regular expression.
- `C-h i`: show the Info page, a menu of major topics.

Some Emacs packages give even more help.

### More help and discoverability packages

Sometimes, you start typing a key sequence but you can't remember it
completely. Or, you wonder what other keybindings are related. Comes
[which-key-mode](https://github.com/justbur/emacs-which-key). This
packages will display all possible keybindings starting with the key(s) you just typed.

For example, I know there are useful keybindings under `C-x` but I don't remember which ones… I just type `C-x`, I wait for half a second, and which-key shows all the ones available.

![](assets/emacs-which-key-minibuffer.png)

Just try it with `C-h` too!

See also [Helpful](https://github.com/Wilfred/helpful), an alternative to the built-in Emacs help that provides much more contextual information.

<img src="assets/emacs-helpful.png" style="height: 450px"/>


### Built-in tutorial

Emacs ships its own tutorial. You should give it a look to learn the most important keybindings and concepts.

Call it with `M-x help-with-tutorial` (where `M-x` is `alt-x`).



### Editing

Emacs has, of course, built-in commands to deal with s-expressions.

#### Forward/Backward/Up/Down movement and selection by s-expressions

Use `C-M-f` and `C-M-b` (`forward-sexp` and `backward-sexp`) to move
in units of s-expressions.

Use `C-M-t` to swap
the first addition sexp and the second one. Put the cursor on the open
parens of "(+ x" in defun c and press

Use `C-M-@` to highlight an entire sexp. Then press `C-M-u` to expand
the selection "upwards" and `C-M-d` to move forward down one level of
parentheses.

#### Deleting s-expressions

Use `C-M-k` (`kill-sexp`) and `C-M-backspace` (`backward-kill-sexp`) (but caution: this keybinding may restart the system on GNU/Linux).

For example, if point is before `(progn` (I’ll use [] as an indication where the cursor is):

~~~lisp
(defun d ()
  (if t
      (+ 3 3)
     [](progn
        (+ 1 1)
        (if t
            (+ 2 2)
            (+ 3 3)))
      (+ 4 4)))
~~~

and you press `C-M-k`, you get:

~~~lisp
(defun d ()
  (if t
      (+ 3 3)
      []
      (+ 4 4)))
~~~


#### Indenting s-expressions

Indentation is automatic for Lisp forms.

Pressing TAB will indent incorrectly indented code. For example, put
the point at the beginning of the `(+ 3 3)` form and press TAB:

~~~lisp
(progn
(+ 3 3))
~~~

you correctly get

~~~lisp
(progn
  (+ 3 3))
~~~

Use `C-M-q` (`slime-reindent-defun`) to indent the current function definition:

~~~lisp
;; Put the cursor on the open parens of "(defun ..."
;; and press "C-M-q" to indent the code:
(defun e ()
"A badly indented function."
(let ((x 20))
(loop for i from 0 to x
do (loop for j from 0 below 10
do (print j))
(if (< i 10)
(let ((z nil) )
(setq z (format t "x=~d" i))
(print z))))))

;; This is the result:

(defun e ()
  "A badly indented function (now correctly indented)."
  (let ((x 20))
    (loop for i from 0 to x
       do (loop for j from 0 below 10
             do (print j))
         (if (< i 10)
             (let ((z nil) )
               (setq z (format t "x=~d" i))
               (print z))))))
~~~

You can also select a region and call `M-x indent-region`.

#### Open and close parenthesis

When you are in a Slime REPL, you can use `C-return` or `M-return`
(`slime-repl-closing-return`) to close the remaining parenthesis and
evaluate your input string.

In files, use `M-(` to insert a pair of parenthesis (`()`) and the same
keybinding with a prefix argument, `C-u M-(`, to enclose the
expression in front of the cursor with a pair of parens.

For example, we start with the cursor before the first paren:

~~~lisp
CL-USER> |(- 2 2)
~~~

Press `C-u M-(` to enclose it with parens:

~~~lisp
CL-USER> (|(- 2 2))
;; now write anything.
CL-USER> (zerop (- 2 2))
~~~

With a numbered prefix argument (`C-u 2 M-(`), wrap around this number of s-expressions.

Additionnaly, use `M-x check-parens` to spot malformed s-exps and `C-c
C-]` (`slime-close-all-parens-in-sexp`) to insert the required number
of closing parenthesis.

There are additional packages that can make your use of parens easier:

- `M-x show-paren-mode`, a built-in Emacs mode: it toggles the
  visualization of matching parenthesis. When enabled, place the
  cursor on a paren and you'll see the other paren it matches
  with. You can initialize it in your Emacs init file with
  `(show-paren-mode t)`. It is a global minor mode (it will work for
  all buffers, all languages).
- when evil-mode (the vim layer) is enabled, you can use the `%` key to go to the matchin paren.
- `M-x electric-pair-mode`, a built-in Emacs mode: when enabled,
typing an open parenthesis automatically inserts the corresponding
closing parenthesis, and vice versa.  (Likewise for brackets, etc.).
If the region is active, the parentheses (brackets, etc.) are inserted
around the region instead.
- you could use [Paredit (animated guide)](http://danmidwood.com/content/2014/11/21/animated-paredit.html) to automatically insert parentheses in pairs,
- or [lispy-mode](https://github.com/abo-abo/lispy), like Paredit, but a key triggers an action when the cursor is placed right before or right after a parentheses.


#### Hiding/showing code

Use `C-x n n` (narrow-to-region) and `C-x n w` to widen back.

See also [code folding](http://wikemacs.org/wiki/Folding) with external packages.

#### Comments

Insert a comment or comment a region with `M-;`, adjust text with `M-q`.

### (optional) Packages for structured editing

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


### Search and replace

#### isearch forward/backward, regexp searches, search/replace

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

Use `M-x grep`, `rgrep`, `occur`…

See also interactive versions with
[helm-swoop](http://wikemacs.org/wiki/Helm-swoop), helm-occur,
[ag.el](https://github.com/Wilfred/ag.el).


## Questions/Answers

### Emacs Lisp vs Common Lisp

It isn't necessary to write Emacs Lisp in order to use Emacs with Slime or Sly for Common Lisp.

However learning Emacs Lisp can be useful and is similar (but different) from CL:
*   Dynamic scope is everywhere
*   There are no reader (or reader-related) functions
*   Does not support all the types that are supported in CL
*   Incomplete implementation of CLOS (with the add-on EIEIO package)
*   No numerical tower support

Some good Emacs Lisp learning resources:
*   [An Introduction to Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/eintr.html)
*   [Writing GNU Emacs Extensions](http://www.oreilly.com/catalog/gnuext/)
*   [Wikemacs](http://wikemacs.org/wiki/Category:Emacs_Lisp)

### What about LSP (Language Server Protocol)?

LSP server and client ports for Common Lisp exist, but we don't *need* them to have a high quality IDE integration. In fact, Slime/Swank follow a client/server architecture, like LSP, but Slime predates LSP by decades, and still offers much more features for lispers than LSP.

### utf-8 encoding

You might want to set this to your init file:

~~~lisp
(set-language-environment "UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
~~~

and for Sly:

~~~lisp
(setf sly-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)
        ))
~~~

This will avoid getting `ascii stream decoding error`s when you have
non-ascii characters in files you evaluate with SLIME.


### Default cut/copy/paste keybindings

*I am so used to C-c, C-v and friends to copy and paste text that
the default Emacs shortcuts don't make any sense to me.*

Luckily, you have a solution! Install [cua-mode](http://www.emacswiki.org/cgi-bin/wiki.pl?CuaMode) and you can continue to use these shortcuts.

~~~lisp
;; C-z=Undo, C-c=Copy, C-x=Cut, C-v=Paste (needs cua.el)
(require 'cua) (CUA-mode t)
~~~


## Appendix

### All Slime REPL shortcuts

Here is the reference of all Slime shortcuts that work in the REPL.

To see them, go in a REPL, type `C-h m` and go to the Slime REPL map section.


```
REPL mode defined in ‘slime-repl.el’:
Major mode for interacting with a superior Lisp.
key             binding
---             -------

C-c             Prefix Command
C-j             slime-repl-newline-and-indent
RET             slime-repl-return
C-x             Prefix Command
ESC             Prefix Command
SPC             slime-space
  (that binding is currently shadowed by another mode)
,               slime-handle-repl-shortcut
DEL             backward-delete-char-untabify
<C-return>      slime-repl-closing-return
<C-down>        slime-repl-forward-input
<C-up>          slime-repl-backward-input
<return>        slime-repl-return

C-x C-e         slime-eval-last-expression

C-c C-c         slime-interrupt
C-c C-n         slime-repl-next-prompt
C-c C-o         slime-repl-clear-output
C-c C-p         slime-repl-previous-prompt
C-c C-s         slime-complete-form
C-c C-u         slime-repl-kill-input
C-c C-z         other-window
C-c ESC         Prefix Command
C-c I           slime-repl-inspect

M-RET           slime-repl-closing-return
M-n             slime-repl-next-input
M-p             slime-repl-previous-input
M-r             slime-repl-previous-matching-input
M-s             previous-line

C-c C-z         run-lisp
  (that binding is currently shadowed by another mode)

C-M-x           lisp-eval-defun

C-M-q           indent-sexp

C-M-q           prog-indent-sexp
  (that binding is currently shadowed by another mode)

C-c M-e         macrostep-expand
C-c M-i         slime-fuzzy-complete-symbol
C-c M-o         slime-repl-clear-buffer
```

### All other Slime shortcuts

There is more to what we showed! Slime has shortcuts to disassemble the function definition of the symbol at point, learn how to navigate the inspector, toggle functions profiling, learn its indentation or completion strategies, use multiple Lisp connections, learn how to [manipulate presentations](https://slime.common-lisp.dev/doc/html/Presentations.html#Presentations)…

Here are all the default keybindings defined by Slime mode.

To see them, go in a .lisp file, type `C-h m` and go to the Slime section.

```
Commands to compile the current buffer’s source file and visually
highlight any resulting compiler notes and warnings:
C-c C-k	- Compile and load the current buffer’s file.
C-c M-k	- Compile (but not load) the current buffer’s file.
C-c C-c	- Compile the top-level form at point.

Commands for visiting compiler notes:
M-n	- Goto the next form with a compiler note.
M-p	- Goto the previous form with a compiler note.
C-c M-c	- Remove compiler-note annotations in buffer.

Finding definitions:
M-.
- Edit the definition of the function called at point.
M-,
- Pop the definition stack to go back from a definition.

Documentation commands:
C-c C-d C-d	- Describe symbol.
C-c C-d C-a	- Apropos search.
C-c M-d	- Disassemble a function.

Evaluation commands:
C-M-x	- Evaluate top-level from containing point.
C-x C-e	- Evaluate sexp before point.
C-c C-p	- Evaluate sexp before point, pretty-print result.

Full set of commands:
key             binding
---             -------

C-c             Prefix Command
C-x             Prefix Command
ESC             Prefix Command
SPC             slime-space

C-c C-c         slime-compile-defun
C-c C-j         slime-eval-last-expression-in-repl
C-c C-k         slime-compile-and-load-file
C-c C-s         slime-complete-form
C-c C-y         slime-call-defun
C-c ESC         Prefix Command
C-c C-]         slime-close-all-parens-in-sexp
C-c x           slime-export-symbol-at-point
C-c ~           slime-sync-package-and-default-directory

C-M-a           slime-beginning-of-defun
C-M-e           slime-end-of-defun
M-n             slime-next-note
M-p             slime-previous-note

C-M-,           slime-previous-location
C-M-.           slime-next-location

C-c TAB         completion-at-point
C-c RET         slime-expand-1
C-c C-p         slime-pprint-eval-last-expression
C-c C-u         slime-undefine-function
C-c ESC         Prefix Command

C-c C-b         slime-interrupt
C-c C-d         slime-doc-map
C-c C-e         slime-interactive-eval
C-c C-l         slime-load-file
C-c C-r         slime-eval-region
C-c C-t         slime-toggle-fancy-trace
C-c C-v         Prefix Command
C-c C-w         slime-who-map
C-c C-x         Prefix Command
C-c C-z         slime-switch-to-output-buffer
C-c ESC         Prefix Command
C-c :           slime-interactive-eval
C-c <           slime-list-callers
C-c >           slime-list-callees
C-c E           slime-edit-value
C-c I           slime-inspect

C-x C-e         slime-eval-last-expression
C-x 4           Prefix Command
C-x 5           Prefix Command

C-M-x           slime-eval-defun
M-,             slime-pop-find-definition-stack
M-.             slime-edit-definition
M-?             slime-edit-uses
M-_             slime-edit-uses

C-c M-c         slime-remove-notes
C-c M-e         macrostep-expand
C-c M-i         slime-fuzzy-complete-symbol
C-c M-k         slime-compile-file
C-c M-q         slime-reindent-defun

C-c M-m         slime-macroexpand-all

C-c C-v C-d     slime-describe-presentation-at-point
C-c C-v TAB     slime-inspect-presentation-at-point
C-c C-v C-n     slime-next-presentation
C-c C-v C-p     slime-previous-presentation
C-c C-v C-r     slime-copy-presentation-at-point-to-repl
C-c C-v C-w     slime-copy-presentation-at-point-to-kill-ring
C-c C-v ESC     Prefix Command
C-c C-v SPC     slime-mark-presentation
C-c C-v d       slime-describe-presentation-at-point
C-c C-v i       slime-inspect-presentation-at-point
C-c C-v n       slime-next-presentation
C-c C-v p       slime-previous-presentation
C-c C-v r       slime-copy-presentation-at-point-to-repl
C-c C-v w       slime-copy-presentation-at-point-to-kill-ring
C-c C-v C-SPC   slime-mark-presentation

C-c C-w C-a     slime-who-specializes
C-c C-w C-b     slime-who-binds
C-c C-w C-c     slime-who-calls
C-c C-w RET     slime-who-macroexpands
C-c C-w C-r     slime-who-references
C-c C-w C-s     slime-who-sets
C-c C-w C-w     slime-calls-who
C-c C-w a       slime-who-specializes
C-c C-w b       slime-who-binds
C-c C-w c       slime-who-calls
C-c C-w d       slime-who-depends-on
C-c C-w m       slime-who-macroexpands
C-c C-w r       slime-who-references
C-c C-w s       slime-who-sets
C-c C-w w       slime-calls-who

C-c C-d C-a     slime-apropos
C-c C-d C-d     slime-describe-symbol
C-c C-d C-f     slime-describe-function
C-c C-d C-g     common-lisp-hyperspec-glossary-term
C-c C-d C-p     slime-apropos-package
C-c C-d C-z     slime-apropos-all
C-c C-d #       common-lisp-hyperspec-lookup-reader-macro
C-c C-d a       slime-apropos
C-c C-d d       slime-describe-symbol
C-c C-d f       slime-describe-function
C-c C-d g       common-lisp-hyperspec-glossary-term
C-c C-d h       slime-documentation-lookup
C-c C-d p       slime-apropos-package
C-c C-d z       slime-apropos-all
C-c C-d ~       common-lisp-hyperspec-format
C-c C-d C-#     common-lisp-hyperspec-lookup-reader-macro
C-c C-d C-~     common-lisp-hyperspec-format

C-c C-x c       slime-list-connections
C-c C-x n       slime-next-connection
C-c C-x p       slime-prev-connection
C-c C-x t       slime-list-threads

C-c M-d         slime-disassemble-symbol
C-c M-p         slime-repl-set-package

C-x 5 .         slime-edit-definition-other-frame

C-x 4 .         slime-edit-definition-other-window

C-c C-v M-o     slime-clear-presentations
```

[slime-fuzzy]: https://common-lisp.net/project/slime/doc/html/Fuzzy-Completion.html

## See also

- [SLIME's documentation](https://slime.common-lisp.dev/doc/html/)
- **[Slime video tutorial](https://www.youtube.com/watch?v=sBcPNr1CKKw)** (and the author's channel, full of great stuff)
- Marco Baringer's [Slime tutorial](https://www.youtube.com/watch?v=NUpAvqa5hQw)
- [Common Lisp REPL exploration guide](https://bnmcgn.github.io/lisp-guide/lisp-exploration.html), a concise and curated set of highlights to find one's way in the REPL.
- [Emacs4CL](https://github.com/susam/emacs4cl), a tiny DIY kit to set up vanilla Emacs for Common Lisp programming.
- [slime-star](https://github.com/mmontone/slime-star), a collection of extensions for SLIME:
  * doc contribs: richer slime-help and slime-info buffers to display documentation.
  * Quicklisp systems: autocompletion to load Quicklisp systems from the REPL.
  * quicksearch integration: search for Common Lisp repositories on Quicklisp, Github and Cliki.
  * Slime breakpoints: set breakpoints visually without code annotation, get buttons to step through code.
  * Quicklisp apropos: "apropos" across Quicklisp libraries.
  * Slime critic: get the Slime critic gently critique your code.
  * interactive print and trace buffers
  * dedicated Emacs buffers for output streams
  * access to the ANSICL specification in Emacs' Info format.
  * Lisp system browser: a (work in progress) Smalltalk-like system browser for Common Lisp, where one can get different panes to browse available packages and their functions, variables, macros, classes, generic functions.
