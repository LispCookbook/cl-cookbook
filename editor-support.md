---
title: Editor support
---

The editor of choice is still Emacs, but it is not the only one.

## Emacs

[SLIME](https://github.com/slime/slime/) is the Superior Lisp
Interaction Mode for Emacs. It has support for interacting with a
running Common Lisp process for compilation, debugging, documentation
lookup, and so on. It works with many implementations.

[Portacle](https://shinmera.github.io/portacle/) is a portable and
multiplatform Common Lisp environment. It ships Emacs25, SBCL,
Quicklisp, Slime and Git.

<img src="assets/portacle.png"
     style="width: 800px"/>

### Installing Slime

Slime is in the official GNU Elpa repository of Emacs Lisp packages
(in Emacs24 and forward). Install with:

    M-x package-install RET slime RET

Now you can run Slime with `M-x slime`.

See also:

* http://wikemacs.org/wiki/SLIME - configuration examples and extensions.


### Using Emacs as an IDE

See ["Using Emacs as an IDE"](emacs-ide.html).

### Setting up Emacs on Windows or Mac

See ["Setting up Emacs on Windows or Mac"](windows.html).


## Vim

[Slimv](http://www.vim.org/scripts/script.php) is a full-blown
environment for Common Lisp inside of Vim.

[Vlime](https://github.com/l04m33/vlime) is a Common Lisp dev
environment for Vim (and Neovim), similar to SLIME for Emacs and SLIMV
for Vim.

<img src="assets/slimv.jpg"
     style="width: 800px"/>

## Atom

See [Atom-Slime](https://atom.io/packages/atom-slime). This package
allows you to interactively develop Common Lisp code, helping turn
Atom into a full-featured Lisp IDE.

<img src="assets/atom-slime.png"
     style="width: 800px"/>


## More

For reviews of plugins for more editors, including free versions of
proprietary ones (LispWorks, Allegro), see
[Articulate Common Lisp](http://articulate-lisp.com/ides/summary.html).
