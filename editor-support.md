---
title: Editor support
---

The editor of choice is still Emacs, but it is not the only one.

## Emacs

[SLIME](https://github.com/slime/slime/) is the Superior Lisp
Interaction Mode for Emacs. It has support for interacting with a
running Common Lisp process for compilation, debugging, documentation
lookup, cross-references, and so on. It works with many implementations.

[Portacle](https://shinmera.github.io/portacle/) is a portable and
multi-platform Common Lisp environment. It ships Emacs25, SBCL,
Quicklisp, SLIME and Git.

<img src="assets/portacle.png"
     style="width: 800px"/>

### Installing SLIME

SLIME is in the official GNU ELPA repository of Emacs Lisp packages
(in Emacs24 and forward). Install with:

    M-x package-install RET slime RET

Since SLIME is heavily modular and the defaults only do the bare minimum (not
even the SLIME REPL), you might want to enable more features with

~~~lisp
(slime-setup '(slime-fancy slime-quicklisp slime-asdf))
~~~

For more details, consult the
[documentation](https://common-lisp.net/project/slime/doc/html/) (also available
as an Info page).

Now you can run SLIME with `M-x slime` and/or `M-x slime-connect`.

See also:

* [https://wikemacs.org/wiki/SLIME](https://wikemacs.org/wiki/SLIME) - configuration examples and extensions.


### Using Emacs as an IDE

See ["Using Emacs as an IDE"](emacs-ide.html).

### Setting up Emacs on Windows or Mac

See ["Setting up Emacs on Windows or Mac"](windows.html).


## Vim & Neovim

[Slimv](https://www.vim.org/scripts/script.php?script_id=2531) is a full-blown
environment for Common Lisp inside of Vim.

[Vlime](https://github.com/vlime/vlime) is a Common Lisp dev
environment for Vim (and Neovim), similar to SLIME for Emacs and SLIMV
for Vim.

<img src="assets/slimv.jpg"
     style="width: 800px"/>

[cl-neovim](https://github.com/adolenc/cl-neovim/) makes it possible to write
Neovim plugins in Common Lisp.

[quicklisp.nvim](https://gitlab.com/HiPhish/quicklisp.nvim) is a Neovim
frontend for Quicklisp.

[Slimv_box](https://github.com/justin2004/slimv_box) brings Vim, SBCL
and tmux in a Docker container for a quick installation.


## Eclipse

[Dandelion](https://github.com/Ragnaroek/dandelion) is a plugin for the
Eclipse IDE.

Available for Windows, Mac and Linux, built-in SBCL and CLISP support
and possibility to connect other environments, interactive debugger
with restarts, macro-expansion, parenthesis matching,…

<img src="dandelion.png" style="width: 800px"/>

## Lem

Lem is an editor tailored for Common Lisp development. Once you
install it, you can start developing. Its interface resembles Emacs
and SLIME (same shortcuts). It comes with an ncurses and an Electron
frontend, and other programming modes: Python, Go, Rust, JS, Nim,
Scheme, HTML, CSS, directory mode, a vim layer, and more.

<img src="https://github.com/cxxxr/lem/raw/master/screenshots/terminal.png"
     style="width: 800px"/>


## Atom

See [SLIMA](https://github.com/neil-lindquist/slima). This package
allows you to interactively develop Common Lisp code, turning
Atom into a pretty good Lisp IDE.

<img src="assets/atom-slime.png"
     style="width: 800px"/>

## Sublime Text

[Sublime Text](http://www.sublimetext.com/3) supports running a Lisp
REPL and evaluating code in it.

You need to install the "SublimeREPL" package and then see the options
in Tools/SublimeREPL to choose your CL implementation, and `eval` what
you want.

<img src="assets/editor-sublime.png"
     style="width: 800px"/>

## Notebooks

[cl-jupyter](https://github.com/fredokun/cl-jupyter) is a Common Lisp
kernel for Jupyter notebooks.


[Darkmatter](https://github.com/tamamu/darkmatter) is a notebook-style
Common Lisp environment.


<img src="https://github.com/tamamu/darkmatter/raw/master/screenshots/screenshot.png"
     style="width: 800px"/>


## REPLs

[cl-repl](https://github.com/koji-kojiro/cl-repl) is an ipython-like REPL. It supports symbol completion, magic and shell commands, editing command in a file and a simple debugger.

<img src="assets/cl-repl.png"
     style="width: 500px"/>


## Others

For reviews of plugins for more editors, including free versions of
proprietary ones (LispWorks, Allegro), see
[Articulate Common Lisp](http://articulate-lisp.com/ides/summary.html).
