---
title: Setting up Emacs on Windows or Mac
---

Emacs is the preferred Lisp source code editor for most CL developers.

Read respective sections for your operating system, then read the last
section that is applicable for both.

## For macOS

Use [homebrew](https://brew.sh) to install Emacs, git and SBCL:

```shell
brew tap d12frosted/emacs-plus
brew install git sbcl emacs-plus
```

## For windows

Using [Chocolatey](https://chocolatey.org), install Emacs, git & SBCL:

```powershell
choco install emacs git sbcl
```

## For both macOs & Windows

This section will show you how to install Quicklisp and configure Emacs.

### Quicklisp

To download Quicklisp:

```shell
curl -O https://beta.quicklisp.org/quicklisp.lisp
```
Start SBCL REPL and install Quicklisp:

```shell
$ sbcl --load quicklisp.lisp
```
at the REPL prompt `*` enter:

```lisp
(quicklisp-quickstart:install)
```
To automatically load Quicklisp in future SBCL sessions:

```lisp
(ql:add-to-init-file)
```

Exit SBCL:
```lisp
(quit)
```

### Configure packages for Emacs

Start Emacs and the [first buffer](https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Interaction.html) you see is going to be called `*scratch*`. In it enter the following:

```emacs-lisp
(progn
  (require 'package)

  ;; Add MELPA repository if not already present
  (unless (assoc "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

  ;; Initialize package system
  (package-initialize)

  ;; Refresh package contents if needed
  (unless package-archive-contents
    (package-refresh-contents))

  ;; Install SLIME if not already installed
  (unless (package-installed-p 'slime)
    (package-install 'slime)
    (message "SLIME installed successfully!"))

  ;; Basic SLIME configuration
  (setq inferior-lisp-program "sbcl")  ; Change to your Lisp implementation
  (setq slime-contribs '(slime-fancy))

  (message "SLIME setup complete! Use M-x slime to start."))
```

Move your cursor to the end of the last parenthesis and type `C-x
C-e`. That is press the `Control` key and hold it and press `x`, then
while still holding down `Control` key, press `e`, followed by
`Enter`.

The above key combination will evaluate the above Emacs Lisp
instructions to install [Slime](https://slime.common-lisp.dev), a
Emacs mode for Common Lisp development.

There is plenty more on how to configure and tweak Emacs, to your
liking, but is outside the scope of this document.
