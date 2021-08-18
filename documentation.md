---
title: Documentation
---

# Using and Creating Common Lisp Documentation

* NB!: This document is under active development, comments are requested.

With the long history of Common Lisp, documentation in and on CL has undergone evolution, along with the language. This
document provides some key resources on documentation as well as some quick recipes to get started. 

# Quick Start Recipes

## Common Lisp Documentation in the REPL

* Use `(DESCRIBE 'OBJECT)` in the REPL 

You can get interactive documentation information diretlty in the REPL, during an interactive Lisp session, dependent on
both the implementation specifics and what/whether docstrings have been provided by developers. 

Documentation is provided via the standard functions:[DESCRIBE](http://www.lispworks.com/documentation/HyperSpec/Body/f_descri.htm), [INSPECT](http://www.lispworks.com/documentation/HyperSpec/Body/f_inspec.htm) and [DOCUMENTATION](http://www.lispworks.com/documentation/HyperSpec/Body/f_docume.htm).

Typically, DESCRIBE gets the most use and is the function to familiarize yourself with objects, including documentation functions.
An interactive example in SBCL, trimmed for brevity (try it out in your implementation):
`(describe 'describe)`
prints out misc. information, including: 
``` 
COMMON-LISP:DESCRIBE
  [symbol]

DESCRIBE names a compiled function:
Documentation:
    Print a description of OBJECT to STREAM-DESIGNATOR.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;DESCRIBE.LISP
```
From the above output, we can understand that DESCRIBE prints a description of a requested object, and SBCL provides information on where the source file resides.
If so curious, you could download the SBCL source, find the approripate file and review how this is implemented, with the
source file itself written  in Common
Lisp. 

Two important points emerge: Using docstrings for defining information retrieved by DOCUMENTATION is a good
practice, and, with caveats, information on source files may be provided by the CL implementation.

* Try DESCRIBE out yourself with ATOM, DEFUN, NIL, INTEGERP. 


## Common Lisp Documentation in the IDE

An IDE can provide the additional convenience of connecting documentation functions to shortcut keys, and integrating the Hyperspec,
making integrated documentation look-up available. Reviewing the documentation for the IDEs is the recommended approach, with some
notes provided below.

# Documentation Best Practices

TODO: 

## Docstrings
[The Common Lisp Style Guide](https://lisp-lang.org/style-guide/#documentation) appropriately notes that Common Lisp allows you to add docstrings to functions, packages, classes and individual slots, and you should use this. Follow this, along with general style guidelines, including commenting. 

## Literate Programming (needs expansion and summary of var. libs)

The Literate Programming approach of writing code and documentation as an integrated whole has several related projects:

* [CLWEB by Alex Plotnick](https://github.com/plotnick/clweb)
* [ERUDITE by Mariano Montone](https://github.com/mmontone/erudite/)
* [LITERATE-LISP by Xu Jingtao](https://github.com/jingtaozf/literate-lisp)
* [MGL-PAX by Gábor Melis](https://github.com/melisgl/mgl-pax) - Exploratory programming tool and documentation generator for programming "inside-out", interactively: inside out: the documentation lives in the code and the interactive development workflow is unchanged.

## Common Lisp Documentation Building

[Common Lisp Documentation Examples](https://cl-doc-systems.github.io/)- the project goal is helping to choose a CL
documentation builder suited to developer needs.


* API documentation generation

* Manual documentation generation (declt etc)

# Documentation Resources

### Standards

* [Common Lisp the Language](https://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html), 2nd Edition, by Guy L. Steele. An
updated version of the CL X3J13 ANSI standard, with comments and examples.
* [The X3J13 Draft version of the Common Lisp Standard]. The draft version is the only one legally available on the
  Internet. The [linked draft resource](https://gitlab.com/vancan1ty/clstandard_build), compiled by Currell Berry, contains individual documents and means to build them. You can also download a [single file
PDF](https://gitlab.com/vancan1ty/clstandard_build/-/blob/master/cl-ansi-standard-draft-w-sidebar.pdf).

### Documentation Resources

* The Common-Lisp.net [Documentation section](https://www.common-lisp.net/documentation). Provides books and misc. resources. 
* [Quickdocs](https://quickdocs.org/) allows finding Common Lisp libraries shipped by Quicklisp and reviewing documentation. Allows searching.
* [CLiki](https://www.cliki.net/), the Common Lisp wiki. 
