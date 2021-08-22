---
title: Using and Creating Documentation
---

* NB!: This draft is under active development, comments and additions are requested. More direct *recipes*, *tips* and *suggested quick-starts* must be added to the proposed key areas of interest:

* Interactive REPL CL Documentation use
* IDE CL Documentation use
* Development Practices
* Resources.  

## Accessing Common Lisp Documentation in the REPL

* Use `(DESCRIBE 'OBJECT)` in the REPL 

You can get documentation information directly in the REPL, in an interactive Lisp session, dependent on both the implementation specifics and what/whether docstrings have been provided by developers. Access is via the standard CL functions: [DESCRIBE](http://www.lispworks.com/documentation/HyperSpec/Body/f_descri.htm), [INSPECT](http://www.lispworks.com/documentation/HyperSpec/Body/f_inspec.htm) and [DOCUMENTATION](http://www.lispworks.com/documentation/HyperSpec/Body/f_docume.htm). Typically, `DESCRIBE` and its interactive version `INSPECT` get the most use. 

Evaluating/entering `(describe 'describe)` in SBCL: 
``` 
SBCL:CL-USER>* (describe 'describe)

COMMON-LISP:DESCRIBE
  [symbol]

DESCRIBE names a compiled function:
Documentation:
    Print a description of OBJECT to STREAM-DESIGNATOR.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;DESCRIBE.LISP
```
DESCRIBE prints a *description* of the requested object, based on the docstring, and SBCL informs where the source file resides.
So, using docstrings for setting information retrieved by DESCRIBE is a *good development practice*, and, with caveats, information on source files may be provided by the CL implementation.

If so desired, this minimalist interactive workflow in the REPL, using included documentation and inspection functions, supplemented with editor configuration, such as with [Magic (ed)](https://github.com/sanel/magic-ed), allows you to stay primarily in the REPL, without an IDE, for an _early, classic_ historic experience. 

Several REPL-centric editors with tab-completion and other perks are described in the [Editor Support section](editor-support.html) of the Cookbook.

* Further experiments: Try out `DESCRIBE`, `INSPECT`, `APROPOS` yourself such as with ATOM, DEFUN, NIL, INTEGERP or your
 own objects. See what INSPECT is more helpful with (such as objects). 

### TODO: CLHS and REPL integration

- [ ]  TODO what is the status of HyperSpec display in Gnu Common Lisp? 
- [ ]  TODO CLISP? Allegedly their 'DESCRIBE linked to the HyperSpec, but   latest .deb failed for me due to HTTPS bug "HTTPS not implemented" last time I checked. Anyone has it working?
* What are the solution to acces ql CLHS package in the repl? ql hyperspec will display the matching url, but one has to access it in an external browser. Comments?

* The following demo lets one lookup an atom in the REPL,  with doc display an external browser. There is an [older HTML to text](https://github.com/vikram/lisplibraries/tree/master/site/html-extract-0.1.0) application, not in ql, or one could use the Cookbook [web scraping](web-scraping.html) info to hack something that could display HyperSpec HTML without depending on software external to CL and REPL. Comments? Improvements? 

~~~lisp

;; Requires [Quicklisp](getting-started.html#install-quicklisp)
(ql:quickload :hyperspec)
(ql:quickload :inferior-shell)
(ql:quickload :clhs)
;; TODO: [ ]check if clhs has a usable lookup function or if :hyperspec is required

(defun ch (str)
  "CH-ecks if STR  is in HyperSpec index, bringing match up in a terminal browser with inferior-shell, or NIL." 
  (let* ((root (namestring(clhs:hyperspec-root)))
         (lookup (hyperspec:lookup str))
         ;; Only tested in console for now, with lynx.
         (request (list 'lynx lookup)))
  (if (not (null lookup))
      (progn
        (setf hyperspec::*hyperspec-root* root)
        (inferior-shell:run/i request)))))
~~~

## Common Lisp Documentation in the IDE

An IDE provides the additional conveniences of documentation shortcut keys and integration of the HyperSpec. The downside involves having to learn the IDE, as well as CL. Emacs with SLIME is by far the most popular approach.  Review the [Editor Support](editor-support.html) section for general information on CL editors. This section shall focus primarily on documentation access.

* **Emacs with Slime** is a popular IDE set-up. Start Slime with the key-binding `M-x slime` and make use of the many specific bindings such as `C-c C-v C-i` to *inspect item at point* or `C-c C-d h` to *look up documentation for the symbol at point in the Common Lisp Hyperspec*. More information is available in the appropriate Cookbook section [Emacs as an IDE](emacs-ide.html), as well as at [Common Lisp REPL Exploration Guide](https://bnmcgn.github.io/lisp-guide/lisp-exploration.html).

* [SLIME](https://common-lisp.net/project/slime/doc/html/Documentation.html#Documentation) online documentation commands follow the example of Emacs Lisp. Pull-up of documentation for symbols and functions is available via commands, as well as HyperSpec look-up. Slime is not just used in Emacs, and has been integrated in other editors, including Vim.
* [SLIME-DOC-CONTRIBS](https://github.com/mmontone/slime-doc-contribs) by Mariano Montone provides doc extensions for SLIME: SLIME-HELP, inspired by helpful-mode, is for browsing Common Lisp documentation instead.
SLIME-INFO uses Emacs info-mode for displaying Common Lisp documentation.

- [ ]  TODO  add recipe for S-D-C

* [SLY](https://joaotavora.github.io/sly/), an experimental fork of SLIME, provides `sly-apropos` for searching through known symbols. 
* [Helm-Sly](https://github.com/emacs-helm/helm-sly) includes `helm-sly-apropos` with live fuzzy completion. 

* Vim TODO overview?
* [Slimv](https://github.com/kovisoft/slimv) is a SWANK client for Vim, similar to SLIME for Emacs, providing a socket interface for evaluating, compiling, debugging, profiling lisp code. Includes HyperSpec lookup in the Repl in Vim. 
* [Doom Emacs](https://github.com/hlissner/doom-emacs), an opinionated Emacs configuration framework, provides CL integration via config, as well as Slime. HyperSpec integration required manual config, when reviewed several years ago. TODO: it's an opinionated defaults distro, does it benefit having it here? 
* [LispWorks](http://www.lispworks.com/) provides [extensive documentation](http://www.lispworks.com/documentation/lw60/IDE-U/html/ide-u-2.htm), including for [Argument list information](http://www.lispworks.com/documentation/lw60/IDE-U/html/ide-u-271.htm#pgfId-977043), [the Expression Menu](http://www.lispworks.com/documentation/lw60/IDE-U/html/ide-u-439.htm#marker-853546).
* [LispIDE](https://www.daansystems.com/lispide/) by by PJ Naughter, A CL and Scheme IDE available for Windows, includes convenient Windows Help (CHM) files for the HyperSpec and CLTL, available with the IDE or [via repository](https://github.com/daansystems/lispide/tree/master/doc).

* [AllegroCL](https://franz.com/support/documentation/) has [extensive documentation](https://franz.com/support/documentation/10.0/doc/contents.htm) including usage during editing. 



# Documentation Best Practices

## Docstrings
[The Common Lisp Style Guide](https://lisp-lang.org/style-guide/#documentation) states: "Common Lisp allows you to add docstrings to functions, packages, classes and individual slots, and you should use this".  
* Docstrings can and should be placed at the beginning of function and variable definitions.

~~~lisp
(defun <name> (list of arguments)
  "A sentence describing what NAME is for.
  - UPPERCASE names of the function or macro arguments
  - Consider adding a "See Also:" section mentioning related items
  - Consider including examples, with the results marked with a '=>'
  - Consider using backtick+quote around the names of other items, 
  such as `example', for Emacs highlighting."
  (function body))
~~~

As Lisp and then Common Lisp evolved, best documentation practices changed, and there is no set standard accepted by all. Some of the popular approaches follow.

* **Long and to the point**. One approach is keeping docstrings in one unbroken line of text. Friendly to whatever program displays the string in the future. Not pretty in Emacs or terminal, but perhaps closer to the original historical spirit. [Quil](https://github.com/quil-lang) is one [example](https://github.com/quil-lang/quilc/blob/master/src/analysis/fusion.lisp), with some caveats.  

~~~lisp
(defun fuse-gate-sequence (gate-sequence)
  "Given a list of gates GATE-SEQUENCE containing *only* fuseable gates, perform gate fusion, returning a new list of gates that is purportedly mathematically equivalent."
  (...))
~~~~
* **Terminal friendly & portable.** The community favorite [Alexandria library](https://common-lisp.net/project/alexandria/) of public domain utilities for CL, celebrated for its style. Check out the [code](https://gitlab.common-lisp.net/alexandria/alexandria) and documentation, as well as how the docs are generated from the code. Note that the docstring is of terminal-friendly width, unlike the previous example.  

A short documentation string in Alexandria:
~~~lisp

(defun ensure-list (list)
 "If LIST is a list, it is returned. Otherwise returns the list designated by LIST."
 (if (listp list)
     list
     (list list)))
~~~

A longer docstring with an example:

~~~lisp
(defun ensure-symbol (name &optional (package *package*))
  "Returns a symbol with name designated by NAME, accessible in package
designated by PACKAGE. If symbol is not already accessible in PACKAGE, it is
interned there. Returns a secondary value reflecting the status of the symbol
in the package, which matches the secondary return value of INTERN.

Example:

  (ensure-symbol :cons :cl) => cl:cons, :external
"
  (intern (string name) package))
~~~ 
* **Doc Generation Aware.** [Serapeum](https://github.com/ruricolist/serapeum), a CL utility library that aims to be less conservative than Alexandria, including utilities of non-CL pedigree. The project's docstrings include formating for documentation generation.

~~~lisp
(defun juxt (&rest fns)
  "Clojure's `juxt'.

Return a function of one argument, which, in turn, returns a list
where each element is the result of applying one of FNS to the
argument.

It’s actually quite simple, but easier to demonstrate than to explain.
The classic example is to use `juxt` to implement `partition`:

    (defalias partition* (juxt #'filter #'remove-if))
    (partition* #'evenp '(1 2 3 4 5 6 7 8 9 10))
    => '((2 4 6 8 10) (1 3 5 7 9))

The general idea is that `juxt` takes things apart."
  (lambda (&rest args)
    (declare (dynamic-extent args))
    (loop for fn in fns
          collect (apply fn args))))

~~~ 

* **Emacs-style**. If you edit your Common Lisp in Emacs and expect the majority of your future users to do so also, adapting the Emacs approach is something to ponder. [The Emacs Documentation tips](https://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html#Documentation-Tips) are verbose. 

~~~lisp
TODO insert example of emacs style function documentation in CL. 
~~~~

* The [Google Common Lisp Style
  Guide](https://google.github.io/styleguide/lispguide.xml#Documentation) [Apache License 2.0]:

For Functions, the docstring should describe the function's contract: what the function does, what the arguments mean, what values are returned, what conditions the function can signal.  It should be expressed at the appropriate level of abstraction, explaining the intended meaning rather than, say, just the syntax.  In documentation strings, capitalize the names of Lisp symbols, such as function arguments.  For example, "The value of LENGTH should be an integer."

~~~lisp
(defun small-prime-number-p (n)
  "Return T if N, an integer, is a prime number. Otherwise, return NIL."
  (... etc ...))

(defgeneric table-clear (table)
  (:documentation
    "Like clrhash, empties the TABLE of all
    associations, and returns the table itself."))
~~~

A long docstring may usefully begin with a short, single-sentence summary, followed by the larger body of the docstring.

When the name of a type is used, the symbol may be quoted by surrounding it with a back quote at the beginning and a single quote at the end.  Emacs will highlight the type, and the highlighting serves as a cue to the reader that `M-.` will lead to the symbol's definition.

~~~lisp
(defun bag-tag-expected-itinerary (bag-tag)
  "Return a list of `legacy-pnr-pax-segment' objects representing
  the expected itinerary of the `bag-tag' object, BAG-TAG."
  ...)
~~~
Every method of a generic function should be independently documented when the specialization affects what the method does, beyond what is described in its generic function's docstring. *[End Of Google Style Guide Quotes]*

* Classes, slots and packages support docstrings via the `:documentation` keyword. Note that *types* are documentation, and Common Lisp allows you to declare the type of class slots. An ideal class definition example from the [Style Guide](https://lisp-lang.org/style-guide/#docstrings-everywhere):

~~~lisp
(defclass request ()
  ((url :reader request-url
        :initarg :url
        :type string
        :documentation "Request URL.")
   (method :reader request-method
           :initarg :method
           :initform :get
           :type keyword
           :documentation "Request method, e.g :get, :post.")
   (parameters :reader request-parameters
               :initarg :parameters
               :initform nil
               :type association-list
               :documentation "The request parameters, as an association list."))
  (:documentation "A general HTTP request."))

~~~
## Literate Programming 

The Literate Programming approach of writing code and documentation as an integrated whole has recieved attention from Common Lisp developers.

* [CLWEB by Alex Plotnick](https://github.com/plotnick/clweb). CL inmplementation of the ideas of CWEB, involving automation of documentation generation from source for pretty-printing (tex). runs under SBCL, Allegro Common Lisp, and Clozure Common Lisp.
* [ERUDITE by Mariano Montone](https://github.com/mmontone/erudite/). Enables interactive development without a tangling phase extracting code from documentation. Documentation is written in CL comments. Choose from the default Erudite syntax, or use plain Latex or Sphinx syntax, and potentially others. Supports ultiple outputs like Latex, Sphinx, Markdown, HTML, etc. Automatic indexing and cross-references. A portable command line interface. Compile and use in several CL systems, tested on SBCL, CCL, CLISP, ECL.
* [LITERATE-LISP by Xu Jingtao](https://github.com/jingtaozf/literate-lisp). Extends the Common Lisp reader syntax so a Common Lisp vendor can read org files as CL source files. By using the package (literate-lisp), Emacs org mode and Emacs Lisp library polymode, literate programming can be very easy in one org file containing both documentation and source codes, and this org file can interact well with SLIME.
* [MGL-PAX by Gábor Melis](https://github.com/melisgl/mgl-pax) - Exploratory programming tool and documentation generator for programming "inside-out", interactively: inside out: the documentation lives in the code and the interactive development workflow is unchanged.

## Common Lisp Documentation Building
- [ ] TODO: [Vincent has an overview](https://lisp-journey.gitlab.io/blog/overview-of-documentation-generators/), discuss adapting/ putting it in?

* [Common Lisp Documentation Examples](https://cl-doc-systems.github.io/)- the project's goal is helping to choose a CL documentation builder suited to developer needs.

The following resources are sourced from the [Awesome Common Lisp](https://github.com/CodyReichert/awesome-cl) curated list by Cody Reichert. Some items are identical to the Literate Programming section.

* [sphinxcontrib-cldomain](https://github.com/russell/sphinxcontrib-cldomain) -
  Extending Sphinx to cover Common Lisp. To build documentation with
  the same ease as sphinx would a Python project. [GPL3][2]
* [Codex](https://github.com/CommonDoc/codex) - A beautiful
  documentation system for Common Lisp. [MIT][200].
* [Staple](https://github.com/Shinmera/staple) - a tool to generate documentation pages using an HTML template. Uses the existing README, adds docstrings, crossreferences and links to the CLHS. [zlib][33].
* [cl-bibtex](https://github.com/mkoeppe/cl-bibtex) - A compatible re-implementation of the BibTeX program in Common Lisp, with a BST-to-CL compiler. [GNU LGPL2.1][11].
* [mgl-pax](https://github.com/melisgl/mgl-pax) - Exploratory programming environment and documentation generator. one may accomplish similar effects as with Literate Programming, but documentation is generated from code, not vice versa. Code is first, code must look pretty, documentation is code. [MIT][200].
  - see this [40ants fork](https://github.com/40ants/doc)
* [erudite](https://github.com/mmontone/erudite) - Literate Programming System built with interactive development in mind. [MIT][200].

- [ ]  TODO Add & expand API documentation generation

- [ ]  TODO Add & expand Manual documentation generation (declt etc)

# Documentation Resources

### Standards

* [Common Lisp the Language](https://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html), 2nd Edition, by Guy L. Steele. An
updated version of the CL X3J13 ANSI standard, with comments and examples.
* [The X3J13 Draft version of the Common Lisp Standard](https://gitlab.com/vancan1ty/clstandard_build). The draft version is the only one legally available on the
  Internet. Compiled by Currell Berry, the resource contains individual documents and means to build them. You can also download a [single file
PDF](https://gitlab.com/vancan1ty/clstandard_build/-/blob/master/cl-ansi-standard-draft-w-sidebar.pdf).
* [The Common Lisp HyperSpec](http://www.lispworks.com/documentation/common-lisp.html). -Based on the CL standard, the HyperSpec is available online or for [download](http://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz), encompassing 15MB of disk storage in about 2300 files. It is copyrighted and has [specific terms of use](http://www.lispworks.com/documentation/HyperSpec/Front/Help.htm#Legal). 

* [DuckDuckGo](https://duckduckgo.com/bang_lite.html) the search engine (DDG) has a convenient !bang search command for the HyperSpec. Begin your DDG search with `!clhs`, such as
  `!clhs describe`, and you will be redirected to the relevant LispWorks HyperSpec search results.

### Documentation Resources

* The Common-Lisp.net [Documentation section](https://www.common-lisp.net/documentation). Provides information on related books and misc. resources. 
* [Quickdocs](https://quickdocs.org/) - List & search Common Lisp libraries shipped by Quicklisp, including their documentation.
* [CLiki](https://www.cliki.net/), the Common Lisp Wiki. 

### Sources
Consider/discuss if [ ] /IEEE citation should be included
* Rainer Joswig
* Guy L. Steele
