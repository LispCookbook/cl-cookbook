---
title: Using Emacs as an IDE
---

The material on this page was originally presented at the [ILC 2003 conference](http://www.international-lisp-conference.org/index.html). A paper with more in-depth coverage of some of the material on this page can be found on [Bill Clementson's ILC2003](http://home.comcast.net/~b.clementson/ilc_2003.htm) page.

This page is meant to provide an introduction to using Emacs as a Lisp IDE. The key bindings used in the example code snippets assume an Emacs configuration similar to that provided by the [.emacs](https://github.com/LispCookbook/cl-cookbook/blob/master/.emacs) file that is included as part of the [Setting up an IDE with Emacs on Windows or Mac OS X](windows.html) page. If you use ILISP, the key bindings reflect the bindings that are present in the current CVS version of ILISP.


<a name="Slide-2"></a>

##Why Use Emacs?

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


<a name="Slide-4"></a>

## Lisp Modes in Emacs

*   There are 4 different alternative major modes to use for CL programming:
    *   Inferior Lisp Mode
    *   [ILISP](http://sourceforge.net/projects/ilisp/)
    *   [ELI](http://www.franz.com/)
    *   [SLIME](http://common-lisp.net/project/slime/)


<a name="Slide-5"></a>

## Inferior Lisp Mode

*   Pros:
    *   Comes with Emacs
    *   Fast start-up, easy setup
    *   Supports many Lisp implementations

*   Cons:
    *   Limited functionality (compared to ILISP and ELI)
    *   No multiprocessing support
    *   Some conflicts with comint mode

*   Setup:
    *   Included with Emacs, so no separate installation required


<a name="slide-6"></a>

## ILISP

*   Pros:
    *   Vastly superior to Inferior Lisp Mode in functionality
    *   Supports many Lisp implementations

*   Cons:
    *   No multiprocessing support
    *   Some conflicts with comint mode

*   Setup:
    *   A basic installation involves downloading the ILISP package from the web building it and configuring it
    *   Customization can be complex, useful to use instructions on [CL Cookbook](windows.html) to get started


<a name="Slide-7"></a>

## ELI: Emacs-Lisp Interface

*   Pros:
    *   Supports multiprocessing (this is a big pro)
    *   Has commands that allow you to work with changed definitions
    *   Standard, consistent set of options for managing output
    *   Support for ACL and support from Franz is very good

*   Cons:
    *   For CMUCL and SBCL, only limited functionality is available
    *   No built-in support for accessing either Franz or CL documentation

*   Setup:
    *   Basic ELI setup is very straight-forward for ACL


<a name="Slide-slime"></a>

## SLIME: Superior Lisp Interaction Mode for Emacs

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
    *   Installing it from MELPA is straightforward. Search package-list-packages for 'slime' and click to install. If MELPA is configured correctly, it will install itself and all dependencies.
    *   Run slime with M-x slime


<a name="Slide-8"></a>

## Lisp Modes in Emacs - Which One to Choose?

*   My Recommendation:
    *   Inferior Lisp Mode for casual CL programming only
    *   ELI if you use ACL
    *   SLIME otherwise


<a name="Slide-9"></a>

## Working with Lisp Code

*   Lisp Editing

*   Evaluating and Compiling Lisp

*   Searching Lisp Code

*   Note: Example code assumes you are using a setup similar to what is defined in the [.emacs file](https://github.com/LispCookbook/cl-cookbook/blob/master/.emacs) from the [CL Cookbook](windows.html) site


<a name="Slide-10"></a>

## Working with Lisp Code - Editing

*   Forward/Backward/Up/Down movement and selection by s-expressions ( [s1.lisp](s1.lisp) )

~~~lisp
;; Put the cursor on the open parens of "(defvar.." and press "C-M-f"
;; and "C-M-b" a few times to see how you move in units of sexps. Put
;; the cursor on the second additon in the "(progn" statement and
;; press "C-M-t" to swap the first addition sexp and the second
;; one. Put the cursor on the open parens of "(+ x" in defun c and
;; press "C-M-@" to highlight the entire sexp. Then press "C-M-u" to
;; expand the selection "upwards" to the enclosing "(let". Pressing
;; "C-M-d" moves to the next enclosed sexp or (if you are at the
;; beginning of a line) to the enclosed sexp on the line:

(defvar a "a variable")

(defun b ()
  "a function"
  (+ 2 2))

(defun c ()
  "another function"
  (let ((x 42))
    (+ x
       (+ 2 2)
       (+ 3 3)
       (+ 4 4))))

(progn
  (+ 1 1)
  (+ 2 2)
  (+ 3 3))
~~~

*   Deleting s-expressions ( [s2.lisp](s2.lisp) )

~~~lisp
;; Put the cursor on the open parens of "(progn .." and press "C-M-k"
;; to delete it. Then press "C-M-backspace" to delete the sexp before
;; the cursor:** 
(defun d ()
  (if t
      (+ 3 3)
      (progn
        (+ 1 1)
        (if t
            (+ 2 2)
            (+ 3 3)))
            (+ 4 4)))
~~~

*   Indenting s-expressions ( [s3.lisp](s3.lisp) )

~~~lisp
;; Put the cursor on the open parens of "(defun ..." and press "C-M-q"
;; to indent the code:
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
~~~

~~~lisp
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
~~~

*   Support for parenthesis ( [s4.lisp](s4.lisp) )

~~~lisp
;; Placing the cursor on a "(" or after a ")" highlights the matching
;; parens:
(progn (+ 3 3) (- 2 2))
~~~

~~~lisp
;; A mismatched parens is highlighted in a different color (put cursor
;; after last parens and enter a ")" to see this:
(- 2 2)
~~~

~~~lisp
;; You can also type "M-x check-parens" to locate mismatched parens in
;; a buffer
~~~

~~~lisp
;; Press "M-(" and you will get:
()

;; with the cursor placed on the closing parens, ready to enter the
;; function name.
~~~

~~~lisp
;; Put the cursor on the open parens of the "(+ 2 2)" sexp below and
;; press "C-u 2 M-(" to enclose the next 2 sexps with parens - then
;; type "+ 1" to add "1" to the result of the following 2 sexps:
(progn (+ 2 2) (+ 3 3))
~~~


~~~lisp
;; To delete the enclosing "progn" below, put the cursor on the open
;; parens of the "(+ 1 1)" and press the following sequence of keys:
;; "C-M-k C-M-k C-M-k C-M-u C-M-k C-y M-y C-M-a C-M-q":
(defun a ()
  (progn
    (+ 1 1)
    (+ 2 2)
    (+ 3 3)))
~~~

*   Automatic code indentation (CL vs Elisp) ( [s5.lisp](s5.lisp) )

~~~lisp
;; Indentation is automatic for Lisp forms. Example: Put the cursor
;; after the first addition form and press Enter:
(progn
  (+ 3 3)
  (- 2 2))
~~~

~~~lisp
;; Pressing TAB will indent incorrectly indented code. Example: Put
;; the cursor at the beginning of the "(+ 3 3)" form and press TAB:
(progn
(+ 3 3))
~~~

~~~lisp
;; CL indentation rules are different from Emacs Lisp indentation
;; rules. Make certain you have the following code in a lisp mode hook
;; in your .emacs file:
(set (make-local-variable lisp-indent-function)
     'common-lisp-indent-function)
~~~

*   Close all parenthesis ( [s6.lisp](s6.lisp) )

~~~lisp
;; Press "C-c ]" (in ELI) or "C-C C-v C-]" (in ILISP) to close all
;; parens. Example: Put cursor at end of following form and press the
;; appropriate key sequence:
(progn (if nil (progn (+ 3 (- 2 1
~~~

*   Code completion ( [s7.lisp](s7.lisp) )

~~~lisp
;; Type the following and press "C-c TAB" (both ELI & ILISP) to get an
;; automatic completion for defvar:
(defv
~~~

~~~lisp
;; Typing in the following and pressing "C-c TAB" results in a list of
;; altermatives:
(def
~~~

~~~lisp
;; The Emacs hippie-expand command will expand both lisp symbols
;; (however, it only works off of lisp symbol information that is
;; either available in the buffers or a standard CL symbol) and
;; directories. For example, type in the following and press "C-c /"
;; to get a directory completion:
(setq x "c:/pro
~~~

*   Hiding/showing code ( [s8.lisp](s8.lisp) )

~~~lisp
;; Highlight the middle "(if ..." block and press "C-x n n" to hide
;; everything but that block ("C-x n w" restores the other code):
(if a
    (+ 1 1))
(if b
    (+ 2 2))
(if c
    (+ 3 3))
~~~

~~~lisp
;; Put the cursor on "(defun b ..." and press "C-x n d" to narrow to
;; just defun b (("C-x n w" restores the other code):
(defun a ()
  (+ 1 1))

(defun b ()
  (+ 2 2))

(defun c ()
  (+ 3 3))
~~~

*   Comments ( [s9.lisp](s9.lisp) )

~~~lisp
;; Put the cursor on the following sexp and press "M-;" to get a
;; code line comment (right-hand comment):
(setq x 1)
~~~

~~~lisp
;; Highlight the 2nd & 3rd lines and press "M-;" to comment out those
;; lines (highlighting them a 2nd time and pressing "M-;" removes the
;; comment):
(+ 1 1)
(+ 2 2)
(+ 3 3)
~~~

~~~lisp
;; Using Paul Foley's comment functions allows you to selectively
;; comment out embedded sexps. Example: Put the cursor on the "(+ 4
;; 4)" sexp and press "C-c ;" to comment out that sexp. Pressing "C-c
;; ;" comments out the enclosing sexp (and on upwards). Pressing "C-c
;; :" removes the comment:
(+ 1 (+ 2 (+ 3 (+ 4 4))))
~~~

~~~lisp
;; Emacs knows how to wrap comment text intelligently. For example, this comment line spans
;; muliple lines but is not aligned consitently
;; with the rest of the comments/code in the file (everything else
;; ends at
;; column 68\. Pressing "M-q" adjusts the comments appropriately.
~~~


<a name="Slide-11"></a>

## Working with Lisp Code - Evaluating and Compiling Lisp

*   buffer
*   region
*   defun
*   sexp (previous/next)
*   DWIM
*   Example code ( [s11.lisp](s11.lisp) )

~~~lisp
;; Compile the entire buffer by pressing "C-c C-b" (ELI) or "C-c C-k
;; C-b" (ILISP).

;; Compile a region by selecting the first 2 forms in test-all and
;; pressing "C-c C-r" (ELI) or "C-c C-k C-r" (ILISP).

;; Compile a defun by putting the cursor inside the "test-format"
;; defun and pressing "C-c C-x" (ELI) or "C-c C-k C-d" (ILISP).

;; Compile the sexp before the point by putting the cursor after the
;; closing paren of "(test-format)" and pressing "C-c C-s" (ELI).

;; As a general rule, to evaluate rather than compile, press "C-u"
;; before the ELI command (e.g. -- "C-u C-c C-s" to evaluate the sexp
;; before the point instead of "C-c C-s" to compile it) or enter the
;; ILISP key sequence with a "C-c C-j" prefix rather than a "C-c C-k"
;; prefix (e.g. -- use "C-c C-j C-d" to evaluate a defun instead of
;; "C-c C-k C-d" to compile the defun)

;; The "Do What I Mean" evaluation/compilation functions work on the
;; following basis: If a region is selected, process the region.  If
;; the cursor is on or immediately after a ')', process the last sexp.
;; If the cursor is on or immediately before a '(', process the next
;; sexp. If the cursor is inside a defun, process the defun. If the
;; cursor is inside a top-level sexp, process the top-level
;; sexp. Tests are done in the order specified, so (if there is any
;; ambiguity), make certain that the cursor is either on a parenthesis
;; (for the last/next commands or not directly before/after/on a
;; parenthesis for the defun/top-level commands.  Press "C-c d" (ELI)
;; or "C-c C-j C-j" (ILISP).** (defun test (n)
(loop for i from 0 below n
   do (print i)))

(defun test-format ()
  (format t "This is a test.~%"))

(defun test-format-loop (n)
  (loop for i from 0 below n
     do (test-format)
       (sleep 1)))

(defun test-all ()
  (test 5)
  (test-format)
  (test-format-loop 5))
~~~


<a name="Slide-12"></a>

## Working with Lisp Code - Searching Lisp Code

* Standard Emacs text search (isearch forward/backward, regexp searches, search/replace) ( [s12.lisp](s12.lisp) )

~~~lisp
;; "C-s" does an incremental search forward (e.g. - as each key is
;; the search string is entered, the source file is searched for the
;; first match. This can make finding specific text much quicker as
;; you only need to type in the unique characters. Repeat searches
;; (using the same search characters) can be done by repeatedly
;; pressing "C-s"

;; "C-r" does an incremental search backward

;; "C-s RET" and "C-r RET" both do conventional string searches
;; (forward and backward respectively)

;; "C-M-s" and "C-M-r" both do regular expression searches (forward
;; and backward respectively)

;; "M-%" does a search/replace while "C-M-%" does a regular
;; expression search/replace
~~~

*   Finding occurances (occur, grep) ( [s13.lisp](s13.lisp) )

~~~lisp
;; Use the Emacs "occur" function to find all occurances of a string
;; (or regexp) in a buffer. Example: Enter "M-x occur" and enter the
;; string "defun" to get a list of all the occurances of the
;; characters "defun" in the current buffer.

(defvar aa "a" "a variable")

(defun b ()
  "a function"
  (+ 2 2))

(defun c ()
  "another function"
  (+ 3 3))

(defmacro d (x)
  "a macro"
  `(list ,x))
~~~

~~~lisp
;; Use the Emacs "grep" function to find all occurances of a string
;; (or regexp) in a multiple files. Example: Enter "M-x grep" and
;; enter the string "defun *.lisp" to get a list of all the function
;; definitions in lisp files in the current directory.
~~~

*   Lisp symbols in current source (imenu) ( [s14.lisp](s14.lisp) )

~~~lisp
;; Open a lisp source file and press the middle mouse button for a
;; pop-up window listing all variables and functions in the lisp
;; source file.
~~~

*   Lisp symbols using Lisp ( [s15.lisp](s15.lisp) )

~~~lisp
;; Use the source location maintained by the CL implementation to
;; locate source definitions.  First load this file and the s13.lisp
;; file.  Then put the cursor on the "aa" variable in the following
;; form and press "C-c ."  (ELI) or "M-." (ILISP) to locate the
;; definition.
(setq x aa)
~~~

*   Lisp symbols in multiple source files (etags) ( [s16.lisp](s16.lisp) )

~~~lisp
;; Enter "M-x shell" to bring up a shell window in the current
;; directory. Then run "etags *.lisp" to create a TAG file containing
;; all symbols in lisp files in the current directory. Then run "etags
;; -a subdir/*.lisp" to add to the TAG file symbols from all lisp
;; files in another directory. Locate the definition of the "aa"
;; variable is the s13.lisp file by putting the cursor on the "aa" in
;; the following form and pressing "M-.".
(setq x aa)
~~~

*   Lisp symbols using [ECB](http://ecb.sourceforge.net/) ( [s17.lisp](s17.lisp) )

~~~lisp
;; Press "M-x ecb-activate" to start up ECB. Load a lisp file by
;; pressing the middle mouse button (or the Enter key) on any file
;; name in the ECB mini-window. Pressing the middle mouse button (or
;; the Enter key) on any definition in the ECB symbol mini-window
;; takes you to that definition. Press "C-F7" to toggle between full
;; screen and ECB views.
~~~


<a name="Slide-13"></a>

## Lisp Documentation in Emacs - Learning About Lisp Symbols

*   Argument lists ( [s18.lisp](s18.lisp) )

~~~lisp
;; For ILISP/ELI, type in "(set" and press SPACE to get the arglist in
;; the minibuffer.  For ILISP, type in "(set" and press "C-c C-q C-a"
;; to get the arglist in the listener buffer.  For ILISP, type in
;; "(set " and press "C-u C-c C-q C-a" to get the arguments for "set"
;; pasted into the current buffer.
~~~

*   Documentation ( [s19.lisp](s19.lisp) )

~~~lisp
;; Enter and evaluate the following definitions, then put the cursor
;; on "(xx)" and press "C-c C-f" (ELI) or "C-c C-q C-o" (ILISP) to get
;; the function documentation.
(defun xx ()
  "A do-nothing function"
  nil)

(setf (documentation 'xx 'function) "A function that does nil.")

(xx)
~~~

*   Describe ( [s20.lisp](s20.lisp) )

~~~lisp
;; Enter and evaluate the following definitions, then put the cursor
;; on "(xx)" and press "C-c C-q C-d" (ILISP) to get a description of
;; the function.
(defun xx ()
  "A do-nothing function"
  nil)

(setf (documentation 'xx 'function) "A function that does nil.")

(xx)
~~~

*   Inspect ( [s21.lisp](s21.lisp) )

~~~lisp
;; Enter and evaluate the following definitions, then put the cursor
;; on "(xx)" and press "C-c C-q C-i" (ILISP) to execute "inspect" on
;; the "xx" function. Entering ":h" gives a list of help commands and
;; ":q" exits "inspect".
(defun xx ()
  "A do-nothing function"
  nil)

(setf (documentation 'xx 'function) "A function that does nil.")

(xx)
~~~

*   Macroexpand ( [s22.lisp](s22.lisp) )

~~~lisp
;; Enter the following function definition, then put the cursor on the
;; open parens of "(defun ..." and press "C-c RET" (ELI) or "C-c C-b
;; k" (ILISP) to get a macroexpand-1\. Then press "C-c (" (ELI) or "C-c
;; C-b C-k" (ILISP) to get a recursive macroexpansion.
(defun test (n)
  (loop for i from 0 below n
     do (print i)))
~~~


<a name="Slide-14"></a>

## Lisp Documentation in Emacs - Lisp Documentation

*   [CL HyperSpec](ftp://ftp.lispworks.com/pub/software_tools/documentation/HyperSpec-7-0.tar.gz)
*   [CLtL2](http://www-2.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/lang/lisp/doc/cltl/cltl_ht.tgz)
*   [ACL Documenation](http://www.franz.com/support/documentation/)
*   Example code ( [s23.lisp](s23.lisp) )

~~~lisp
;; Put the cursor on the "(format" below and press "F1" to get the
;; Hyperspec page for "format". Put the cursor on the "d" in the
;; format string and press "C-u F1" to get the Hyperspec page
;; describing the "Tilde D: Decimal" format character.
(format t "One = ~d" 1)
~~~

~~~lisp
;; Note: Depending on the documentation packages that have been
;; loaded, and the browser that you wish to use, the following keys
;; may be used:
;;            Default    W3
;; Package    Browser  Browser  Format-Dft  Format-W3  Info
;; =========  =======  =======  ==========  =========  ====
;; Hyperspec    F1      S-F1    C-u F1      C-u S-F1
;; CLtL2       M-F1    M-S-F1
;; ACL docs    C-F1    C-S-F1
;; Info docs                                           C-M-F1
~~~


<a name="Slide-15"></a>

## Miscellaneous

*   Lisp Listener ( [s24.lisp](s24.lisp) )

~~~lisp
;; With the cursor on the "let", press "C-c x" (ELI or ILISP) to
;; evaluate a lisp form by copying it into the listener.
(let ((n 20))
  (loop for i from 0 below n
     do (print i)))
~~~

~~~lisp
;; In ILISP, most of the eval & compile functions have an "and go"
;; equivalent that transfers the focus to the listener after
;; execution. Their key bindings are the same as the normal
;; eval/compile functions except that the final key does not have a
;; "Ctrl" prefix. For example, put the cursor at the open paren of the
;; "(let ..." sexp above and press "C-c C-j C-n" to evaluate in the
;; source buffer. Then, press "C-c C-j n" to evaluate "and go" to the
;; listener.**
~~~

*   Project Management
    *   [asdf](http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/cclan/asdf/)
    *   [mk-defsystem](http://sourceforge.net/projects/clocc)
*   Debugging
    *   ILISP has a standarized set of key bindings for debugging across implementations ("C-c C-b C-h" for a list of them).
*   Comparing versions of code ( [s10.lisp](s10.lisp) , [s10a.lisp](s10a.lisp) , [s10b.lisp](s10b.lisp) )

~~~lisp
;; Start the ediff utility by entering "M-x ediff". Enter s10a.lisp as
;; the first file name and s10b.lisp as the second file name. Press
;; the space bar to step through the changes. When finished, press "q"
;; to exit.
~~~


<a name="Slide-16"></a>

## Questions/Answers

1. Controlling evaluation output

    **I get irritated by ELI's switching to an output buffer when I
      evaluate a sexp in a Lisp source buffer.**

    *You can control where ELI output goes to by setting the
fi:pop-up-temp-window-behavior variable. Alternatively, you can use my
copy-eval-dwim-lisp function (bound to "C-c x"). It copies Lisp code
from the source buffer to the listener buffer and evaluates it
there. Both buffers stay visible and focus remains in the source
buffer. The code works for ILISP, ELI and Emacs Lisp.*

2. Viewing HyperSpec from within Emacs

    **I like having access to the HyperSpec when I'm in Emacs, but
why does it have to use an external browser? Why can't I just see the
HyperSpec in Emacs?**

    *If you use the Emacs add-on package W3 (or W3M which provides
similar functionality), you can display HTML pages inside of
Emacs. Once you have W3 and the HyperSpec both installed, use code
similar to the following to access the HyperSpec from the Shift-F1
key:*

    <pre><code class="language-lisp">
    (global-set-key [(shift f1)]
                    '(lambda ()
                      (interactive)
                      (let ((browse-url-browser-function
                             'browse-url-w3)
                            (common-lisp-hyperspec-root
                             "file://c:/home/docs/Hyperspec/")
                            (common-lisp-hyperspec-symbol-table
                             (concat common-lisp-hyperspec-root
                                     "Data/Map_Sym.txt"))
                            (hyperspec-prog
                             "c:/home/site/ilisp/extra/hyperspec"))
                        (load-library hyperspec-prog)
                        (common-lisp-hyperspec
                         (thing-at-point 'symbol)))))
    </code></pre>

    *Note that the "let" in the above code sets the
    browse-url-browser-function to W3 for just the HyperSpec. You can
    either set the variable globally (if you want to always use W3 or
    some other specific browser) or locally (if you want to use a
    specific browser and not the default one).*


3. Standard shell

    **I switch between UNIX® and Windows environments and, although
Emacs makes this switch a lot easier, I find it inconvenient having to
use different Shell environments on different operating systems.**

    *On Windows, the [Cygwin tools](http://www.cygwin.com/) provide a
lot of the same tools that are available under UNIX® as well as a BASH
shell. Alternatively, you might want to consider using eshell, a shell
written in Emacs Lisp that comes as a standard feature in later
releases of Emacs. You can access eshell by pressing "F12".*

4. Using ACL tools with Emacs

    **I would like to use Emacs with Franz's ACL but find
that I use the Franz tools so much that I can't afford to not load their IDE.**

    *It doesn't have to be an either/or decision. On Windows, Franz
allows you to specify (under Options) that Emacs is to be the default
editor in place of their built-in editor. On UNIX®, Emacs also works
very well together with the Franz tools.*

5. Windows-style cut/copy/paste

    **I want to use Emacs on a Windows machine. Unfortunately, I have
the Windows cut/copy/paste key bindings burned into my fingertips and
would find it very difficult to switch back and forth between the
Windows standard for these shortcut keys and the Emacs standard.** ~~~

    *Luckily, you don't have to! Download [cua.el](http://www.emacswiki.org/cgi-bin/wiki.pl?CuaMode) and you can continue to use the Windows
defaults. In fact, you may find that the following commands in your .emacs file will make Emacs more
Windows-like:*

    <pre><code class="language-lisp">
    ;; Windows-like mouse/arrow movement & selection (pc-selection-mode)
    (delete-selection-mode t)
    ;; C-z=Undo, C-c=Copy, C-x=Cut, C-v=Paste (needs cua.el)
    (require 'cua) (CUA-mode t)
    </code></pre>

6. Simplified Emacs setup

    **There was a lot of Emacs Lisp code presented in this paper. Do I
really have to type in all this stuff to get started with Emacs and
Lisp?**

    *No, there is a
[sample .emacs file](https://github.com/LispCookbook/cl-cookbook/blob/master/.emacs)
that can be used to get started. It contains all of the configurations
that have been described in this page and (hopefully) should work with
some minor tweaking. See the
[CL-Cookbook](http://lispcookbook.github.io/cl-cookbook/) page on
"[Setting up an IDE with Emacs on Windows or Mac OS X](windows.html)".*

7. Alternatives to Emacs for CL programming

    **I've tried out Emacs and I just can't get used to it. What other
Lisp-friendly alternative are there?**

    * The [Franz](http://www.franz.com/), [LispWorks](http://www.lispworks.com/), [Corman](http://www.cormanlisp.com/), and [Digitool](http://www.digitool.com/) commercial Lisp
offerings all have Lisp-aware editors.
    * CMUCL has [Hemlock](http://www.cons.org/cmucl/hemlock/index.html), which is also [being adapted for other Lisps](http://www.stud.uni-karlsruhe.de/~unk6/hemlock/).
    * [XEmacs](http://www.xemacs.org/) is an alternative to GNU Emacs that works with many of the same Elisp libraries. Some people prefer it to GNU Emacs.
    * Vim can be used to edit Lisp code. An [article](http://lisp-p.org/15-vim/) by Larry Clapp gives some
    pointers on how to use Vim with Lisp.
    * [Jabberwocky](http://jabberwocky.sourceforge.net/) is a Lisp editor/debugger written in Java.
    * Lastly, for true masochists, notepad on Windows or ed on UNIX® can also be used. ;-)
