---
title: LispWorks
---

[LispWorks](http://www.lispworks.com/) is a Common Lisp implementation
that comes with its own Integrated Development Environment (IDE) and
its share of unique features, such as the CAPI GUI toolkit.  It is
**proprietary** and provides a **free limited version**.

Here, we will mainly explore its IDE, asking ourselves what it can
offer to a seasoned lisper used to Emacs and Slime. The short answer
is: more graphical tools, such as an easy to use graphical stepper, a
tracer, a code coverage browser or again a class browser. Setting and
using breakpoints was a much more pleasing experience than on Slime.

## LispWorks features

We can see TODO URL

- CAPI GUI toolkit: provides native look-and-feel on Windows, Cocoa, GTK+ and Motif.

- [LispWorks for mobile](http://www.lispworks.com/products/lw4mr.html) runtime, for Android and iOS.

- tree shaker

## Free edition limitations

The download instructions and the limitations are given [on the download page](http://www.lispworks.com/downloads/index.html).

The limitations are the following:

- There is a **heap size limit** which, if exceeded, causes the image to exit. A warning is provided when the limit is approached.

What does it prevent us to do? As an illustration, we can not load this set of libraries together in the same image, it is too much:

~~~lisp
(ql:quickload '("alexandria" "serapeum" "bordeaux-threads" "lparallel" "dexador" "hunchentoot" "quri" "ltk" "cl-ppcre" "mito"))
~~~

You get a warning and if you continue, LispWorks closes.

- There is a **time limit of 5 hours** for each session, after which LispWorks Personal exits, possibly without saving your work or performing cleanups such as removing temporary files. You are warned after 4 hours of use.

- The functions [save-image](http://www.lispworks.com/documentation/lw71/LW/html/lw-95.htm), [deliver](http://www.lispworks.com/documentation/lw71/DV/html/delivery-4.htm#pgfId-852223), and load-all-patches are not available.
  => it is **impossible to build a binary**.

- **Initialization files are not loaded**.
  => if you are used to initializing Quicklisp from your `~/.sbclrc` on Emacs, you'll have to load an init file manually every time you start LispWorks.

For the record, the snippet provided by Quicklisp to put in one's startup file is the following:

~~~lisp
;; provided you installed quicklisp in ~/quicklisp/
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
~~~

You'll have to paste it to the listener window (with the `C-y` key (y means yank)).

- Layered products that are part of LispWorks Professional and Enterprise Editions (CLIM, KnowledgeWorks, Common SQL and LispWorks ORB) are not included. But **we can try the CAPI toolkit**.

The installation process requires you to fill an HTML form to receive
a download link, then to run a first script that makes you accept the
terms and the licence, then to run a second script that installs the software.

## LispWorks IDE

The LispWorks IDE is self-contained, but it is also possible to use LispWorks from Emacs and Slime.

### The editor

The editor experience was okay in our tests, only a bit minimal. It
offers a TAB-completion pop-up, syntax highlighting, Emacs-like
keybindings (including the `M-x` extended command). The menus help
the discovery.

We had an issue though, in that the go-to-source function did not work out for built-in Lisp symbols.

See also:

- the [Editor User Guide](http://www.lispworks.com/documentation/lw71/EDUG-U/html/eduser-u.htm).

### Keybindings

Most of the keybindings are similar to Emacs, but not all. Here are some differences:

- to **compile a function**, use `C-S-c` (control, shift and c), instead of C-c C-c.
- to **compile the current buffer**, use `C-S-b` (instead of C-c C-k).

Similar ones include:

- `C-g` to cancel what you're doing,
- `M-w` and `C-y` to copy and paste,
- `M-b`, `M-f`, `C-a`, `C-e`… to move around words, to go to the beginning or the end of the line,
- `C-k` to kill until the end of the line,
- `M-.` to find the source of a symbol,
- `C-x C-e` to evaluate the current defun,
- …

It is possible to use **classical keybindings**, à la KDE/Gnome. Go to the
Preferences menu, Environment and in the Emulation tab.

There is **no Vim layer**.

### Searching keybindings by name

It is possible to search for a keybinding associated to a function, or
a function name from its keybinding, with the Help -> Editing -> Key
to Command / Command to Key menu entries.

### Tweaking the IDE

It is possible to change keybindings. The editor's state is accessible
from the `editor` package, and the editor is built with the CAPI
framework, so we can use the `capi` interface too. Useful functions
include:

~~~lisp
`
editor:bind-key
editor:defcommand
editor:current-point
editor:with-point  ;; save point location
editor:move-point
editor:*buffer-list*
editor:*in-listener* ;; returns T when we are in the REPL
…
~~~

Here's how you can bind keys:

~~~lisp
;; Indent new lines.
;; By default, the point is not indented after a Return.
(editor:bind-key "Indent New Line" #\Return :mode "Lisp")

;; Insert pairs.
(editor:bind-key "Insert Parentheses For Selection" #\( :mode "Lisp") ;;
(editor:bind-key "Insert Double Quotes For Selection" #\" :mode "Lisp")
~~~

Here's how to define a new command. We make the `)` key
to go past the next closing parenthesis.


~~~lisp
(editor:defcommand "Move Over ()" (p)
  "Move past the next close parenthesis.
Any indentation preceeding the parenthesis is deleted."
  "Move past the next close parenthesis."
  ;; thanks to Thomas Hermann
  ;; https://github.com/ThomasHermann/LispWorks/blob/master/editor.lisp
  (declare (ignore p))
  (let ((point (editor:current-point)))
    (editor:with-point ((m point))
      (cond ((editor::forward-up-list m)
	     (editor:move-point point m)
             (editor::point-before point)
             (loop (editor:with-point ((back point))
                     (editor::back-to-indentation back)
                     (unless (editor:point= back point)
                       (return)))
                   (editor::delete-indentation point))
	     (editor::point-after point))
	    (t (editor:editor-error))))))

(editor:bind-key "Move Over ()" #\) :mode "Lisp")
~~~

And here's how you can change indentation for special forms:

~~~lisp
(editor:setup-indent "if" 1 4 1)
~~~

See also:

- a list of LispWork keybindings: https://www.nicklevine.org/declarative/lectures/additional/key-binds.html

### The listener

The listener is the REPL.

Its interactive debugger is primarily textual but you can also
interact with it with graphical elements. For example, you can use the
Abort button of the menu bar, which brings you back to the top
level. You can invoke the graphical debugger to see the stacktraces
and interact with them. See the Debugger button at the very end of the
toolbar.

![](assets/lispworks/toolbar-debugger.png)

If you see the name of your function in the stacktraces (you will if
you wrote and compiled your code in a file, and not directly wrote it
in the REPL), you can double-click on its name to go back to the
editor and have it highlight the part of your code that triggered the
error.

NB: this is equivalent of pressing `M-v` in Slime.

<div class="info" style="  background-color: #e7f3fe;   border-left: 6px solid #2196F3; padding: 1em;">
<p><strong>NB:</strong>
  this is equivalent of pressing <code>M-v</code> in Slime.
</p>
</div>


### The stepper. Breakpoints.

The [stepper](http://www.lispworks.com/documentation/lw61/IDE-W/html/ide-w-496.htm) is one
of the areas where LispWorks shines.

When your are writing code in the editor window, you can set
breakpoints with the big red "Breakpoint" button (or by calling `M-x Stepper Breakpoint`).
This puts a red mark in your code.

The next time your code is executed, you'll get a comprehensive Stepper pop-up window showing:

- your source code, with an indicator showing what expression is being evaluated
- a lower pane with two tabs:
  - the backtrace, showing the intermediate variables, thus showing their evolution during the execution of the program
  - the listener, in the context of this function call, where you can evaluate expressions
- and the menu bar with the stepper controls: you can step into the next expression, step on the next function call, continue execution until the position of the cursor, continue the execution until the next breakpoint, etc.

![](assets/lispworks/stepper.gif)

## See also

- [LispWorks IDE User Guide](http://www.lispworks.com/documentation/lw71/IDE-U/html/ide-u.htm)
- the [Awesome LispWorks](https://github.com/fourier/awesome-lispworks) list
