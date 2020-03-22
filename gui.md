---
title: GUI toolkits
---

You re-compile your function with a key binding and your GUI program
instantly displays the changes: that's the promise of using Common
Lisp for Graphical User Interfaces.

+ binary, FFI linking

# Introduction

Common Lisp has good and active bindings for the following GUI toolkits:

- [Tk][tk] with [Ltk][ltk]
- [Qt4][qt4] with [Qtools][qtools]
- [IUP][iup-tecgraf] with [lispnik/iup][iup-lisp]
- [Gtk3][gtk] with [cl-cffi-gtk][cl-cffi-gtk]
- [Nuklear][nuklear] with [Bodge-Nuklear][bodge-nuklear]

Those are the ones we'll present in this recipe.

In addition, you might want to have a look to:

- the [CAPI][capi] toolkit, by LispWorks (proprietary), a complete and cross-platform toolkit, very praised by its users
- [Ceramic][ceramic], to ship a cross-platform web app with Electron
- [CocoaInterface](https://github.com/plkrueger/CocoaInterface/), a
Cocoa interface for Clozure Common Lisp. Build Cocoa user interface
windows dynamically using Lisp code and bypass the typical Xcode
processes
* [McCLIM](https://common-lisp.net/project/mcclim/), a toolkit in 100% Common Lisp,
* [Alloy](https://github.com/Shirakumo/alloy), another very new toolkit in 100% Common Lisp, used for example in the [Kandria](https://github.com/shinmera/kandria) game.
* [nodgui](https://notabug.org/cage/nodgui), a fork of Ltk, with syntax sugar and additional widgets
* [eql, eql5, eql5-android](https://gitlab.com/eql), embedded Qt4 and Qt5 Lisp, embedded in ECL, embeddable in Qt. Port of EQL5 to the Android platform
* this [demo using Java Swing from ABCL](https://github.com/defunkydrummer/abcl-jazz)

as well as the other ones listed on [awesome-cl#gui](https://github.com/CodyReichert/awesome-cl#gui) and [Cliki](https://www.cliki.net/GUI).

## Tk (Ltk)

[Tk][tk] (or Tcl/Tk, where Tcl is the programming language) has the
infamous reputation of having an outdated look. This is not (so) true
anymore since its 8.5 version of 2007. It doesn't look native, but it
is probably better than you think!

Tk doesn't have a great choice of widgets, but it has a useful canvas,
and it has a couple of unique features: we can develop a graphical
interface **fully interactively** and we can run the GUI **remotely**
from the core app.

So, Tk isn't fancy, but it is an used and proven GUI toolkit (and
programming language) still used in the industry. It can be a great
choice to quickly create simple GUIs, to leverage its ease of deployment, or
when stability is required.

The Lisp binding is [Ltk][ltk].


- **Written in**: Tcl
- **Portability**: cross-platform (Windows, macOS, Linux).

- **Widgets**: this is not the fort of Tk. It has a **small set** of
  default widgets, and misses important ones, for example a calendar. We
  can find some in extensions (such as in **Nodgui**), but they don't
  feel native, at all.

- **Interactive development**: very much. Tk uses the program Wish… TODO:

- **Graphical builder**: no

- **Other features**:
  - **remote execution**: the connection between Lisp and Tcl/Tk is
    done via a stream. It is thus possible to run the Lisp program on
    one computer, and to display the GUI on another one. The only
    thing required on the client computer is tcl/tk installed and the
    remote.tcl script. See [Ltk-remote](http://www.peter-herth.de/ltk/ltkdoc/node46.html).

- **Bindings documentation**: short but complete. Nodgui too.
- **Bindings stability**: very stable
- **Bindings activity**: low to non-existent.
- **Licence**: TODO
- Example applications:
  - [Fulci](https://notabug.org/cage/fulci/) - a program to organize your movie collections.
  - [cl-torrents]() - searching torrents on popular trackers. CLI, readline and a simple Tk GUI.

[image]

**List of widgets**

```
Button Canvas Check-button Entry Frame Label Labelframe Listbox
Menu Menubutton Message
Paned-window
Radio-button Scale
Scrollbar Spinbox Text
Toplevel Widget Canvas

Ltk-megawidgets:
    progress
    history-entry
    menu-entry
```

Nodgui adds:

```
treelist tooltip searchable-listbox date-picker calendar autocomplete-listbox
password-entry progress-bar-star notify-window
dot-plot bar-chart equalizer-bar
swap-list
```

## Qt4 (Qtools)

Do we need to present Qt and [Qt4][qt4]? Qt is huge and contains
everything and the kitchen sink. Qt not only provides UI widgets, but
numerous other layers (networking, D-BUS…).

Qt is free for open-source software, however you'll want to check the
conditions to ship proprietary ones.

The [Qtools][qtools] bindings target Qt4. The Qt5 Lisp bindings are
yet to be created.

<!-- note: experiments involving gobject-introspection -->

- **Framework written in**: C++
- **Portability**: multiplatform, Android, embedded systems, WASM.

- **Widgets choice**: large.

- **Graphical builder**: yes.

- **Other features**: Web browser, a lot more.

- **Bindings documentation**: lengthy explanations, a few examples. Prior Qt knowledge is required.
- **Bindings stability**: stable
- **Bindings activity**: active
- **Qt Licence**: both commercial and open source licences.
- Example applications:
  - todo, snake,…


## Gtk+3 (cl-cffi-gtk)

[Gtk+3][gtk] is the primary library used to build [GNOME][gnome]
applications. Its (currently most advanced) lisp bindings is
[cl-cffi-gtk][cl-cffi-gtk]. While primarily created for GNU/Linux, Gtk
works fine under macOS and can now also be used on Windows.

Gtk also has a the companion
[Broadway](https://developer.gnome.org/gtk3/stable/gtk-broadway.html)
protocol to display applications in the browser, via HTML5 and web
sockets. McClim too got a prototype using Broadway.


- **Framework written in**: C
- **Portability**: GNU/Linux and macOS, also Windows.

- **Widgets choice**: large.

- **Graphical builder**: yes: Glade.
- **Other features**:

- **Bindings documentation**: very good: http://www.crategus.com/books/cl-gtk/gtk-tutorial.html
- **Bindings stability**: stable
- **Bindings activity**: low activity, active development.
- **Licence**: TODO
- Example applications:
  - todo, snake,…


## IUP (lispnik/IUP)

[IUP][iup-tecgraf] is a cross-platform GUI toolkit actively developed
at the PUC university of Rio de Janeiro, Brazil. It uses **native
controls**: the Windows API for Windows, Gtk3 for GNU/Linux. At the
time of writing, it has a Cocoa port in the works (as well as iOS,
Android and WASW ones). A particularity of IUP is its **small API**.

The Lisp bindings are https://github.com/lispnik/iup/. They are nicely
done in that they are automatically generated from the C sources. They
can follow new IUP versions with a minimal work and the required steps
are documented. All this gives us good guarantee over the bus
factor…

IUP stands as a great solution in between Tk and Gtk or Qt.

- **Framework written in**: C (official API also in Lua and LED)
- **Portability**: Windows and Linux, work started for
  Cocoa, iOS, Android, WASM.

- **Widgets choice**: medium.

- **Graphical builder**: yes: [IupVisualLED](http://webserver2.tecgraf.puc-rio.br/iup/en/iupvisualled.html)

- **Other features**: OpenGL, Web browser (WebKitGTK on GNU/Linux), plotting, Scintilla text editor

- **Bindings documentation**: good examples and good readme, otherwise low.
- **Bindings stability**: alpha (but fully generated and working nicely)
- **Bindings activity**: low
- **Licence**: TODO
- Example applications:
  - todo, snake,…



**List of widgets**

```
Radio, Tabs, FlatTabs, ScrollBox, DetachBox,
Button, FlatButton, DropButton, Calendar, Canvas, Colorbar, ColorBrowser, DatePick, Dial, Gauge, Label, FlatLabel,
FlatSeparator, Link, List, FlatList, ProgressBar, Spin, Text, Toggle, Tree, Val,
listDialog, Alarm, Color, Message, Font, Scintilla, file-dialog…
Cells, Matrix, MatrixEx, MatrixList,
GLCanvas, Plot, MglPlot, OleControl, WebBrowser (WebKit/Gtk+)…
drag-and-drop
```

<!-- editor's note: found missing a list view with columns. -->

![](/assets/iup-demo.png)


## Nuklear (Bodge-Nuklear)

[Nuklear][nuklear] is a small [immediate-mode](https://en.wikipedia.org/wiki/Immediate_mode_GUI) GUI toolkit:

> [Nuklear] is a minimal-state, immediate-mode graphical user interface toolkit written in ANSI C and licensed under public domain. It was designed as a simple embeddable user interface for application and does not have any dependencies, a default render backend or OS window/input handling but instead provides a highly modular, library-based approach, with simple input state for input and draw commands describing primitive shapes as output. So instead of providing a layered library that tries to abstract over a number of platform and render backends, it focuses only on the actual UI.

its Lisp binding is [Bodge-Nuklear][bodge-nuklear], and its higher level companions [bodge-ui](ones) and [bodge-ui-window](https://github.com/borodust/bodge-ui-window).

Nuklear is fully skinnable. It is particularly well suited for games,
or for applications where you want to create new controls.


- **Framework written in**: ANSI C, single-header library.
- **Portability**: where C runs. Nuklear doesn't contain
  platform-specific code. No direct OS or window handling is done in
  Nuklear. Instead *all input state has to be provided by platform
  specific code*.

- **Widgets choice**: small.

- **Graphical builder**: no.

- **Other features**: fully skinnable and customizable.

- **Bindings stability**: stable
- **Bindings activity**: active
- **Licence**: MIT or Public Domain (unlicence).
- Example applications:
  - [Obvius](https://github.com/thicksteadTHpp/Obvius/) - a ressurrected image processing library.

**List of widgets**

Non-exhaustive list:

```
buttons, progressbar, image selector, (collapsable) tree, list, grid, range, slider, color picker,
date-picker
```

![](assets/gui/nuklear.png)

# Getting started

## Tk

Ltk is quick and easy to grasp.

~~~lisp
(ql:quickload :ltk)
(in-package :ltk-user)  ;; for our tests
~~~


**How to create widgets**

All widgets are created with a regular `make-instance`:

~~~lisp
(make-instance 'button)
(make-instance 'treeview)
~~~

This makes Ltk explorable with the default symbol completion.

**How to start the main loop**

As with most bindings, the GUI-related code must be started inside a macro that
handles the main loop, here `with-ltk`:

~~~lisp
(with-ltk ()
  (let ((frame (make-instance 'frame)))
    …))
~~~

**How to display widgets**

After we created some widgets, we must place them on the layout. There
are a few Tk systems for that, but the most recent one and the one we
should start with is the `grid`. `grid` is a function that takes as
arguments the widget, its column, its row, and a few optional
parameters.

As with any Lisp code in a regular environment, the functions'
signatures are indicated by the editor. It makes Ltk explorable.

Here's how to display a button:

~~~lisp
(with-ltk ()
  (let ((button (make-instance 'button :text "hello")))
    (grid button 0 0)))
~~~

That's all there is to it.

Building a self-contained executable is equally without surprises.

### Reacting to events

Many widgets have a `:command` argument that accept a lambda which is
executed when the widget's event is started. In the case of a button,
that will be on a click:

~~~lisp
(make-instance 'button
  :text "Hello"
  :command (lambda ()
             (format t "clicked")))
~~~


### Interactive development

When we start the Tk process in the background with `(start-wish)`, we
can create widgets and place them on the grid interactively.

See [the documentation](http://www.peter-herth.de/ltk/ltkdoc/node8.html).

Once we're done, we can `(exit-wish)`.


### Nodgui

To try the Nodgui demo, do:

~~~lisp
(ql:quickload :nodgui)
(nodgui.demo:demo)
~~~


## Gtk3

The
[documentation](http://www.crategus.com/books/cl-gtk/gtk-tutorial.html)
is exceptionally good, for beginners including.

The library to quickload is `cl-cffi-gtk`. It is made of numerous
ones, that we have to `:use` for our package.

~~~lisp
(ql:quickload :cl-cffi-gtk)

(defpackage :gtk-tutorial
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :common-lisp))

(in-package :gtk-tutorial)
~~~

**How to run the main loop**

As with the other libraries, everything happens inside the main loop
wrapper, here `with-main-loop`.

**How to create a window**

`(make-instance 'gtk-window :type :toplevel :title "hello" ...)`.

**How to create a widget**

All widgets have a corresponding class. We can create them with
`make-instance 'widget-class`, but we preferably use the constructors.

The constructors end with (or contain) "new":

```lisp
(gtk-label-new)
(gtk-button-new-with-label "Label")
```

**How to create a layout**

~~~lisp
(let ((box (make-instance 'gtk-box :orientation :horizontal :spacing 6))) ...)
~~~

then pack a widget onto the box:

~~~lisp
(gtk-box-pack-start box mybutton-1)
~~~

and add the box to the window:

~~~lisp
(gtk-container-add window box)
~~~

and display them all:

~~~lisp
(gtk-widget-show-all window)
~~~

### Reacting to events

Use `g-signal-connect` + the concerned widget + the event name (as a
string) + a lambda, that takes the widget as argument:

~~~lisp
(g-signal-connect window "destroy"
  (lambda (widget)
    (declare (ignore widget))
    (leave-gtk-main)))
~~~

Or again:

~~~lisp
(g-signal-connect button "clicked"
  (lambda (widget)
    (declare (ignore widget))
    (format t "Button was pressed.~%")))
~~~

### Full example

~~~lisp
(defun hello-world ()
  ;; in the docs, this is example-upgraded-hello-world-2.
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Hello Buttons"
                                 :default-width 250
                                 :default-height 75
                                 :border-width 12))
          (box (make-instance 'gtk-box
                              :orientation :horizontal
                              :spacing 6)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (let ((button (gtk-button-new-with-label "Button 1")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (format t "Button 1 was pressed.~%")))
        (gtk-box-pack-start box button))
      (let ((button (gtk-button-new-with-label "Button 2")))
        (g-signal-connect button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (format t "Button 2 was pressed.~%")))
        (gtk-box-pack-start box button))
      (gtk-container-add window box)
      (gtk-widget-show-all window))))
~~~

![](assets/gui/gtk3-hello-buttons.png)


## IUP

Please check the installation instructions upstream. You may need one
system dependency on GNU/Linux, and to modify an environment variable
on Windows.

Finally, do:

~~~lisp
(ql:quickload :iup)
~~~

We are not going to `:use` IUP (it is a bad practice generally after all).

~~~lisp
(defpackage :test-iup
  (:use :cl))
(in-package :test-iup)
~~~

The following snippet creates a dialog frame to display a text label.

~~~lisp
(defun hello ()
  (iup:with-iup ()
    (let* ((label (iup:label :title (format nil "Hello, World!~%IUP ~A~%~A ~A"
                                            (iup:version)
                                            (lisp-implementation-type)
                                            (lisp-implementation-version))))
           (dialog (iup:dialog label :title "Hello, World!")))
      (iup:show dialog)
      (iup:main-loop))))
(hello)
~~~

Important note for SBCL: we currently must trap division-by-zero
errors (see advancement on [this
issue](https://github.com/lispnik/iup/issues/30)). So, run snippets
like so:

~~~lisp
(defun run-gui-function ()
  #-sbcl (gui-function)
  #+sbcl
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (gui-function)))
~~~


**How to run the main loop**

As with all the bindings seen so far, widgets are shown inside a
`with-iup` macro.

**How to create widgets**

The constructor function is the name of the widget: `iup:label`,
`iup:dialog`.

**How to display a widget**

Be sure to "show" it: `(iup:show dialog)`.

You can group widgets on `frame`s, and stack them vertically or
horizontally (`vbox`, `hbox`).

To allow a widget to be expanded on window resize, use `:expand
:yes` (or `:horizontal` and `:vertical`) (see the example below).

Use also the `:alignement` properties.

**How to get and set a widget's attributes**

Use `(iup:attribute widget attribute)` to get the attribute's value,
and use `setf` on it to set it.


### Reacting to events

Most widgets take an `:action` parameter that takes a lambda function
with one parameter (the handle).

~~~lisp
(iup:button :title "Test &1"
            :expand :yes
            :tip "Callback inline at control creation"
            :action (lambda (handle)
                      (iup:message "title" "button1's action callback")
                      iup:+default+))
~~~

Below we create a label and put a button below it. We display a
message dialog when we click on the button.

~~~lisp
(defun click-button ()
  (iup:with-iup ()
    (let* ((label (iup:label :title (format nil "Hello, World!~%IUP ~A~%~A ~A"
                                            (iup:version)
                                            (lisp-implementation-type)
                                            (lisp-implementation-version))))
           (button (iup:button :title "Click me"
                               :expand :yes
                               :tip "yes, click me"
                               :action (lambda (handle)
                                         (declare (ignorable handle))
                                         (iup:message "title" "button clicked")
                                         iup:+default+)))
           (vbox
            (iup:vbox (list label button)
                      :gap "10"
                      :margin "10x10"
                      :alignment :acenter))
           (dialog (iup:dialog vbox :title "Hello, World!")))
      (iup:show dialog)
      (iup:main-loop))))

#+sbcl
(sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (click-button))
~~~

Here's a similar example to make a counter of clicks.
We use a label to hold the count. Its title is an integer.

~~~lisp
(defun counter ()
  (iup:with-iup ()
    (let* ((counter (iup:label :title 0))
           (label (iup:label :title (format nil "The button was clicked ~a time(s)."
                                            (iup:attribute counter :title))))
           (button (iup:button :title "Click me"
                               :expand :yes
                               :tip "yes, click me"
                               :action (lambda (handle)
                                         (declare (ignorable handle))
                                         (setf (iup:attribute counter :title)
                                               (1+ (iup:attribute counter :title 'number)))
                                         (setf (iup:attribute label :title)
                                               (format nil "The button was clicked ~a times."
                                                       (iup:attribute counter :title)))
                                         iup:+default+)))
           (vbox
            (iup:vbox (list label button)
                      :gap "10"
                      :margin "10x10"
                      :alignment :acenter))
           (dialog (iup:dialog vbox :title "Counter")))
      (iup:show dialog)
      (iup:main-loop))))

(defun run-counter ()
  #-sbcl
  (counter)
  #+sbcl
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (counter)))
~~~

### List widget example

Below we create three list widgets with simple and multiple selection, we
set their default value (the pre-selected row) and we place them
horizontally side by side.

~~~lisp
(defun list-test ()
  (iup:with-iup ()
    (let*  ((list-1 (iup:list :tip "List 1"  ;; tooltip
                              ;; multiple selection
                              :multiple :yes
                              :expand :yes))
            (list-2 (iup:list :value 2   ;; default index of the selected row
                              :tip "List 2" :expand :yes))
            (list-3 (iup:list :value 9 :tip "List 3" :expand :yes))
            (frame (iup:frame
                    (iup:hbox
                     (progn
                       ;; display a list of integers.
                       (loop for i from 1 upto 10
                          do (setf (iup:attribute list-1 i)
                                   (format nil "~A" i))
                          do (setf (iup:attribute list-2 i)
                                   (format nil "~A" (+ i 10)))
                          do (setf (iup:attribute list-3 i)
                                   (format nil "~A" (+ i 50))))
                       ;; vbox wants a list of widgets.
                       (list list-1 list-2 list-3)))
                    :title "IUP List"))
            (dialog (iup:dialog frame :menu "menu" :title "List example")))

      (iup:map dialog)
      (iup:show dialog)
      (iup:main-loop))))

(defun run-list-test ()
  #-sbcl (hello)
  #+sbcl
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (list-test)))
~~~


# Conclusion

[tk]: https://www.tcl.tk
[ltk]: http://www.peter-herth.de/ltk/ltkdoc/
[qt4]: https://doc.qt.io/archives/qt-4.8/index.html
[gtk]: https://www.gtk.org/
[qtools]: https://github.com/Shinmera/qtools
[cl-cffi-gtk]: https://github.com/Ferada/cl-cffi-gtk/
[iup-tecgraf]: http://webserver2.tecgraf.puc-rio.br/iup/
[iup-lisp]: https://github.com/lispnik/iup/
[gnome]: https://www.gnome.org/
[nuklear]: https://github.com/Immediate-Mode-UI/Nuklear
[bodge-nuklear]: https://github.com/borodust/bodge-nuklear
[capi]: http://www.lispworks.com/products/capi.html
[ceramic]: http://ceramic.github.io/
