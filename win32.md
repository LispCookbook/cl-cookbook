---
title: Using the Win32 API
---


<a name="intro"></a>

## Introduction and Scope

This chapter introduces the basics of the Win32 API, demonstrates how it can be called from Lisp, and shows how Lisp can be used to create a powerful Win32 programming environment optimized for a specific application domain. Any such Lisp environment exposing every option in the Win32 API must be at least as complex as the Win32 API. However, it is hard to imagine a given application domain needing every option of every function in the Win32 API. Extending Lisp's syntax in a domain-specific manner hides those parts of the Win32 API superfluous to the domain. (Most likely the majority of the Win32 API will be hidden.) The programmer then deals with and thinks about only those pieces of the API needed for the task at hand. Pieces of the Win32 API needed later are easily exposed at any time.

A single environment is not easily optimized for both simple and complex applications. The methods outlined in this chapter can be used to construct environments, from simple to complex, appropriate for a range of application domains.

The beginning sections of the chapter introduce concepts and ideas leading to the penultimate section, which concludes with a "Hello" program:

~~~lisp
(require "win-header")
(in-package :wh)
(export '(initialize-appendix-b))

(DefMsgHandler appendix-b-proc MsgMap ()
               (WM_PAINT
                (text-out "Hello from Lisp!" 0 0 :color (RGB 0 0 255))
                0))

(defun initialize-appendix-b ()
  (DefActiveTopWindow "Hello" 'appendix-b-proc
    :style ws_overlappedwindow :width 150 :height 200 :title "Hello Program"))
~~~

Lispworks for Windows version 4.2.7 is used. (The Personal Edition is available for free download.) No attempt is made to accommodate other Lisps and no thought is given to cross-platform compatibility. These ideas were discussed when outlining this document and the current approach decided upon because:

* It is good to publish early and publish often. Reducing the scope fits this philosophy. Other Lisps can be included at a later time. I know that [Corman Lisp](http://www.cormanlisp.com) is one that includes a lot of Win32 API capabilities. I don't have experience with other vendor's Lisps, so look at them all.
* The information presented should apply generally to other vendor's Lisps. The foreign language interface may change but Win32 remains the same.

The Cookbook welcomes contributions broadening coverage in these (and other) areas.

Anyone programming for the Win32 API should be familiar with [Programming Windows, The Definitive Guide to the Win32 API](http://www.charlespetzold.com/pw5/index.html) by Charles Petzold, or a comparable book.

The code presented here has been tested only with Lispworks for Windows 4.2.7 Professional Edition under Windows XP.


<a name="whywin32"></a>

## Why Use Lisp with Win32?

If a program can be cross-platform, it should be. The _Common_ in Common Lisp is a tribute to the cross-platform, and cross-vendor, nature of Common Lisp. The advantages of this portability are not ignored lightly. However, writing programs which take advantage of the Win32 API in a non-portable fashion is not a mandate for doing so without the power of Lisp at one's fingertips.

One concern some may have is the perception that Lisp executables are large. Microsoft operating systems are written in C and C++. The supporting libraries for C/C++ programs come with the operating system. Lisp run-time support looks large only because it does not come with the OS. It is possible to ship Lisp programs in delivery mode, especially with Lispworks for Windows which requires no royalties for this, and it is possible to ship the run-time once and then ship small, compiled programs as they become available.

If the requirement is to create small programs suitable for public download in large volumes, the fact that Lisp runtime support is not loaded on most computers is a limitation. However, Lispworks for Windows allows aggressive pruning during the delivery stage and programs can sometimes be reduced below two megabytes, which is suitable for many download situations. Situations requiring the full Lisp run-time are most likely large applications where it makes sense to deliver the runtime on CD, or via a large one-time download, and later deliver individually-compiled components which are loaded during program or component initialization.

It is my understanding that the lower limit on delivered program size is not inherent to Lisp as a language but only a result of current market demand. As market demand grows for truly small stand-alone executable programs written in Lisp, vendors will have incentive to spend time developing better pruners and shakers. (Lisp starts with a lot and removes what's not needed to produce a deliverable while C/C++ starts with a little and adds what's needed during the link phase. A freshly-run Lisp begins with a REPL, a read-eval-print loop, with the full power of Lisp available interactively and ready for use.)

Seeing the gory details of Lispwork's Foreign Language Interface, or FLI, may seem strange at first. The OS is written in C/C++, so OS calls from C/C++ are not foreign calls, just as Lisp functions called from Lisp functions are not foreign calls.


<a name="apioverview"></a>

## A (Very) Brief Overview of a Win32 Program's Life

The OS treats a Win32 GUI program as a set of subroutines, or functions.

* The first of these functions, WinMain, is called when the program is initialized.
* WinMain calls the Win32 function RegisterClass to inform the OS of the location of a callback function. Several calls may be made to RegisterClass but only one of these calls, naming one callback function, serves the primary window of the application. After RegisterClass is called, the OS knows about the newly-defined class.
* The Win32 function CreateWindowEx is called and given the class name specified on a previous RegisterClass call. Now the window exists and, depending upon the parameters passed to CreateWindowEx, is visible.
* The OS begins queueing messages relating to the newly-created window. The queue is saved for delivery to WinMain.
* WinMain accesses the message queue in a loop referred to as a _message pump_. The message pump loop calls GetMessage, TranslateMessage, and DispatchMessage.
* GetMessage retrieves one message from the queue. There is some method or dependability to the sequence in which messages are queued. A certain sequence of messages is queued when a window is created, for instance. Events, such as mouse movements, cause other messages to be queued.
* TranslateMessage translates virtual-key messages to character messages. DispatchMessage is a call to the OS requesting the OS to handle the message. The OS handles the message by calling another subroutine, or function, in the application program. The application function which is called is the function specified in a call to RegisterClass, which included a class name parameter, where that class name was specified in the call to CreateWindowEx. (Phew).
* The application function specified in the RegisterClass call takes four parameters:
    1. a handle to the window associated with the message
    2. the message id, an integer
    3. a wParam unsigned long
    4. an lParam unsigned long
* The semantics of wParam and lParam vary depending upon the message id.
* The application function contains the equivalent of a case statement, switching on the message id. There are many (hundreds of) message ids. Common ones include wm_create, sent when a window is created, wm_paint, sent when a window's contents are to be drawn, and wm_destroy, sent when a window is about to go away. Other messages are generated in response to events such as key presses, mouse movements, and mouse button clicks. There are windows messages related to displaying video from attached cameras, capturing or playing sound files, dialing telephones, and much more. There are thousands of these messages in the OS but any given program normally deals with a small subset of them. When a message function receives a message with which it does not deal explicitly, the message is passed to a Win32 default function.
*   When a menu command or other event causes the program to enter code which calls the Win32 function PostQuitMessage, the message pump returns zero from GetMessage, which is the cue to exit the message pump loop. WinMain then exits and the program ends.


<a name="unicode"></a>

## Windows Character Systems and Lisp

Some Microsoft operating systems use a single-byte, ASCII, character set and others use a double-byte, Unicode, character set. Use

~~~lisp
(defun external-format ()
  (if (string= (software-type) "Windows NT")
      :unicode
    :ascii)
~~~

to determine which format is in use. Win32 functions taking or returning characters or strings come in two flavors: 1) those ending in A for ASCII characters and 2) those ending in W for wide Unicode characters. This external-format function is useful primarily when calling `fli:with-foreign-string`, part of the Lispworks foreign function interface. When defining Win32 functions in the Foreign Function Interface, or FLI, the presence of the keyword :dbcs indicates that the function has both a single-byte and a double-byte version. When :dbcs is present, Lispworks appends an "A" to the function name in single-byte Windows 95 and a "W" in double-byte Windows NT/2000/XP. (I wrote and tested the example program (see Appendix A) under Windows XP.) Without :dbcs, Lispworks leaves the foreign function name unchanged.

One FLI definition for the Win32 TextOut function is:

~~~lisp
(fli:define-foreign-function
    (TextOut "TextOut" :dbcs :calling-convention :stdcall)
    ((HDC (:unsigned :long)) (nXStart (:unsigned :int)) (nYStart (:unsigned :int))
     (lpString :pointer) (cbString (:unsigned :int)))
  :result-type (:unsigned :long))
~~~

which is equivalent to:

~~~lisp
(fli:define-foreign-function
    (TextOut "TextOutW" :calling-convention :stdcall)
    ((HDC (:unsigned :long)) (nXStart (:unsigned :int)) (nYStart (:unsigned :int))
     (lpString :pointer) (cbString (:unsigned :int)))
  :result-type (:unsigned :long))
~~~


under NT/2000/XP (the second example would use "TextOutA" under 95).

To demonstrate this, let's use a simple FLI definition which is easy to call interactively for testing purposes. We are trying only to see if a given Win32 function is known to the OS. In the following REPL interaction, the return result is important only when the Lisp restart handler is invoked. When the restart handler is not invoked, the Win32 function was found, loaded and called. Trying to call a Win32 function which the FLI cannot find results in an invocation of the restart handler. (The correct define-foreign-function definition for textout can be found in [Appendix A](#appendixa)

~~~lisp
CL-USER 9 > (fli:define-foreign-function
                (TextOut-1 "TextOut" :dbcs :calling-convention :stdcall)
                () :result-type :int)
TEXTOUT-1

CL-USER 10 > (textout-1)
1

CL-USER 11 > (fli:define-foreign-function
                 (TextOut-2 "TextOut" :dbcs :calling-convention :stdcall)
                 () :result-type :int)
TEXTOUT-2

CL-USER 12 > (textout-2)
0
~~~


The `TextOut` function was found both times. This shows that a given Win32 function can be named in more than one FLI definition. This technique is sometimes useful when more than one Lisp datatype can satisfy the requirements for a parameter of the Win32 function.

~~~lisp
CL-USER 13 > (fli:define-foreign-function
                 (TextOut-3 "TextOutW" :dbcs :calling-convention :stdcall)
                 () :result-type :int)

TEXTOUT-3

CL-USER 14 > (textout-3)

Error: Foreign function TEXTOUT-3 trying to call to unresolved
external function "TextOutWW".

1 (abort) Return to level 0.
2 Return to top-level loop.
3 Return from multiprocessing.

Type :b for backtrace, :c  to proceed,  or :? for other
options

CL-USER 15 : 1 > :top

CL-USER 16 > (fli:define-foreign-function
                 (TextOut-4 "TextOutW" :calling-convention :stdcall)
                 () :result-type :int)

TEXTOUT-4

CL-USER 17 > (textout-4)
1

CL-USER 18 > (fli:define-foreign-function
                 (TextOut-5 "TextOutA" :calling-convention :stdcall)
                 () :result-type :int)
TEXTOUT-5

CL-USER 19 > (textout-5)
0

CL-USER 20 >
~~~


I elided a warning Lispworks gives after multiple definitions of a foreign function when the previous definition differs in its use of the :dbcs keyword from the current definition's use. After CL-USER 14, Lispworks complains about "TextOutWW", which shows that using the :dbcs keyword causes Lispworks to append a 'W' to the foreign function's name, although the existence of the foreign function itself is not verified until an actual call is made. TextOut-5 verifies the existence of the Win32 function TextOutA, which is the ASCII version of TextOut.

I have seen strange character sets in titles and other places and have been able to resolve those problems by ensuring I made consistent use of the :dbcs and/or W/A declarations.

Edi Weitz asked if the :dbcs keyword decides at compile time or at run time which function to call, the ...A or the ...W. I wrote the following program:

~~~lisp
(in-package :cl-user)

(fli:define-foreign-function
    (MessageBox "MessageBox" :dbcs :calling-convention :stdcall)
    ((hwnd (:unsigned :long)) (text (:unsigned :long)) (title (:unsigned :long)) (flags (:unsigned :long)))
  :result-type (:unsigned :long))

(defun external-format ()
  (if (string= (software-type) "Windows NT")
      :unicode
      :ascii))

(defun display-format-used ()
  (fli:with-foreign-string
      (unicode-p u-ec u-bc :external-format (external-format)) "Unicode"
      (fli:with-foreign-string
	  (ascii-p a-ec a-bc :external-format (external-format)) "Ascii"
	  (fli:with-foreign-string
              (title-p t-ec t-bc :external-format (external-format)) "External Format"
              (if (eq (external-format) :unicode)
                  (messagebox 0 (fli:pointer-address unicode-p)
                              (fli:pointer-address title-p) 0)
                  (messagebox 0 (fli:pointer-address ascii-p)
                              (fli:pointer-address title-p) 0))))))

(compile 'external-format)
(compile 'display-format-used)

(deliver 'display-format-used "dbcs-run" 5)
(quit)

; We then have dbcs-run.exe.  When run on Windows XP, dbcs-run pops up a messagebox
; displaying "Unicode".  The same dbcs-run.exe file, ftp'd to a Macintosh running OS 9 with
; Virtual PC running Windows 98, pops up a message box displaying "Ascii".
~~~


<a name="fli"></a>

## FLI - The Foreign Language Interface - Translating C Header Files to Lisp

When calling the Win32 API from C/C++, header files provided with the compiler are `#include`d in the program. The header files contain the definitions of constants, structures, and functions comprising the API. These definitions must be available to the Lisp program. I find it most straightforward to do this conversion by hand. Although there are automated methods, doing it manually does not take long on a per-function basis.

In C/C++, the `#define`s exist in the preprocessor. Only those `define`s used by the program are included in the object code. With Lisp, the `defconstants` are all loaded into the Lisp image, whether or not they are subsequently used. I do not know a clean solution for this issue.

In the meantime, I make a base, or core, win-header.lisp and use other .lisp files, grouped by functionality, for less-frequently-used definitions, loading those .lisp files when I need them.<a name="fli-types"></a>


### FLI Data Types

The Win32 C/C++ header files include many typedefs for OS-specific data types, including HINSTANCE, HANDLE, HMENU, LPCTSTR, and more. Regarding Lisp, these essentially boil down to signed or unsigned, long or char, and singleton or array, or C structures composed of those types. (Int seems to be the same as long.)

Lisp does not know or care about the difference between an HINSTANCE and an HMENU. They both are simply 32-bit values. Lisp pays attention to these values at two different points in time: 1) when moving Lisp data to a foreign field and 2) when moving the foreign data to Lisp. Lispworks attempts coercion at those points and conditions result when incorrect attempts are made to do conversions like stuffing a negative value into an unsigned field. If more hints about type are given to Lisp, such as declaring a foreign field to be of type :pointer, Lisp will complain when trying to stuff zero into the pointer. That is not handy if one is trying to pass a null pointer to the OS. Thus, I find it easier to call most parameters long, although I bend that rule from time to time.

Lispworks FLI pointers are actually a Lisp structure containing an address retrieved, or unboxed, by fli:pointer-address. When passing a pointer value to the OS, for example when passing the address of a `RECT` to `GetClientRect`, there are two steps that need to happen: 1) allocate the foreign structure and 2) pass the address of that allocated structure to the OS. Most of the time these allocations are best handled with `fli:with-dynamic-foreign-objects` enclosing calls to `fli:allocate-dynamic-foreign-object` because one doesn't have to worry about deallocations. I pass the address of the allocated structure using `fli:pointer-address` (unboxing the pointer value) and define the field in the foreign function's parameter list as an unsigned long.

The FLI allows things to be defined such that Lispworks will try automatic coercion (unboxing). Try defining the parameter type as :pointer. However, Lispworks complains when trying to pass a NULL pointer, although I did not try creating a FLI pointer with address zero. The approach I chose, calling pointers unsigned longs, is clear to me and works well in both directions (OS->Lisp, Lisp->OS). This may simply be a result of my current lack of complete understanding and there may be a better way.

On occasion is it helpful to define C arrays inside C structures, in particular in `sPAINTSTRUCT`. This works but I don't like my current method of obtaining the address of structure members or array entries. I find myself counting byte offsets by hand and using something like:

~~~lisp
(defun interior-copy (to-struct-ptr byte-offset src-ptr)
  (let ((ptr (fli:make-pointer
              :address (fli:pointer-address to-struct-ptr :type :char))))
    (fli:incf-pointer ptr byte-offset)
    (wcscpy (fli:pointer-address ptr) (fli:pointer-address src-ptr))))
~~~


where wcscpy, the wide-character version of strcpy, is defined through the FLI. I hope there's a better way to do this and that someone quickly teaches me. I haven't worked enough with different OSes and Lispworks to know the best way to choose strcpy vs. wcscpy, other than to use (software-type) to decide which to call. (Or use (external-format), defined in Appendix A.)

Although the data types defined to Lisp are kept a minimum, it is very useful for documentation purposes to mimic the typedef names used in the C/C++ header files. Thus `fli:define-c-typedef` is used to define BOOL, DWORD, HANDLE, HDC, and other similar Win32 data types.

Many OS-specific constants must be made available to the Lisp program:

~~~lisp
(defconstant CW_USEDEFAULT       #x80000000)
(defconstant IDC_ARROW                32512)
(defconstant SW_SHOW                      5)
(defconstant WM_CLOSE            #x00000010)
(defconstant WM_DESTROY          #x00000002)
~~~


These constants are given by name, without values, in the MSDN documentation. The Lisp program needs not only the name but also the value. An easy way to find the necessary values is to grep through the VC98/Include directory. Visual Studio contains a "find in files" function on its toolbar which allows this kind of search. Kenny Tilton says, "What I did was grab any VC++ project that builds (the NeHe OpenGL site is full of VC++ projects (see OpenGL tutorials in sidebar to left of the page at [http:///nehe.gamedev.net](http://nehe.gamedev.net)) which built without a problem for me) and then right-click on the symbol I was curious about. (Of > course first you have to find a reference <g>.) VC++ then offers 'find definition' and will jump right to a header entry for a function or constant or macro or whatever."<a name="fli-structures"></a></g>

### FLI Data Structures

I usually define the structure and a typedef for it:

~~~lisp
; PAINTSTRUCT
(fli:define-c-struct sPAINTSTRUCT
    (HDC hdc)
  (fErase bool)
  (rcPaint-x uint)
  (rcPaint-y uint)
  (rcPaint-width uint)
  (rcPaint-height uint)
  (fRestore bool)
  (fIncUpdate bool)
  (rgbReserved (:c-array wBYTE 32)))
(fli:define-c-typedef PAINTSTRUCT sPAINTSTRUCT)
~~~

and then can do something like:

~~~lisp
(fli:with-dynamic-foreign-objects ()
  (let ((ps-ptr (fli:allocate-dynamic-foreign-object :type 'paintstruct)))
    (format t "~&Pointer value: ~a" (fli:pointer-address ps-ptr))))
~~~

although I'm not clear on why the typedef is valuable. Lisp is not C and in Lisp the typedef does not save me from typing `struct sPAINTSTRUCT`, for example. I think the typedefs are superfluous and I probably will stop using them.<a name="fli-functions"></a>


### FLI Functions

It is very easy to define OS calls in the FLI. I start with the API definition in the OS documentation. If Visual C++ is available, the MSDN documentation is probably loaded on the machine. The documentation is available on the [MSDN website](http://msdn.microsoft.com). I go to the Win32 documentation page for the desired function and do a simple translation:

~~~lisp
; LoadCursor
(fli:define-foreign-function
    (LoadCursor "LoadCursor" :dbcs :calling-convention :stdcall)
    ((hInstance handle) (param ulong))
  :result-type handle)
~~~


All the Win32 calls I've seen so far are `:calling-convention :stdcall`. If I know the function includes a text parameter, I include the :dbcs keyword. If I don't know, I try it without :dbcs. The actual function called in this example is `LoadCursorA` or `LoadCursorW`.


<a name="callbacks"></a>

## Callbacks from Windows to Lisp

Once the message pump is up and going, the OS delivers the messages by calling a Lisp function repeatedly. Lisp functions callable from the foreign environment can be defined in the following manner:

~~~lisp
; WndProc -- Window procedure for the window we will create
(fli:define-foreign-callable
    (wndproc :result-type :long :calling-convention :stdcall)
    ((hwnd hwnd) (msg ulong)
     (wparam ulong) (lparam ulong))
  (case msg
    (#.WM_PAINT (wndproc-paint hwnd msg wparam lparam))
    #+console (#.WM_DESTROY (PostQuitMessage 0) 0)
    (t (DefWindowProc hwnd msg wparam lparam))))
~~~


This wndproc function is the message dispatcher. The OS calls wndproc once for every message sent to the program. Wndproc is responsible for understanding the message and calling the appropriate function.

The #. reader macro returns the value of `WM_PAINT` and `WM_DESTROY` at compile-time, allowing `case` to work. #+console means "include the next form only if :console is a member of *features*".

The example Win32 Lisp program in Appendix A may be run either from the Lispworks IDE or from console mode, such as ILISP in Emacs. If `PostQuitMessage` is called from the IDE, the IDE shuts down. If `PostQuitMessage` is not called in console mode, the Win32 window does not close.


<a name="startup"></a>

## Starting the Program

Multiprocessing is always running under the Lispworks IDE but may or may not be running using ILISP under Emacs. Using multiprocessing is great because one can peek and poke at the program and its variables, provide new or redefined functions which take effect immediately, and even make Win32 API calls, all while the program is running and the window is visible with all its buttons and menus active.

Using multiprocessing has not proven so nice for me under ILISP. I love the ILISP and Emacs environment. The Lispworks IDE is very nice, and I keep a copy of it going for certain tasks such as finding online manuals and using debug tools such as the inspector. For editing and most running, though, I prefer Emacs and ILISP. However, I have not learned how to view multiple processes under ILISP, nor do I know how to switch between them. When I use multiprocessing with ILISP, it appears to me that any thread with a condition grabs *standard-output* and *standard-input*. I don't know how to switch back to the other thread. This is enough of a problem that I don't use multiprocessing under ILISP and when I need or want the interactive debug capabilities possible with multiprocessing, or need multiprocessing in any form, I switch to the Lispworks IDE.

When running under the IDE, Lispworks provides the message pump. When running under Emacs/ILISP (or in console mode, as would happen in a delivered application), the Lisp program itself must provide the message pump.

Thus in the example program in Appendix A, the function `create-toplevel-window` ensures multiprocessing is running when in console mode. The function `create-toplevel-window-run` performs the message pump operation in console mode but not otherwise.

The program in Appendix A makes a call to register-class when the file is loaded. The call needs to be made only once and so I make the call at the top-level:

~~~lisp
(defvar *reg-class-atom* (register-class))
~~~


`create-toplevel-window-run` then only needs to call `CreateWindowEx` and optionally start the message pump.


<a name="repl"></a>

## The Lisp REPL and Win32 Development

When a Win32 application is running from within the Lispworks IDE, one is able to enter Lisp forms at the IDE's REPL prompt. One can view any variable, redefine any function, and make calls to `SendMessage` or any other Win32 function that doesn't require context from the OS, such as being within a WM_PAINT. If one redefines the function called when a button is clicked, the next click of the button gets the new function. The Lispworks debug tools are available. Other Lisp programs can be run simultaneously. Individual functions within the running Win32 program can be called from the REPL. Functions can be traced and untraced, advice can be added or removed, and CLOS classes can be redefined on the fly with Lisp guaranteeing that the slot additions or deletions happen in an orderly fashion within the running program.

Lisp is designed to allow programs to run for years at a time, with careful management, and to allow the programs to be maintained, with bugs fixed, new functions defined, and CLOS objects redefined, during that time.


<a name="capi"></a>

## Making Direct Win32 Calls from CAPI

Lispworks includes CAPI, a cross-platform API for GUI program development. CAPI is powerful and easy to use. For true cross-platform capability, it is important to stay with 100%-pure CAPI.

However, even in a pure Win32 environment it is reasonable to want to use CAPI's features quickly to generate advanced GUI programs without having to recreate every wheel. It is possible to use Win32-specific features from within a CAPI program.

~~~lisp
(defclass image-pane (output-pane) ()
  (:default-initargs
   :display-callback 'draw-image))

...

(let ((pane-1 (make-instance 'image-pane))
      ...
      (contain (make-instance 'column-layout
                              :description (list pane-1 other-pane))
        :best-width 640
        :best-height 480)))

...

(defun draw-image (pane x y width height)
  (let ((hwnd (capi:simple-pane-handle pane)))
    ;; This returns the actual Win32 window handle
    ;; Now call CreateWindowEx with hwnd as the new window's parent
    ;; The Win32-defined window then covers the
    ;; CAPI window. After destroying or hiding the Win32 window, the
    ;; CAPI window is revealed.
    ;; Be careful not to create the window if it already is created.

    ....
~~~

Note the connections from image-pane to pane-1 and from image-pane to draw-image, and that pane-1 is contained in the CAPI window. Draw-image gets called when it is time to ... well, when it is time to draw the image!

Certainly calls to Win32 functions which don't require handles to windows or other interaction with the CAPI environment work just fine.

Many good programs use primarily local variables. If one wishes to use the multiprocessing environment to operate upon the program from the Lispworks IDE while the Windows program is running, it is important to have access to symbols and variables for the window handles and other Windows resources. If `SendMessage` is to be called from the REPL, a valid hwnd must be available. A way to have the hwnd available is to do a setf from within a function to which the OS passes the hwnd. This may be used as a debug-only technique or left as a permanent part of the program.


<a name="cinterface"></a>

## Interfacing to C

The OS makes many of the Win32 functions available always. Other functions, for example the `avicap32` video functions, exist in DLLs which must be explicitly loaded. Third-party or custom-built DLLs also require explicit loading.

Make a def file, such as avicap32.def, named after the desired DLL:

~~~
exports capCreateCaptureWindow=capCreateCaptureWindowW
exports capGetDriverDescription=capGetDriverDescriptionW
~~~


and in Lisp

~~~lisp
;; capCreateCaptureWindow
(fli:define-foreign-function
    (capCreateCaptureWindow "capCreateCaptureWindowW")
    ((lpszWindowName :pointer) (dwStyle fli-dword) (x :int) (y :int)
     (nWidth :int) (nHeight :int) (HWND fli-hwnd) (nID :int))
  :result-type (:unsigned :long)
  :module :avicap32
  :documentation "Opens a video capture window.")

(fli:register-module "avicap32")
~~~


Windows created using functions in DLLs can be given a CAPI window as a parent, as previously shown. The def file may or may not be required, depending upon what functions are desired. Maybe :dbcs should be used here, eliminating the need for the hard-coded 'W' in the foreign function name.


<a name="raiiandgc"></a>

## RAII and GC

A common C++ idiom is "Resource Acquisition Is Initialization", in which a C++ object acquires an operating system resource, perhaps an open file, in the constructor and releases the resource in the object's destructor. These objects may have dynamic or indefinite extent.

Objects with dynamic extent are declared local at the beginning of a C++ function and the object's destructor is called when the function returns and the object goes out of scope. The corresponding Lisp idiom is the use of a `with-...` macro. The macro is responsible for acquiring the resource and releasing it under an unwind-protect.

In C++, objects with indefinite extent must have their destructor called explicitly, with `delete` or `delete []`. The destructor tears down the object, first releasing any acquired resources via explicitly-programmed C++ code, then releasing the object's memory via compiler-generated code as the destructor exits.

Lisp is garbage collected, which means that Lisp is responsible for freeing the object's memory. However, that may not happen for a very long time after the last reference to the object has disappeared. The garbage collector runs only as memory fills or when it is explicitly called. If an object holds an acquired resource, almost always there is a proper time to release the resource and not releasing it at that time leads to resource exhaustion.

Lisp is not responsible for acquired resources, such as window handles, which the programmer acquired with explicit Lisp code. The programmer must define a function, something like `(defun release-resources...`, and call the release function at the point where the destructor would have been called in C++. After the release function returns and there are no references to the object, Lisp will free the object's memory during a future garbage collection.

Another issue with the Win32/Lisp environment concerns the GC, which is free to move Lisp data. One cannot give the OS the address of Lisp data which may be moved by the GC. Any data given to the OS should be allocated through the FLI, which is responsible for making the data immovable.


<a name="com"></a>

## COM

COM is widely used in Windows programming. Lispworks for Windows includes a _COM/Automation User Guide and Reference Manual_ and the associated functions. I have not played with COM under Lisp and only note the availability of the manuals and functions. Actually, I did require com and automation and called the com:midl form, which loaded a huge series of IDL files, amazing in breadth and extent. I didn't actually make things happen with COM, though.


<a name="power"></a>

## Beginning to Use the Power of Lisp

My first thought, when I finally completed my demo program, was "That looks like any other Win32 program." There was nothing Lispy about it. It was just Win32 code, programmable in any language. Different languages are good for different problem sets. When I think of Perl, I think of text. I think of numbers along with Fortran. When I think of Lisp, I think of defining my own language. Macros are one of the tools used to define embedded languages within Lisp and are part of what makes Lisp the programmable programming language.

Win32 API programming cries out for new languages. It is a very powerful and flexible API but in a given application context, only certain subsets are used and they are used in repetitive fashions. This does not mean that the APIs should be redefined, were that possible. What works for one application may not work for the next. There probably are some language extensions that will be used in nearly all Win32 programs. Other extensions will apply only to certain applications.

One beauty of Lisp is that the programmer can define a new extension at any time. See [the Common Lisp Cookbook's chapter on macros](macros.html). I also recommend Paul Graham's [On Lisp](http://paulgraham.com/books.html) for learning to write macros and a whole lot more.

When writing code, notice when the same pattern is typed over and over. Then think, sooner rather than later, "it's time for a macro or a function." Notice the repetitive coding even when you're writing macros. Macros can be built upon macros, and macros can generate macros.

Knowing whether to choose a macro or a function is partly a function of code bloat. Macros are evaluated in place and cause new code to be created, where functions do not. The advantage of macros is that they can create closures, capturing variable values present in the environment when the macro is expanded, thus eliminating much of the need for simple objects. If the macro creates `defvar` or `defparameter` forms, other functions the macro creates can use those vars and parameters, although closures can also be used for that purpose. The use of defvars allows reference to those defvars at the REPL in a multiprocessing environment while the Win32 program has an open window and is executing. Whether defvars or closures are used, absolutely zero store-and-retrieve infrastructure is needed.

If macros are not used in this way, then functions that would have been created by the macro must have some form of object look-up code to retrieve window handles, captions, and other resources specific to the object in question.

Be aware that functions can be declaimed inline and can be passed as parameters, while macros cannot.

Many of the macros defined in [On Lisp](http://www.paulgraham.com/books.html) are very useful in Win32 programming. I use the symbol creation macros extensively.

One set of needed macros make using the Foreign Language Interface easier, more compact, and more readable. I have a macro `with-foreign-strings` which takes a list of pointer-name/string pairs and creates a nested series of `(fli:with-foreign-string...` calls, including creation of the element-count and byte-count parameters with unique, predictable names for each string. My `setf-foreign-slot-values`, and `with-foreign-slot-values` macros also make for more compact and readable code.

Another set of macros is useful for defining the Windows message handler functions. I prefer to create a CLOS class for windows messages and let the CLOS method dispatcher find the proper function for each message. This allows message handlers to be inherited and more-specific handlers defined for selected messages in the derived class. CLOS handles this nicely. I have a `DefMsgHandler` macro that calls RegisterClass, defines the CLOS method for each message to be handled, takes care of necessary housekeeping in ubiquitous functions such as `WM_PAINT`, and allows easy definition of function bodies to be executed for each desired type of message. Other macros are useful for defining pushbuttons, edit boxes, list views, and other Win32 controls.

Look at [Appendix B](#appendixb). Compare it to [Appendix A](#appendixa). Of course, an extended version of the header-file portion of Appendix A is used in Appendix B but not shown there. All but a little of the program in Appendix A has been reduced to a library which is invoked in Appendix B. All of the application-specific information is contained in the very small program in Appendix B.

The program in [Appendix C](#appendixc) uses these macros to define a window including radio buttons, pushbuttons, a check box, text drawn on the background window, and a listview with columns.


<a name="conclusion"></a>

### Conclusion

The example code presented in the text and in appendices A-C places an emphasis on staying in Lisp and accessing the Win32 API from there. Paul Tarvydas has code in [Appendix D](#appendixd) which demonstrates cooperation and interaction between C and Lisp. In Paul's well-documented example, a C dll is used to drive the message loop. The Lisp callback function's address is placed from within Lisp into a variable in the dll.

Lisp provides an interactive and rich programming environment. The judicious use of macros to create language extensions in Lisp concentrates application-specific information into small local areas of the overall system. This simplifies the effort of understanding the application, increases the reliability of the application, reduces maintenance time, and increases the reliability of maintenance changes. Lisp is designed to make this easy.

* * *

<a name="appendixa"></a>

## Appendix A: "Hello, Lisp" Program #1

Here is a Win32 Lisp program that opens a GUI window displaying "Hello, Lisp!". A detailed discussion of program specifics follows the listing. The program listing contains necessary lines from the header files which are `#included` in a C/C++ program.

It may be advantageous to open a separate window with the program source code visible while reading the text.

~~~lisp
{% include code/w32-appendix-a.lisp %}
~~~

![](AppendixA.jpg)

* * *

<a name="appendixb"></a>

## Appendix B: "Hello, Lisp!" Program #2

~~~lisp
{% include code/w32-appendix-b.lisp %}
~~~

![](AppendixB.jpg)

* * *

<a name="appendixc"></a>

### Appendix C: Program #3

~~~lisp
{% include code/w32-appendix-c.lisp %}
~~~

![](AppendixC.jpg)

* * *

<a name="appendixd"></a>

### Appendix D: Paul Tarvydas's Example

Here's an example that creates a windows class (in C) and gets invoked and
handled from LWW.  It is similar to the "Hello" example in Petzhold, except
that it hooks to the LWW mainloop instead of creating its own.  Probably it
ain't as pretty as it might be, due to my rustiness with Win32 (and my lack
of patience with it :-).

To use:

1) create a DevStudio Win32 DLL project called "wintest"
2) put the wintest.c file into the project
3) copy the run.lisp file into the project directory (so that the Debug
directory is a subdirectory)
4) Build the C project.
5) Set Project>>Settings>>Debug>>Executable for debug session to point
to the
lispworksxxx.exe.
6) Run the project - this should bring up lispworks.
7) Open run.lisp, compile and load it.
8) In the listener, type "(run)".
9) You should then see a window with "hello" in the middle of it.

The example window class is built and initialized in C (called from the lisp
mainline).  The windows callbacks to this window are handled in lisp (eg. the
WM_PAINT message) - windows has been given a pointer to a lisp function
(Lisp_WndProc) and has been told to use it for callbacks.  The lisp code
makes direct Win32 calls that display the "hello" text.  Lisp uses FLI
foreign functions and foreign variables to set this up.  [If one were doing
this on a real project, a less contrived flow of control would be chosen, but
this one appears to exercise the FLI calls that you were asking about].

[I welcome comments from anyone, re. style, simplification, etc.]

Paul Tarvydas
tarvydas at spamoff-attcanada dotca

~~~c
{% include code/w32-appendix-d.c %}
~~~

~~~lisp
{% include code/w32-appendix-d.lisp %}
~~~
