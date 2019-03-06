---
title: Performance Tuning and Tips
---

Most (if not all) Common Lisp implementations translates the source code into
assembly language, so the performance is really good compared with some other
interpreted languages.

However, sometimes we just want the program to be faster. This chapter
introduces some techniques to squeeze the CPU power out.

## Finding Bottlenecks

### Acquiring Execution Time

The macro [`time`][time] might be the most useful tool for profiling. It takes
a form, evaluates it and prints timing information in
[`*trace-output*`][trace-output], as shown below:

~~~lisp
* (defun collect (start end)
    "Collect numbers [start, end] as list."
    (loop for i from start to end
          collect i))

* (time (collect 1 10))

Evaluation took:
  0.000 seconds of real time
  0.000001 seconds of total run time (0.000001 user, 0.000000 system)
  100.00% CPU
  3,800 processor cycles
  0 bytes consed
~~~

By using `TIME` macro it is fairly easy to find out which part of your program
takes too much time.

### Checking Assembly Code

The function [`disassemble`][disassemble] takes a function and returns the
machine code of it. For example:

~~~lisp
* (defun plus (a b)
    (+ a b))
PLUS

* (disassemble 'plus)
; disassembly for PLUS
; Size: 37 bytes. Origin: #x52B8063B
; 3B:       498B5D60         MOV RBX, [R13+96]                ; no-arg-parsing entry point
                                                              ; thread.binding-stack-pointer
; 3F:       48895DF8         MOV [RBP-8], RBX
; 43:       498BD0           MOV RDX, R8
; 46:       488BFE           MOV RDI, RSI
; 49:       FF1425B0001052   CALL QWORD PTR [#x521000B0]      ; GENERIC-+
; 50:       488B75E8         MOV RSI, [RBP-24]
; 54:       4C8B45F0         MOV R8, [RBP-16]
; 58:       488BE5           MOV RSP, RBP
; 5B:       F8               CLC
; 5C:       5D               POP RBP
; 5D:       C3               RET
; 5E:       CC0F             BREAK 15                         ; Invalid argument count trap
~~~

## Using Declare Expression

The [*declare expression*][declare] can be used to provide hints for compilers
to perform various optimization. Please note that these hints are
implementation-dependent. Some implementations such as SBCL support this
feature, and you may refer to their own documents for detailed
information. Here only some basic techniques mentioned in CLHS are introduced.

In general, declare expressions can occur only at the beginning of the bodies
of certain forms, or immediately after a documentation string if the context
allows. Also, the content of a declare expression is restricted to limited
forms. Here we introduce some of them that are related to performance tuning.

Please keep in mind that these optimization skills introduced in this section
are strongly connected to the Lisp implementation selected. Always check their
documents before using `declare`!

### Speed and Safety

Lisp allows you to specify several quality properties for the compiler using
the declaration [`optimize`][optimize]. Each quality may be assigned a value
from 0 to 3, with 0 being "totally unimportant" and 3 being "extremely
important".

The most significant qualities might be `safety` and `speed`. 

By default, Lisp considers code safety to be much more important than
speed. But you may adjust the weight for more aggressive optimization.

~~~lisp
* (defun max-original (a b)
    (max a b))
MAX-ORIGINAL

* (disassemble 'max-original)
; disassembly for MAX-ORIGINAL
; Size: 144 bytes. Origin: #x52D450EF
...

* (defun max-with-speed-3 (a b)
    (declare (optimize (speed 3) (safety 0)))
    (max a b))
MAX-WITH-SPEED-3

* (disassemble 'max-with-speed-3)
; disassembly for MAX-WITH-SPEED-3
; Size: 92 bytes. Origin: #x52D452C3
...
~~~

As you can see, the generated assembly code is much shorter, which means that
unused code are pruned.

### Type Hints

As mentioned in the [*Type System*][sec:type] chapter, Lisp has a very
powerful type system. You may provide type hints so that the compiler may
reduce size of generated code.

~~~lisp
* (defun max-with-type (a b)
    (declare (optimize (speed 3) (safety 0)))
    (declare (type integer a b))
    (max a b))
MAX-WITH-TYPE

* (disassemble 'max-with-type)
; disassembly for MAX-WITH-TYPE
; Size: 42 bytes. Origin: #x52D48A23
~~~

The size of generated assembly code shrunk to about 1/3 of the size. What
about speed?

~~~lisp
* (time (dotimes (i 10000) (max-original 100 200)))
Evaluation took:
  0.000 seconds of real time
  0.000107 seconds of total run time (0.000088 user, 0.000019 system)
  100.00% CPU
  361,088 processor cycles
  0 bytes consed

* (time (dotimes (i 10000) (max-with-type 100 200)))
Evaluation took:
  0.000 seconds of real time
  0.000044 seconds of total run time (0.000036 user, 0.000008 system)
  100.00% CPU
  146,960 processor cycles
  0 bytes consed
~~~

You see, by specifying type hints, our code runs much faster!

But wait...What happens if we declare wrong types? The answer is: it depends.

For example, SBCL treats type declarations in a [special way][sbcl-type]. It
performs different levels of type checking according to the safety level. If
safety level is set to 0, no type checking will be performed. Thus wrong type
specifier might bring a lot of damage.

### More on Type Declaration

If you try to evaluate `declare` form in the top level, you might get the
following error:

~~~lisp
Execution of a form compiled with errors.
Form:
  (DECLARE (SPEED 3))
Compile-time error:
  There is no function named DECLARE.  References to DECLARE in some contexts
(like starts of blocks) are unevaluated expressions, but here the expression is
being evaluated, which invokes undefined behaviour.
   [Condition of type SB-INT:COMPILED-PROGRAM-ERROR]
~~~

This is because type declarations have [scopes][declare-scope]. In the
examples above, we have seen type declarations applied to a function. 

During development it is usually useful to raise importance of safety in order
to find out potential problems as soon as possible. On the contrary, speed
might be more important after deployment. However, it might be too verbose to
specify declaration expression for each single function.

The macro [`declaim`][declaim] provides such possibility. It can be used as a
top level form in a file and the declarations will be made at compile-time.

~~~lisp
* (declaim (optimize (speed 0) (safety 3)))
NIL

* (defun max-original (a b)
    (max a b))
MAX-ORIGINAL

* (disassemble 'max-original)
; disassembly for MAX-ORIGINAL
; Size: 181 bytes. Origin: #x52D47D9C
...

* (declaim (optimize (speed 3) (safety 3)))
NIL

* (defun max-original (a b)
    (max a b))
MAX-ORIGINAL

* (disassemble 'max-original)
; disassembly for MAX-ORIGINAL
; Size: 142 bytes. Origin: #x52D4815D
~~~

You may specify different safety and speed level and perform optimization
strategy globally.

### Code Inline

The declaration [`inline`][inline] replaces function calls with function
body. It will save the cost of function calls but will potentially increase
the code size. The best situation to use `inline` might be those small but
frequently used functions. The following snippet shows how to encourage and
prohibit code inline.

~~~lisp
;; The globally defined function DISPATCH should be open-coded,
;; if the implementation supports inlining, unless a NOTINLINE 
;; declaration overrides this effect.
(declaim (inline dispatch))
(defun dispatch (x) (funcall (get (car x) 'dispatch) x))

;; Here is an example where inlining would be encouraged.
(defun top-level-1 () (dispatch (read-command)))
 
;; Here is an example where inlining would be prohibited.
(defun top-level-2 ()
  (declare (notinline dispatch))
  (dispatch (read-command)))

;; Here is an example where inlining would be prohibited.
(declaim (notinline dispatch))
(defun top-level-3 () (dispatch (read-command)))

;; Here is an example where inlining would be encouraged.
(defun top-level-4 () 
  (declare (inline dispatch))
  (dispatch (read-command)))
~~~

## Optimizing Generic Functions

### Using Static Dispatch

Generic functions provide much convenience and flexibility during
development. However, the flexibility comes with cost: generic methods are
much slower than trivial functions. The performance cost becomes a burden
especially when the flexibility is not needed.

Package [`inlined-generic-function`][inlined-generic-function] provides
functions to convert generic functions to static dispatch, moving dispatch
cost to compile-time. You just need to define generic function as a
`inlined-generic-function`.

~~~lisp
* (defgeneric plus (a b)
    (:generic-function-class inlined-generic-function))
#<INLINED-GENERIC-FUNCTION HELLO::PLUS (2)>

* (defmethod plus ((a fixnum) (b fixnum))
    (+ a b))
#<INLINED-METHOD HELLO::PLUS (FIXNUM FIXNUM) {10056D7513}>

* (defun func-using-plus (a b)
    (plus a b))
FUNC-USING-PLUS

* (defun func-using-plus-inline (a b)
    (declare (inline plus))
    (plus a b))
FUNC-USING-PLUS-INLINE

* (time
   (dotimes (i 100000)
     (func-using-plus 100 200)))
Evaluation took:
  0.018 seconds of real time
  0.017819 seconds of total run time (0.017800 user, 0.000019 system)
  100.00% CPU
  3 lambdas converted
  71,132,440 processor cycles
  6,586,240 bytes consed

* (time
   (dotimes (i 100000)
     (func-using-plus-inline 100 200)))
Evaluation took:
  0.001 seconds of real time
  0.000326 seconds of total run time (0.000326 user, 0.000000 system)
  0.00% CPU
  1,301,040 processor cycles
  0 bytes consed
~~~

The inlining is not enabled by default because once inlined, changes made to
methods will not be reflected.

It can be enabled globally by adding `:inline-generic-function` flag in
[`*features*`][*features*].

~~~lisp
* (push :inline-generic-function *features*)
(:INLINE-GENERIC-FUNCTION :SLYNK :CLOSER-MOP :CL-FAD :BORDEAUX-THREADS
:THREAD-SUPPORT :CL-PPCRE ALEXANDRIA.0.DEV::SEQUENCE-EMPTYP :QUICKLISP
:QUICKLISP-SUPPORT-HTTPS :SB-BSD-SOCKETS-ADDRINFO :ASDF3.3 :ASDF3.2 :ASDF3.1
:ASDF3 :ASDF2 :ASDF :OS-UNIX :NON-BASE-CHARS-EXIST-P :ASDF-UNICODE :ROS.INIT
:X86-64 :64-BIT :64-BIT-REGISTERS :ALIEN-CALLBACKS :ANSI-CL :AVX2
:C-STACK-IS-CONTROL-STACK :CALL-SYMBOL :COMMON-LISP :COMPACT-INSTANCE-HEADER
:COMPARE-AND-SWAP-VOPS :CYCLE-COUNTER :ELF :FP-AND-PC-STANDARD-SAVE ..)
~~~

When this feature is present, all inlinable generic functions are inlined
unless it is declared `notinline`.

[time]: http://www.lispworks.com/documentation/lw51/CLHS/Body/m_time.htm
[trace-output]: http://www.lispworks.com/documentation/lw71/CLHS/Body/v_debug_.htm#STtrace-outputST
[disassemble]: http://www.lispworks.com/documentation/lw61/LW/html/lw-643.htm
[inlined-generic-function]: https://github.com/guicho271828/inlined-generic-function
[sec:type]: #type
[declare]: http://www.lispworks.com/documentation/lw71/CLHS/Body/s_declar.htm
[declare-scope]: http://www.lispworks.com/documentation/lw71/CLHS/Body/03_cd.htm
[optimize]: http://www.lispworks.com/documentation/lw71/CLHS/Body/d_optimi.htm
[sbcl-type]: http://sbcl.org/manual/index.html#Handling-of-Types
[declaim]: http://www.lispworks.com/documentation/lw71/CLHS/Body/m_declai.htm
[inline]: http://www.lispworks.com/documentation/lw51/CLHS/Body/d_inline.htm
[*features*]: http://www.lispworks.com/documentation/lw71/CLHS/Body/v_featur.htm
