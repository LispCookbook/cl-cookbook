---
title: Performance Tuning and Tips
---

Many Common Lisp implementations translate the source code into assembly
language, so the performance is really good compared with some other
interpreted languages.

However, sometimes we just want the program to be faster. This chapter
introduces some techniques to squeeze the CPU power out.

## Finding Bottlenecks

### Acquiring Execution Time

The macro [`time`][time] is very useful for finding out bottlenecks. It takes
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

By using `time` macro it is fairly easy to find out which part of your program
takes too much time.

Please note that the timing information provided here is not guaranteed to be
reliable enough for marketing comparisons. It should only be used for tuning
purpose, as demonstrated in this chapter.

### Checking Assembly Code

The function [`disassemble`][disassemble] takes a function and prints the
compiled code of it to `*standard-output*`. For example:

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

The code above was evaluated in SBCL. In some other implementations such as
CLISP, `disassembly` might return something different:

~~~lisp
* (defun plus (a b)
    (+ a b))
PLUS

* (disassemble 'plus)
Disassembly of function PLUS
2 required arguments
0 optional arguments
No rest parameter
No keyword parameters
4 byte-code instructions:
0     (LOAD&PUSH 2)
1     (LOAD&PUSH 2)
2     (CALLSR 2 55)                       ; +
5     (SKIP&RET 3)
NIL
~~~

It is because SBCL compiles the Lisp code into machine code, while CLISP does
not.

## Using Declare Expression

The [*declare expression*][declare] can be used to provide hints for compilers
to perform various optimization. Please note that these hints are
implementation-dependent. Some implementations such as SBCL support this
feature, and you may refer to their own documentation for detailed
information. Here only some basic techniques mentioned in CLHS are introduced.

In general, declare expressions can occur only at the beginning of the bodies
of certain forms, or immediately after a documentation string if the context
allows. Also, the content of a declare expression is restricted to limited
forms. Here we introduce some of them that are related to performance tuning.

Please keep in mind that these optimization skills introduced in this section
are strongly connected to the Lisp implementation selected. Always check their
documentation before using `declare`!

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
; 7A7:       8D46F1           lea eax, [rsi-15]               ; no-arg-parsing entry point
; 7AA:       A801             test al, 1
; 7AC:       750E             jne L0
; 7AE:       3C0A             cmp al, 10
; 7B0:       740A             jeq L0
; 7B2:       A80F             test al, 15
; 7B4:       7576             jne L5
; 7B6:       807EF11D         cmp byte ptr [rsi-15], 29
; 7BA:       7770             jnbe L5
; 7BC: L0:   8D43F1           lea eax, [rbx-15]
; 7BF:       A801             test al, 1
; 7C1:       750E             jne L1
; 7C3:       3C0A             cmp al, 10
; 7C5:       740A             jeq L1
; 7C7:       A80F             test al, 15
; 7C9:       755A             jne L4
; 7CB:       807BF11D         cmp byte ptr [rbx-15], 29
; 7CF:       7754             jnbe L4
; 7D1: L1:   488BD3           mov rdx, rbx
; 7D4:       488BFE           mov rdi, rsi
; 7D7:       B9C1030020       mov ecx, 536871873              ; generic->
; 7DC:       FFD1             call rcx
; 7DE:       488B75F0         mov rsi, [rbp-16]
; 7E2:       488B5DF8         mov rbx, [rbp-8]
; 7E6:       7E09             jle L3
; 7E8:       488BD3           mov rdx, rbx
; 7EB: L2:   488BE5           mov rsp, rbp
; 7EE:       F8               clc
; 7EF:       5D               pop rbp
; 7F0:       C3               ret
; 7F1: L3:   4C8BCB           mov r9, rbx
; 7F4:       4C894DE8         mov [rbp-24], r9
; 7F8:       4C8BC6           mov r8, rsi
; 7FB:       4C8945E0         mov [rbp-32], r8
; 7FF:       488BD3           mov rdx, rbx
; 802:       488BFE           mov rdi, rsi
; 805:       B929040020       mov ecx, 536871977              ; generic-=
; 80A:       FFD1             call rcx
; 80C:       4C8B45E0         mov r8, [rbp-32]
; 810:       4C8B4DE8         mov r9, [rbp-24]
; 814:       488B75F0         mov rsi, [rbp-16]
; 818:       488B5DF8         mov rbx, [rbp-8]
; 81C:       498BD0           mov rdx, r8
; 81F:       490F44D1         cmoveq rdx, r9
; 823:       EBC6             jmp L2
; 825: L4:   CC0A             break 10                        ; error trap
; 827:       04               byte #X04
; 828:       13               byte #X13                       ; OBJECT-NOT-REAL-ERROR
; 829:       FE9B01           byte #XFE, #X9B, #X01           ; RBX
; 82C: L5:   CC0A             break 10                        ; error trap
; 82E:       04               byte #X04
; 82F:       13               byte #X13                       ; OBJECT-NOT-REAL-ERROR
; 830:       FE1B03           byte #XFE, #X1B, #X03           ; RSI
; 833:       CC0A             break 10                        ; error trap
; 835:       02               byte #X02
; 836:       19               byte #X19                       ; INVALID-ARG-COUNT-ERROR
; 837:       9A               byte #X9A                       ; RCX

* (defun max-with-speed-3 (a b)
    (declare (optimize (speed 3) (safety 0)))
    (max a b))
MAX-WITH-SPEED-3

* (disassemble 'max-with-speed-3)
; disassembly for MAX-WITH-SPEED-3
; Size: 92 bytes. Origin: #x52D452C3
; 3B:       48895DE0         mov [rbp-32], rbx                ; no-arg-parsing entry point
; 3F:       488945E8         mov [rbp-24], rax
; 43:       488BD0           mov rdx, rax
; 46:       488BFB           mov rdi, rbx
; 49:       B9C1030020       mov ecx, 536871873               ; generic->
; 4E:       FFD1             call rcx
; 50:       488B45E8         mov rax, [rbp-24]
; 54:       488B5DE0         mov rbx, [rbp-32]
; 58:       7E0C             jle L1
; 5A:       4C8BC0           mov r8, rax
; 5D: L0:   498BD0           mov rdx, r8
; 60:       488BE5           mov rsp, rbp
; 63:       F8               clc
; 64:       5D               pop rbp
; 65:       C3               ret
; 66: L1:   488945E8         mov [rbp-24], rax
; 6A:       488BF0           mov rsi, rax
; 6D:       488975F0         mov [rbp-16], rsi
; 71:       4C8BC3           mov r8, rbx
; 74:       4C8945F8         mov [rbp-8], r8
; 78:       488BD0           mov rdx, rax
; 7B:       488BFB           mov rdi, rbx
; 7E:       B929040020       mov ecx, 536871977               ; generic-=
; 83:       FFD1             call rcx
; 85:       488B45E8         mov rax, [rbp-24]
; 89:       488B75F0         mov rsi, [rbp-16]
; 8D:       4C8B45F8         mov r8, [rbp-8]
; 91:       4C0F44C6         cmoveq r8, rsi
; 95:       EBC6             jmp L0
~~~

As you can see, the generated assembly code is much shorter (92 bytes
VS 144).  The compiler was able to perform optimizations. Yet we
can do better by declaring types.


### Type Hints

As mentioned in the [*Type System*][sec:type] chapter, Lisp has a relatively
powerful type system. You may provide type hints so that the compiler may
reduce the size of the generated code.

~~~lisp
* (defun max-with-type (a b)
    (declare (optimize (speed 3) (safety 0)))
    (declare (type integer a b))
    (max a b))
MAX-WITH-TYPE

* (disassemble 'max-with-type)
; disassembly for MAX-WITH-TYPE
; Size: 42 bytes. Origin: #x52D48A23
; 1B:       488BF7           mov rsi, rdi                     ; no-arg-parsing entry point
; 1E:       488975F0         mov [rbp-16], rsi
; 22:       488BD8           mov rbx, rax
; 25:       48895DF8         mov [rbp-8], rbx
; 29:       488BD0           mov rdx, rax
; 2C:       B98C030020       mov ecx, 536871820               ; generic-<
; 31:       FFD1             call rcx
; 33:       488B75F0         mov rsi, [rbp-16]
; 37:       488B5DF8         mov rbx, [rbp-8]
; 3B:       480F4CDE         cmovl rbx, rsi
; 3F:       488BD3           mov rdx, rbx
; 42:       488BE5           mov rsp, rbp
; 45:       F8               clc
; 46:       5D               pop rbp
; 47:       C3               ret
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
safety level is set to 0, no type checking will be performed. Thus a wrong
type specifier might cause a lot of damage.

### More on Type Declaration with `declaim`

If you try to evaluate a `declare` form in the top level, you might get the
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

During development it is usually useful to raise the importance of safety in
order to find out potential problems as soon as possible. On the contrary,
speed might be more important after deployment. However, it might be too
verbose to specify declaration expression for each single function.

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

Please note that `declaim` works in **compile-time** of a file. It is mostly
used to make some declares local to that file. And it is unspecified whether
or not the compile-time side-effects of a declaim persist after the file has
been compiled.


### Declaring function types
Another useful declaration is a `ftype` declaration which establishes
the relationship between the function argument types and the return value type.
If the type of passed arguments matches the declared types, the return value type
is expected to match the declared one. Because of that, a function can have more
than one `ftype` declaration associated with it. A `ftype` declaration restricts
the type of the argument every time the function is called. It has the following form:

~~~lisp
 (declare (ftype (function (arg1 arg2 ...) return-value) function-name1))
~~~~

If the function returns `nil`, its return type is `null`.
This declaration does not put any restriction on the types of arguments by itself.
It only takes effect if the provided arguments have the specified types -- otherwise
no error is signaled and declaration has no effect. For example,
the following declamation states that if the argument to the function `square`
is a `fixnum`, the value of the function will also be a `fixnum`:

~~~lisp
(declaim (ftype (function (fixnum) fixnum) square))
(defun square (x) (* x x))
~~~~
If we provide it with the argument which is not declared to be of type `fixnum`,
no optimization will take place:

~~~lisp
(defun do-some-arithmetic (x)
  (the fixnum (+ x (square x))))
~~~~

Now let's try to optimize the speed. The compiler will state that there is type uncertainty:

~~~lisp
(defun do-some-arithmetic (x)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (the fixnum (+ x (square x))))

; compiling (DEFUN DO-SOME-ARITHMETIC ...)

; file: /tmp/slimeRzDh1R
 in: DEFUN DO-SOME-ARITHMETIC
;     (+ TEST-FRAMEWORK::X (TEST-FRAMEWORK::SQUARE TEST-FRAMEWORK::X))
;
; note: forced to do GENERIC-+ (cost 10)
;       unable to do inline fixnum arithmetic (cost 2) because:
;       The first argument is a NUMBER, not a FIXNUM.
;       unable to do inline (signed-byte 64) arithmetic (cost 5) because:
;       The first argument is a NUMBER, not a (SIGNED-BYTE 64).
;       etc.
;
; compilation unit finished
;   printed 1 note


      (disassemble 'do-some-arithmetic)
; disassembly for DO-SOME-ARITHMETIC
; Size: 53 bytes. Origin: #x52CD1D1A
; 1A:       488945F8         MOV [RBP-8], RAX                 ; no-arg-parsing entry point
; 1E:       488BD0           MOV RDX, RAX
; 21:       4883EC10         SUB RSP, 16
; 25:       B902000000       MOV ECX, 2
; 2A:       48892C24         MOV [RSP], RBP
; 2E:       488BEC           MOV RBP, RSP
; 31:       E8C2737CFD       CALL #x504990F8                  ; #<FDEFN SQUARE>
; 36:       480F42E3         CMOVB RSP, RBX
; 3A:       488B45F8         MOV RAX, [RBP-8]
; 3E:       488BFA           MOV RDI, RDX
; 41:       488BD0           MOV RDX, RAX
; 44:       E807EE42FF       CALL #x52100B50                  ; GENERIC-+
; 49:       488BE5           MOV RSP, RBP
; 4C:       F8               CLC
; 4D:       5D               POP RBP
; 4E:       C3               RET
NIL
~~~~


Now we can add a type declaration for `x`, so the compiler can assume
that the expression `(square x)` is a `fixnum`, and use the fixnum-specific `+`:

~~~lisp
(defun do-some-arithmetic (x)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type fixnum x))
  (the fixnum (+ x (square x))))

       (disassemble 'do-some-arithmetic)

; disassembly for DO-SOME-ARITHMETIC
; Size: 48 bytes. Origin: #x52C084DA
; 4DA:       488945F8         MOV [RBP-8], RAX                ; no-arg-parsing entry point
; 4DE:       4883EC10         SUB RSP, 16
; 4E2:       488BD0           MOV RDX, RAX
; 4E5:       B902000000       MOV ECX, 2
; 4EA:       48892C24         MOV [RSP], RBP
; 4EE:       488BEC           MOV RBP, RSP
; 4F1:       E8020C89FD       CALL #x504990F8                 ; #<FDEFN SQUARE>
; 4F6:       480F42E3         CMOVB RSP, RBX
; 4FA:       488B45F8         MOV RAX, [RBP-8]
; 4FE:       4801D0           ADD RAX, RDX
; 501:       488BD0           MOV RDX, RAX
; 504:       488BE5           MOV RSP, RBP
; 507:       F8               CLC
; 508:       5D               POP RBP
; 509:       C3               RET
NIL
~~~~

### Code Inline

The declaration [`inline`][inline] replaces function calls with function body,
if the compiler supports it. It will save the cost of function calls but will
potentially increase the code size. The best situation to use `inline` might
be those small but frequently used functions. The following snippet shows how
to encourage and prohibit code inline.

~~~lisp
;; The globally defined function DISPATCH should be open-coded,
;; if the implementation supports inlining, unless a NOTINLINE
;; declaration overrides this effect.
(declaim (inline dispatch))
(defun dispatch (x) (funcall (get (car x) 'dispatch) x))

;; Here is an example where inlining would be encouraged.
;; Because function DISPATCH was defined as INLINE, the code
;; inlining will be encouraged by default.
(defun use-dispatch-inline-by-default ()
  (dispatch (read-command)))

;; Here is an example where inlining would be prohibited.
;; The NOTINLINE here only affects this function.
(defun use-dispatch-with-declare-notinline  ()
  (declare (notinline dispatch))
  (dispatch (read-command)))

;; Here is an example where inlining would be prohibited.
;; The NOTINLINE here affects all following code.
(declaim (notinline dispatch))
(defun use-dispatch-with-declaim-noinline ()
  (dispatch (read-command)))

;; Inlining would be encouraged because you specified it.
;; The INLINE here only affects this function.
(defun use-dispatch-with-inline ()
  (declare (inline dispatch))
  (dispatch (read-command)))
~~~

Please note that when the inlined functions change, all the callers must be
re-compiled.

## Optimizing Generic Functions

### Using Static Dispatch

Generic functions provide much convenience and flexibility during
development. However, the flexibility comes with cost: generic methods are
much slower than trivial functions. The performance cost becomes a burden
especially when the flexibility is not needed.

The package [`inlined-generic-function`][inlined-generic-function] provides
functions to convert generic functions to static dispatch, moving the dispatch
cost to compile-time. You just need to define generic function as a
`inlined-generic-function`.

**Caution**

This package is declared as experimental thus is not recommended to be used in
a serious software production. Use it at your own risk!

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
[disassemble]: http://www.lispworks.com/documentation/lw60/CLHS/Body/f_disass.htm
[inlined-generic-function]: https://github.com/guicho271828/inlined-generic-function
[sec:type]: #type
[declare]: http://www.lispworks.com/documentation/lw71/CLHS/Body/s_declar.htm
[declare-scope]: http://www.lispworks.com/documentation/lw71/CLHS/Body/03_cd.htm
[optimize]: http://www.lispworks.com/documentation/lw71/CLHS/Body/d_optimi.htm
[sbcl-type]: http://sbcl.org/manual/index.html#Handling-of-Types
[declaim]: http://www.lispworks.com/documentation/lw71/CLHS/Body/m_declai.htm
[inline]: http://www.lispworks.com/documentation/lw51/CLHS/Body/d_inline.htm
[*features*]: http://www.lispworks.com/documentation/lw71/CLHS/Body/v_featur.htm
