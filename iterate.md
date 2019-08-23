---
title: iterate
---

`iterate` is a powerful iteration facility for Common Lisp, and a alternative to `loop`. As opposed to `loop`, `iterate` is more lispy, extensible, besides being more powerful.

See [The Iterate Manual] for a more detailed explanation of the various facilities. Also check [Other Resources on Iterate](#other-resources-on-iterate).

## Highlights

- [def*](#defclause-sequence): extensibility
- [display-iterate-clauses](#display-iterate-clauses): quick help for clauses
- [dsetq](#dsetq): for use outside the `iter` form
- [finally-protected](#finally-protected): for protecting against unwindings
- [finding-maximizing](#finding): `loop` equivalent can be quite verbose
- [for - Sequence Iteration](#for): in-string, in-vector, particularly, in-package, in-packages, in-file, in-stream, in-hashtable.
- [generate](#generate)
- [in](#in): `loop` equivalent can be quite verbose
- there's no support for "true" parallel assignment - see [with](#with)
- documentation is also availble as documentation strings for several of the keywords

## Other Points

- `iterate` does not declare variables unless asked to. See [Types and Declarations].
- `into `*`var`* stores the value into *`var`* instead of returning it.
- There, probably, are some idiosyncrasies involved with running the epilogue code - it is not run "always": `always`, `finding`, `finish`, `thereis`.


### Notes for contributors to this page

- Documentation for `finding` is incomplete here.
- Documentation for `def*`, `for` has been omitted here.
- When are the variable values for prologue defined?
- Will there be future versions of iterate? What is the compatibility relation between `terminate` and `finish` for `for-next` [Generalized Drivers]?

### Installation

If `quicklisp` is set-up, simply `(ql:quickload 'iterate)`. 

Otherwise [head over here](https://common-lisp.net/project/iterate/index.html).

### Accumulation vs Reduction

The differences between accumulate and reducing are slight. One difference is that the functions take their arguments in a different order. Another is that in the absence of init-val, accumulate will use nil, whereas reducing will generate different code that avoids any dependence on the initial value. The reason for having both clauses is that one usually thinks of reductions (like sum) and accumulations (like collect) as different beasts.


## Documentation

### accumulate
`accumulate `*`expr`*` by `*`func`*` &optional initial-value `*`init-val`*` into `*`var`*

This is a general-purpose accumulation clause. func should be a function of two arguments, the value of expr and the value accumulated so far in the iteration, and it should return the updated value. If no initial value is supplied, nil is used.


```lisp
CL-USER> (iter (for i in '(1 2 3))
               (accumulate i by 
                           (lambda (i values-so-far)
                             (cons i values-so-far))))
(3 2 1)
CL-USER> (iter (for i in '(1 2 3))
               (accumulate i by 
                           (lambda (i values-so-far)
                             (cons i values-so-far))
                           initial-value '(init)))
(3 2 1 INIT)
CL-USER> (iter (for i in '(1 2 3))
               (accumulate i by 
                          (lambda (i values-so-far)
                            (cons i values-so-far))
                          initial-value '(init) into var))
NIL
```


See [Accumulations], [Accumulation vs Reduction](#accumulation-vs-reduction) and [reducing](#reducing). 


### accumulating
An alias for [accumulate](#accumulate).

### adjoining
`adjoining `*`exptr`*` &optional into `*`var`*` test `*`test`*` at `*`place`*` result-type `*`type`*

Like [collect], but only adds the value of exptr if it is not already present. test, which defaults to #'eql, is the test to be used with member.

### after-each
`after-each &rest `*`forms`*

Executes forms at the end of the loop body, after each iteration. Forms may contain iterate clauses.
```lisp
CL-USER> (iter (for i below 4) 
               (after-each (print var))
               (if (oddp i)
                   (collect i into var)
                   (collect (* 2 i) into var)))

; (0) 
; (0 1) 
; (0 1 4) 
; (0 1 4 3) 
NIL
```
See [Code Placement] and [Problems with Code Movement].

### always
`always `*`expr`*
```lisp
CL-USER> (iter (for i below 4) (always (evenp i)))
NIL
```
- If expr ever evaluates to nil, then nil is immediately returned; the epilogue code is not executed. 
- If expr never evaluates to nil, the epilogue code is executed and the last value of expr (or t if expr was never evaluated) is returned (whereas loop would constantly return t).

See [Aggregated Boolean Tests].

### appending
`appending `*`exptr`*` &optional into `*`var`*` at `*`place`*
Like [collect], but behaves like the Common Lisp `append`, and works only on lists.
```lisp
CL-USER> (iter (for i in '((1) (2 3) (4 5 6)))
               (appending i))
(1 2 3 4 5 6)
CL-USER> (iter (for i in '((1) (2 3) (4 5 6)))
               (appending i into var))
NIL
```
See [Accumulations].

### as
An alias for [for](#for).

### collect
`collect `*`exptr`*` &optional into `*`var`*` test `*`test`*` at `*`place`*` result-type `*`type`*
```lisp
CL-USER> (iter (for i from 1 to 5)
               (collect i))
(1 2 3 4 5)
CL-USER> (iter (for i from 1 to 5)
               (collect i at start)) ;; likely to be faster
(5 4 3 2 1)
```
- `place` can be either `beginning`/`start` or `end`: default value is `end`.
- `type` should be a subtype of `sequence` - default is `list`; however, the type of sequence being constructed inside the loop is undefined when a non-list type is specified.

```lisp
CL-USER> (iter (for i from 1 to 3)
               (collect i into vec result-type 'vector)
               (print vec)
               (finally (return vec)))

; (1)
; (1 2) 
; (1 2 3) 
#(1 2 3)
```
- `type` or `place` may be optionally quoted.

See [Accumulations].

### collecting
Alias for [collect].

### count
Alias for [counting](#counting).

This, probably, overrides the CL `count` when used in top-level inside an iterate loop.
```lisp
CL-USER> (iter (for i in '(1 2 3))
               (finally (return (count 1 '(1 2 1)))))
2
```

### counting
`counting `*`expr`*` &optional into `*`var`*

See [Reductions] and [accumulate].

### declare-variables
`(declare (declare-variables))`

- iterate does not declare variable types unless asked to. 
- Declaration of types of user introduced symbols can be done by either the usual Common Lisp `declare`, but this declaration should be inside the iter form.
- Declaration of internal variables or use of `the` requires one to use `declare-variables`, or set `iterate:::*always-declare-variables* to `t`.

```lisp
CL-USER> (macroexpand-1 '(iter (for (the fixnum el) in '(1 2 3))
                               (declare (DECLARE-VARIABLES))
                               (count (oddp el))))
;; note that this produces a type declaration for el.
CL-USER> (macroexpand-1 '(iter (for el in '(1 2 3))
                               (declare (DECLARE-VARIABLES))
                               (count (oddp el))))
;; this does not produce a type declaration for el.
CL-USER> (macroexpand-1 '(iter (for (the fixnum el) in '(1 2 3))
                               (count (oddp el))))
;; this does not produce any declarations.
```
See [Types and Declarations].

### defclause-sequence
[Undocumented here.]

See [Extensibility Aids].

### defmacro-clause
[Undocumented here]

See [Rolling Your Own].

### defmacro-driver
[Undocumented here.]

See [Writing Drivers].

### defsynonym
[Undocumented here.]

See [Extensibility Aids].

### display-iterate-clauses
`display-iterate-clauses &optional `*`clause-spec`*

```lisp
CL-USER> (display-iterate-clauses 'repeat)
; REPEAT                    Repeat the loop some number of times
T
CL-USER> (display-iterate-clauses '(for in-vector))
; FOR IN-VECTOR &OPTIONAL FROM UPFROM DOWNFROM TO DOWNTO ABOVE BELOW BY 
;     WITH-INDEX            Elements of a vector
T
```

See [On-line help].

### dsetq
`dsetq`*` template expr`*

Can be used outside iter.
```lisp
CL-USER> (foo)
FIRST
SECOND
CL-USER> (progn
           (dsetq (values a b) (foo))
           (list a b)) ;; undeclared variables warning
(FIRST SECOND)
```
See [Destructuring].

### else
`else &rest `*`forms`*

Forms are executed if loop is never entered, but is terminated normally.
```lisp
CL-USER> (iter (for i in '(1 2 3)) (while nil)
           (else (write 'else)))
; ELSE
NIL
```
See [Code Placement] and [Problems with Code Movement].

### finally
`finally &rest `*`forms`*

Forms are executed after a normal termination of the loop.
```lisp
CL-USER> (iter (for i in '(1 2 3)) (finally (write 'end)))
; END
NIL
```
See [Code Placement] and [Problems with Code Movement].

### finally-protected
`finally-protected &rest `*`forms`*

Forms are executed "always" - regardless of whether the termination was notmal
```lisp
CL-USER> (iter (for i in-vector '(1 2 3)) 
           (finally-protected (write 'error)))
;; warnings
ERROR ; Evaluation aborted on #<SIMPLE-TYPE-ERROR expected-type: VECTOR datum: (1 2 3)>.
CL-USER> (iter (for i in '(1 2 3)) 
           (finally-protected (write 'no-error)))
; NO-ERROR
NIL
```
See [Code Placement] and [Problems with Code Movement].

### finding
`finding `*`expr`*` such-that `*`test`*` &optionally into `*`var`*` on-failure `*`failure-value`*

- The loop terminates (with epilogue code) whenever *`test`* evaluates to non-`nil`.
- *`expr`* that satifies the *`test`*, or *`failure-value`*, or `nil` is returned (unless modified by epilogue).
- *`failure-value`* is always evaluated.

```lisp
CL-USER> (iter (for x in '(1 2 3))
               (finding x such-that #'evenp on-failure 'not-found))
2
CL-USER> (iter (for x in '(1 2 3))
               (finding x such-that #'evenp on-failure (error "not found")))
; Evaluation aborted on #<SIMPLE-ERROR "not found" {1002F63063}>.
CL-USER> (iter (for x in '(1 2 3))
               (if (evenp x) (leave x))
               (finally (error "not found")))
2
```

`finding `*`expr`*` maximizing `*`m-expr`*` &optionally into `*`var`*

`finding `*`expr`*` minimizing `*`m-expr`*` &optionally into `*`var`*

- Returns *`expr`* corresponding to the maximum value of *`m-expr`*. 
- If *`m-expr`* is never evaluated (how?), the return value is `nil` or `0` depending on the type (or its absence) of *`expr`* (or *`var`* if supplied.)
- Here, *`m-expr`* can also be a list of two symbols.

```lisp
CL-USER> CL-USER> (iter (for list in '((1) (2 3) nil))
               (finding list maximizing (length list)))
(2 3)
CL-USER> (iter (for i in '(1 2 3))
               (finding (* 2 i) maximizing (- i) into (twice neg))
	           (finally (return (values twice neg))))
2
-1
```
[Example required for the case when *`m-expr`* is not evaluated.]

See [Finders].


### finish
`finish`

Stop the loop (and run the epilogue code).
```lisp
CL-USER> (iter (for i in '(1 2 3)) (if (evenp i) (finish)))
NIL
```

See [Control Flow].

### first-iteration-p
`first-iteration-p`

`t` in the first cycle of the loop, otherwise `nil`.
```lisp
CL-USER> (iter (for el in '(nil 1 2 nil 3))
               (when el
                 (unless (first-iteration-p)
                   (princ ", "))
                 (princ el)))
; , 1, 2, 3
NIL
```
See [Boolean Tests].

### first-time-p
`first-iteration-p`

`t` only when the expression is evaluated for the first time.
```lisp
CL-USER> (iter (for el in '(nil 1 2 nil 3))
               (when el
                 (unless (first-time-p)
                   (princ ", "))
                 (princ el)))
; 1, 2, 3
NIL
```
See [Boolean Tests].

### for
[Undocumented here.]

See

- [Numeric Iteration] 
- [Sequence Iteration]
- [Variable Binding and Setting]
- [Generalized Drivers] 
- [Previous Values of Driver Variables]

### generate
See [Generators] and [for].
```lisp
CL-USER> (iter (for el in '(a b nil c))
               (generate i upfrom 1)
               (if el (collect (cons el (next i)))))
((A . 1) (B . 2) (C . 3))
```
`for` can be replaced by `generate` to achieve the desired result, except in the case of [Variable Binding and Setting].

### generating
Alias for [generate](#generate)

### if-first-time
`if-first-time `*`then`*` &optional `*`else`*
```lisp
CL-USER> (iter (for i in '(1 2 3))
               (if-first-time
                (princ 'first)
                (print 'not-first)))
; FIRST
; NOT-FIRST 
; NOT-FIRST 
NIL
```
See [Control Flow].

### in
`in`*`name`*` &rest `*`forms`*
```lisp
CL-USER> (defvar ar #2A((1 2 3) (4 5 6)))
AR
CL-USER> (iter outer 
               (for i below (array-dimension ar 0))
               (iter (for j below (array-dimension ar 1))
                     (in outer
                         (collect (aref ar i j)))))
(1 2 3 4 5 6)
```

See [Named Blocks].

### initially
`in &rest `*`forms`*
Place the forms in the prologue of the loop.
```lisp
CL-USER> (iter (initially (princ 'hi))
               (for i below 3)
               (print i))
; HI
; 0 
; 1 
; 2 
NIL
CL-USER> (iter (for i below 3)
               (initially (princ i)))
; -1 ;; this is probably an undefined behaviour.
NIL
```
See [Code Placement] and [Problems with Code Movement].

### leave
`leave &optional `*`value`*

Returns from the current iterate form with *`value`* or `nil`.
```lisp
CL-USER> (iter (for i below 3)
               (leave
                (iter (for j below 2)
                      (if (oddp j) (leave j)))))
1
```

See [Control Flow].

### maximize
`maximize `*`expr`*` &optional into `*`var`*
```lisp
CL-USER> (iter (for list in '((1) (1 2) nil))
               (maximize (length list)))
2
```
See [Reductions] and [finding](#finding).

### maximizing
Alias for [maximize](#maximize).

### minimize
`minimize `*`expr`*` &optional into `*`var`*
```lisp
CL-USER> (iter (for list in '((1) (1 2) nil))
               (minimize (length list)))
0
```
See [Reductions] and [finding](#finding).

### minimizing
Alias for [minimize](#minimize).

### multiply
`multiply `*`expr`*` &optional into `*`var`*
```lisp
CL-USER> (iter (for i from 1 to 5)
               (multiply i))
120
```
Initial value of `*`var`* is 1.

See [Reductions].

### multiplying
Alias for [multiply](#

### nconcing
`nconcing `*`exptr`*` &optional into `*`var`*` test `*`test`*` at `*`place`*` result-type `*`type`*

See [Accumulations] and [collect].

### never
`never `*`expr`*

Effectively `(always (not expr))`, but does not influence the last value returned by a possible other `always` clause.
```lisp
CL-USER> (iter (repeat 2)
               (always 2)
               (never nil))
2
```

See [Aggregated Boolean Tests]

### next
See [generate](#generate).

### next-iteration
`next-iteration`
```lisp
CL-USER> (iter (for i below 3)
               (if (oddp i) 
                   (next-iteration)
                   (collect i)))
(0 2)
```
### nunioning
`nunioning `*`exptr`*` &optional into `*`var`*` test `*`test`*` at `*`place`*` result-type `*`type`*

See [Accumulations] and [collect].

### reducing
`reducing `*`expr`*` by `*`func`*` &optional initial-value `*`init-val`*` into `*`var`*
```lisp
CL-USER> (iter (for i in '(1 2 3))
               (reducing i by 
                         (lambda (value-so-far i)
                           (cons i value-so-far))
                         initial-value ()))
(3 2 1)
```

See [Reductions], [Accumulation vs Reduction](#accumulation-vs-reduction) and [accumulate](#accumulate).

### repeat
`repeat `*`n`*

Repeat the loop n times.

See [Drivers].
```lisp
CL-USER> (iter (repeat 3) (print 'doing))

; DOING 
; DOING 
; DOING 
NIL
```

### sum
`sum `*`expr`*` &optional into `*`var`*
```lisp
CL-USER> (iter (for i from 1 to 5)
               (sum i))
15
```
### summing
Alias for [sum](#sum).

### terminate
`terminate 

Use to terminate `for-next` clause. Effectively an alias for `finish` - but use with `for-next` to maintain compatibility with future versions of `iterate`(!).

```lisp
CL-USER> (iter (for i upfrom 0)
               (if (> i 5) (terminate) (collect i)))
(0 1 2 3 4 5)
CL-USER> (iter (initially (setq i 0))
               (for i next 
                    (if (> i 10) 
                        (terminate)
                        (incf i))))
NIL

```

See [Generalized Drivers].

### thereis
`thereis `*`expr`*

- If *`expr`* is ever non-`nil`, its value is returned without running the epilogue code.
- Otherwise epilogue code is run, and `nil` is returned.
- Cannot be used with `always` or `never`.

See [Aggregated Boolean Tests].

### unioning
`unioning `*`exptr`*` &optional into `*`var`*` test `*`test`*` at `*`place`*` result-type `*`type`*

See [Accumulations] and [collect].

### until
`until `*`expr`*
```lisp
CL-USER> (iter (for i in '(1 2 3 4 5))
               (until (> i 5))
               (collect i))
(1 2 3 4 5)
```
Equivalent to `(if expr (finish))`.

See [finish](#finish) and [Control Flow].

### while
`repeat `*`n`*
`until `*`expr`*

Equivalent to `(if (not expr) (finish))`. 
```lisp
CL-USER> (iter (for i below 10)
               (while (= 0 (rem i 5)))
               (collect i))
(0)
```
See [finish](#finish) and [Control Flow].

### with 
`with `*`var`*` &optional = `*`value`*

*`var`* is bound to *`value`* before the loop is entered. Binding happens sequentially, as while using a `let*`, and not in parallel as with `let`.
```lisp
CL-USER> (iter (with i = 0)
               (while (< i 3))
               (collect (incf i)))
(1 2 3)
CL-USER> (iter (with i = 1)
               (for i below 3)
               (collect (incf i)))
; Evaluation aborted on #<SIMPLE-ERROR "Iterate~@[, in ~a~]:~%Duplicate variable: ~a" {1004185183}>.
```

See [Variable Binding and Setting] and [Parallel Binding and Stepping].

## Other Resources on Iterate

- [The Iterate Manual]
- [Comparing LOOP and ITERATE](https://items.sjbach.com/211/comparing-loop-and-iterate)
- [Loop v Iterate - SabraOnTheHill](https://sites.google.com/site/sabraonthehill/loop-v-iter)

## Also check out

- [The Common Lisp Cookbook](http://lispcookbook.github.io/cl-cookbook/)


---

[The Iterate Manual]: https://common-lisp.net/project/iterate/index.html
[tCLC]: https://github.com/LispCookbook/cl-cookbook
[DLI]: appendix/Don't Loop, Iterate
[Accumulations]: https://common-lisp.net/project/iterate/doc/Accumulations.html
[Code Placement]: https://common-lisp.net/project/iterate/doc/Code-Placement.html#index-after_002deach-103
[Problems with Code Movement]: https://common-lisp.net/project/iterate/doc/Problems-with-Code-Movement.html#Problems-with-Code-Movement
[Aggregated Boolean Tests]: https://common-lisp.net/project/iterate/doc/Aggregated-Boolean-Tests.html
[collect]: #collect
[Reductions]: https://common-lisp.net/project/iterate/doc/Reductions.html
[Types and Declarations]: https://common-lisp.net/project/iterate/doc/Types-and-Declarations.html
[Extensibility Aids]: https://common-lisp.net/project/iterate/doc/Extensibility-Aids.html
[Rolling Your Own]: https://common-lisp.net/project/iterate/doc/Rolling-Your-Own.html
[Writing Drivers]: https://common-lisp.net/project/iterate/doc/Writing-Drivers.html
[On-line help]: https://common-lisp.net/project/iterate/doc/On_002dline-Help.html
[Destructuring]: https://common-lisp.net/project/iterate/doc/Destructuring.html
[Finders]: https://common-lisp.net/project/iterate/doc/Finders.html
[Control Flow]: https://common-lisp.net/project/iterate/doc/Control-Flow.html
[Boolean Tests]: https://common-lisp.net/project/iterate/doc/Boolean-Tests.html
[Numeric Iteration]: https://common-lisp.net/project/iterate/doc/Numerical-Iteration.html
[Sequence Iteration]: https://common-lisp.net/project/iterate/doc/Sequence-Iteration.html
[Variable Binding and Setting]: https://common-lisp.net/project/iterate/doc/Variable-Binding-and-Setting.html
[Generalized Drivers]: https://common-lisp.net/project/iterate/doc/Generalized-Drivers.html
[Previous Values of Driver Variables]: https://common-lisp.net/project/iterate/doc/Previous-Values-of-Driver-Variables.html
[Generators]: https://common-lisp.net/project/iterate/doc/Generators.html
[for]: #for
[Named Blocks]: https://common-lisp.net/project/iterate/doc/Named-Blocks.html
[Drivers]: https://common-lisp.net/project/iterate/doc/Drivers.html
[accumulate]: #accumulate
[Parallel Binding and Stepping]: https://common-lisp.net/project/iterate/doc/Parallel-Binding-and-Stepping.html#Parallel-Binding-and-Stepping
