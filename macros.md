---
title: Macros
---

The word _macro_ is used generally in computer science to mean a syntactic extension to a programming language. (Note: The name comes from the word "macro-instruction," which was a useful feature of many second-generation assembly languages. A macro-instruction looked like a single instruction, but expanded into a sequence of actual instructions. The basic idea has since been used many times, notably in the C preprocessor. The name "macro" is perhaps not ideal, since it connotes nothing relevant to what it names, but we're stuck with it.) Although many languages have a macro facility, none of them are as powerful as Lisp's. The basic mechanism of Lisp macros is simple, but has subtle complexities, so learning your way around it takes a bit of practice.

# How Macros Work

A macro is an ordinary piece of Lisp code that operates on _another piece of putative Lisp code,_ translating it into (a version closer to) executable Lisp. That may sound a bit complicated, so let's give a simple example. Suppose you want a version of [`setq`](http://www.lispworks.com/documentation/HyperSpec/Body/s_setq.htm) that sets two variables to the same value. So if you write

~~~lisp
(setq2 x y (+ z 3))
~~~

when `z`=8 then both `x` and `y` are set to 11. (I can't think of any use for this, but it's just an example.)

It should be obvious that we can't define `setq2` as a function. If `x`=50 and `y`=_-5_, this function would receive the values 50, _-5_, and 11; it would have no knowledge of what variables were supposed to be set. What we really want to say is, When you (the Lisp system) see:

```lisp
(setq2 v1 v2 e)
```

then treat it as equivalent to:

```lisp
(progn
  (setq v1 e)
  (setq v2 e))
```

Actually, this isn't quite right, but it will do for now. A macro allows us to do precisely this, by specifying a program for transforming the input pattern <code>(setq2 <i>v<sub>1</sub></i> <i>v<sub>2</sub></i> <i>e</i>)</code> into the output pattern `(progn ...)`.

## Quote

Here's how we could define the `setq2` macro:

~~~lisp
(defmacro setq2 (v1 v2 e)
  (list 'progn (list 'setq v1 e) (list 'setq v2 e)))
~~~

It takes as parameters two variables and one expression.

Then it returns a piece of code. In Lisp, because code is represented
as lists, we can simply return a list that represents code.

We also use the *quote*: each *quoted* symbol evaluates to itself, aka
it is returned as is:

- `(quote foo bar baz)` returns `(foo bar baz)`
- the quote character, `'`, is a shortcut for `quote`, a *special operator* (not a function nor a macro, but one of a few special operators forming the core of Lisp).
- so, `'foo` evaluates to `foo`.

So, our macro retourns the following bits:

- the symbol `progn`,
- a second list, that contains
  - the symbol `setq`
  - the variable `v1`: note that the variable is not evaluated inside the macro!
  - the expression `e`: it is not evaluated either!
- a second list, with `v2`.

We can use it like this:

```lisp
(defparameter v1 1)
(defparameter v2 2)
(setq2 v1 v2 3)
;; 3
```

We can check, `v1` and `v2` were set to `3`.


## Macroexpand

We must start writing a macro when we know what code we want to
generate. Once we've begun writing one, it becomes very useful to
check effectively what code does the macro generate. The function for
that is `macroexpand`. It is a function, and we give it some code, as
a list (so, we quote the code snippet we give it):

~~~lisp
(macroexpand '(setq2 v1 v2 3))
;; (PROGN (SETQ V1 3) (SETQ V2 3))
;; T
~~~

Yay, our macro expands to the code we wanted!

More interestingly:

~~~lisp
(macroexpand '(setq2 v1 v2 (+ z 3)))
;; (PROGN (SETQ V1 (+ z 3)) (SETQ V2 (+ z 3)))
;; T
~~~

We can confirm that our expression `e`, here `(+ z 3)`, was not
evaluated. We will see how to control the evaluation of arguments with
the comma: `,`.

## Note: Slime tips

With Slime, you can call macroexpand by putting the cursor at
the left of the parenthesis of the s-expr to expand and call the function``M-x
slime-macroexpand-[1,all]``, or ``C-c M-m``:

~~~lisp
[|](setq2 v1 v2 3)
;^ cursor
; C-c M-m
; =>
; (PROGN (SETQ V1 3) (SETQ V2 3))
~~~

Another tip: on a macro name, type ``C-c C-w m`` (or ``M-x
slime-who-macroexpands``) to get a new buffer with all the places
where the macro was expanded. Then type the usual ``C-c C-k``
(``slime-compile-and-load-file``) to recompile all of them.


## Macros VS functions

Our macro is very close to the following function definition:

~~~lisp
(defun setq2-function (v1 v2 e)
  (list 'progn (list 'setq v1 e) (list 'setq v2 e)))
~~~

If we evaluated `(setq2-function 'x 'y '(+ z 3))` (note that each
argument is *quoted*, so it isn't evaluated when we call the
function), we would get

```lisp
(progn (setq x (+ z 3)) (setq y (+ z 3)))
```

This is a perfectly ordinary Lisp computation, whose sole point of interest is that its output is a piece of executable Lisp code. What `defmacro` does is create this function implicitly and make sure that whenever an expression of the form `(setq2 x y (+ z 3))` is seen, `setq2-function` is called with the pieces of the form as arguments, namely `x`, `y`, and `(+ z 3)`. The resulting piece of code then replaces the call to `setq2`, and execution resumes as if the new piece of code had occurred in the first place. The macro form is said to _expand_ into the new piece of code.

## Evaluation context

This is all there is to it, except, of course, for the myriad subtle consequences. The main consequence is that _run time for the `setq2` macro_ is _compile time for its context._ That is, suppose the Lisp system is compiling a function, and midway through it finds the expression `(setq2 x y (+ z 3))`. The job of the compiler is, of course, to translate source code into something executable, such as machine language or perhaps byte code. Hence it doesn't execute the source code, but operates on it in various mysterious ways. However, once the compiler sees the `setq2` expression, it must suddenly switch to executing the body of the `setq2` macro. As I said, this is an ordinary piece of Lisp code, which can in principle do anything any other piece of Lisp code can do. That means that when the compiler is running, the entire Lisp (run-time) system must be present.

We'll stress this once more: at compile-time, you have the full language at your disposal.

Novices often make the following sort of mistake. Suppose that the `setq2` macro needs to do some complex transformation on its `e` argument before plugging it into the result. Suppose this transformation can be written as a Lisp procedure `some-computation`. The novice will often write:

~~~lisp
(defmacro setq2 (v1 v2 e)
  (let ((e1 (some-computation e)))
    (list 'progn (list 'setq v1 e1) (list 'setq v2 e1))))

(defmacro some-computation (exp) ...) ;; _Wrong!_
~~~

The mistake is to suppose that once a macro is called, the Lisp system enters a "macro world," so naturally everything in that world must be defined using `defmacro`. This is the wrong picture. The right picture is that `defmacro` enables a step into the _ordinary Lisp world_, but in which the principal object of manipulation is Lisp code. Once that step is taken, one uses ordinary Lisp function definitions:

~~~lisp
(defmacro setq2 (v1 v2 e)
  (let ((e1 (some-computation e)))
    (list 'progn (list 'setq v1 e1) (list 'setq v2 e1))))

(defun some-computation (exp) ...) ;; _Right!_
~~~

One possible explanation for this mistake may be that in other languages, such as C, invoking a preprocessor macro _does_ get you into a different world; you can't run an arbitrary C program. It might be worth pausing to think about what it might mean to be able to.

Another subtle consequence is that we must spell out how the arguments to the macro get distributed to the hypothetical behind-the-scenes function (called `setq2-function` in my example). In most cases, it is easy to do so: In defining a macro, we use all the usual `lambda`-list syntax, such as `&optional`, `&rest`, `&key`, but what gets bound to the formal parameters are pieces of the macro form, not their values (which are mostly unknown, this being compile time for the macro form). So if we defined a macro thus:

~~~lisp
(defmacro foo (x &optional y &key (cxt 'null)) ...)
~~~

then

_If we call it thus ..._     |_The parameters' values are ..._
-----------------------------|-----------------------------------
`(foo a)`                    | `x=a`, `y=nil`, `cxt=null`
`(foo (+ a 1) (- y 1))`      |`x=(+ a 1)`, `y=(- y 1)`, `cxt=null`
`(foo a b :cxt (zap zip))`   |`x=a`, `y=b`, `cxt=(zap zip)`

<br>

Note that the values of the variables are the actual expressions `(+ a 1)` and `(zap zip)`. There is no requirement that these expressions' values be known, or even that they have values. The macro can do anything it likes with them. For instance, here's an even more useless variant of `setq`: <code>(setq-reversible <i>e<sub>1</sub></i> <i>e<sub>2</sub></i> <i>d</i>)</code> behaves like <code>(setq <i>e<sub>1</sub></i> <i>e<sub>2</sub></i>)</code> if <i>d=</i><code>:normal</code>, and behaves like <code>(setq <i>e<sub>2</sub></i> <i>e<sub>1</sub></i>)</code> if _d=_`:backward`. It could be defined thus:

~~~lisp
(defmacro setq-reversible (e1 e2 direction)
  (case direction
    (:normal (list 'setq e1 e2))
    (:backward (list 'setq e2 e1))
    (t (error "Unknown direction: ~a" direction))))
~~~

Here's how it expands:

~~~lisp
(macroexpand '(setq-reversible x y :normal))
(SETQ X Y)
T
(macroexpand '(setq-reversible x y :backward))
(SETQ Y X)
T
~~~

And with a wrong direction:

~~~lisp
(macroexpand '(setq-reversible x y :other-way-around))
~~~

We get an error and are prompted into the debugger!

We'll see the backquote and comma mechanism in the next section, but
here's a fix:

~~~lisp
(defmacro setq-reversible (v1 v2 direction)
  (case direction
    (:normal (list 'setq v1 v2))
    (:backward (list 'setq v2 v1))
    (t `(error "Unknown direction: ~a" ,direction))))
    ;; ^^ backquote                    ^^ comma: get the value inside the backquote.

(macroexpand '(SETQ-REVERSIBLE v1 v2 :other-way-around))
;; (ERROR "Unknown direction: ~a" :OTHER-WAY-AROUND)
;; T
~~~

Now when we call `(setq-reversible v1 v2 :other-way-around)` we still get the
error and the debugger, but at least not when using `macroexpand`.

<a name="2-backquote"></a>

# Backquote and comma

Before taking another step, we need to introduce a piece of Lisp notation that is indispensable to defining macros, even though technically it is quite independent of macros. This is the _backquote facility_. As we saw above, the main job of a macro, when all is said and done, is to define a piece of Lisp code, and that means evaluating expressions such as `(list 'prog (list 'setq ...) ...)`. As these expressions grow in complexity, it becomes hard to read them and write them. What we find ourselves wanting is a notation that provides the skeleton of an expression, with some of the pieces filled in with new expressions. That's what backquote provides. Instead of the the `list` expression given above, one writes

~~~lisp
  `(progn (setq ,v1 ,e) (setq ,v2 ,e))
;;^ backquote   ^   ^         ^   ^ commas
~~~

The backquote (`) character signals that in the expression that follows, every subexpression _not_ preceded by a comma is to be quoted, and every subexpression preceded by a comma is to be evaluated.

You can think of it, and use it, as data interpolation:

~~~lisp
`(v1 = ,v1) ;; => (V1 = 3)
~~~


That's mostly all there is to backquote. There are just two extra items to point out.

### Comma-splice ,@

First, if you write "`,@e`" instead of "`,e`" then the value of _e_ is _spliced_ (or "joined", "combined", "interleaved") into the result. So if `v` equals `(oh boy)`, then

~~~lisp
(zap ,@v ,v)
~~~

evaluates to

~~~lisp
(zap oh boy (oh boy))
;;   ^^^^^ elements of v (two elements), spliced.
;;          ^^ v itself (a list)
~~~

The second occurrence of `v` is replaced by its value. The first is replaced by the elements of its value. If `v` had had value `()`, it would have disappeared entirely: the value of `(zap ,@v ,v)` would have been `(zap ())`, which is the same as `(zap nil)`.

### Quote-comma ',

When we are inside a backquote context and we want to print an
expression literally, we have no choice but to use the combination of
quote and comma:

~~~lisp
(defmacro explain-exp (exp)
  `(format t "~S = ~S" ',exp ,exp))
  ;;                   ^^

(explain-exp (+ 2 3))
;; (+ 2 3) = 5
~~~

See by yourself:

~~~lisp
;; Defmacro with no quote at all:
(defmacro explain-exp (exp)
  (format t "~a = ~a" exp exp))
(explain-exp v1)
;; V1 = V1

;; OK, with a backquote and a comma to get the value of exp:
(defmacro explain-exp (exp)
  ;; WRONG example
  `(format t "~a = ~a" exp ,exp))
(explain-exp v1)
;; => error: The variable EXP is unbound.

;; We then must use quote-comma:
(defmacro explain-exp (exp)
  `(format t "~a = ~a" ',exp ,exp))
(explain-exp (+ 1 2))
;; (+ 1 2) = 3
~~~


### Nested backquotes

Second, one might wonder what happens if a backquote expression occurs inside another backquote. The answer is that the backquote becomes essentially unreadable and unwriteable; using nested backquote is usually a tedious debugging exercise. The reason, in my not-so-humble opinion, is that backquote is defined wrong. A comma pairs up with the innermost backquote when the default should be that it pairs up with the outermost. But this is not the place for a rant; consult your favorite Lisp reference for the exact behavior of nested backquote plus some examples.

### Building lists with backquote

One problem with backquote is that once you learn it you tend to use for every list-building occasion. For instance, you might write

~~~lisp
(mapcan (lambda (x)
          (cond ((symbolp x) `((,x)))
                ((> x 10) `(,x ,x))
                (t '())))
        some-list)
~~~

which yields `((a) 15 15)` when `some-list` = `(a 6 15)`. The problem is that [`mapcan`](http://www.lispworks.com/documentation/HyperSpec/Body/f_mapc_.htm) destructively alters the results returned by the [`lambda`](http://www.lispworks.com/documentation/HyperSpec/Body/s_lambda.htm)-expression. Can we be sure that the lists returned by that expression are "[fresh](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#fresh)," that is, they are different (in the [`eq`](http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm) sense) from the structures returned on other calls of that `lambda` expression? In the present case, close analysis will show that they must be fresh, but in general backquote is not obligated to return a fresh list every time (whether it does or not is implementation-dependent). If the example above got changed to

~~~lisp
(mapcan (lambda (x)
          (cond ((symbolp x) `((,x)))
                ((> x 10) `(,x ,x))
                ((>= x 0) `(low))
                (t '())))
        some-list)
~~~

then backquote may well treat <code>`(low)</code> as if it were `'(low)`; the list will be allocated at load time, and every time the `lambda` is evaluated, that same chunk of storage will be returned. So if we evaluate the expression with `some-list` = `(a 6 15)`, we will get `((a) low 15 15)`, but as a side effect the constant `(low)` will get clobbered to become `(low 15 15)`. If we then evaluate the expression with, say, `some-list` = `(8 oops)`, the result will be `(low 15 15 (oops))`, and now the "constant" that started off as `'(low)` will be `(low 15 15 (oops))`. (Note: The bug exemplified here takes other forms, and has often bit newbies - as well as experienced programmers - in the ass. The general form is that a constant list is produced as the value of something that is later destructively altered. The first line of defense against this bug is never to destructively alter any list. For newbies, this is also the last line of defense. For those of us who imagine we're more sophisticated, the next line of defense is to think very carefully any time you use [`nconc`](http://www.lispworks.com/documentation/HyperSpec/Body/f_nconc.htm) or `mapcan`).

To fix the bug, you can write `(map 'list ...)` instead of `mapcan`. However, if you are determined to use `mapcan`, write the expression this way:

~~~lisp
(mapcan (lambda (x)
          (cond ((symbolp x) (list `(,x)))
                ((> x 10) (list x x))
                ((>= x 0) (list 'low))
                (t '())))
        some-list)
~~~

My personal preference is to use backquote _only_ to build S-expressions, that is, hierarchical expressions that consist of symbols, numbers, and strings, and that are not conceptualized as changing in length. For instance, I would never write

~~~lisp
(setq sk `(,x ,@sk))
~~~

If `sk` is being used as a stack, that is, it's going to be [`pop`](http://www.lispworks.com/documentation/HyperSpec/Body/m_pop.htm)ped in the normal course of things, I would write `(push x sk)`. If not, I would write `(setq sk (cons x sk))`.

<a name="LtohTOCentry-3"></a>

# Getting Macros Right

I said in the first section that my definition of `setq2` wasn't quite right, and now it's time to fix it.

Suppose we write `(setq2 x y (+ x 2))`, when `x`_=8_. Then according to the definition given above, this form will expand into

~~~lisp
(progn
  (setq x (+ x 2))
  (setq y (+ x 2)))
~~~

so that `x` will have value 10 and `y` will have value 12. Indeed, here's its macroexpansion:

~~~lisp
(macroexpand '(setq2 x y (+ x 2)))
;;(PROGN (SETQ X (+ X 2)) (SETQ Y (+ X 2)))
~~~

Chances are that isn't what the macro is expected to do (although you never know). Another problematic case is `(setq2 x y (pop l))`, which causes `l` to be popped twice; again, probably not right.

The solution is to evaluate the expression `e` just once, save it in a temporary variable, and then set `v1` and `v2` to it.

## Gensym

To make temporary variables, we use the `gensym` function, which returns a fresh variable guaranteed to appear nowhere else. Here is what the macro should look like:

~~~lisp
(defmacro setq2 (v1 v2 e)
  (let ((tempvar (gensym)))
    `(let ((,tempvar ,e))
       (progn (setq ,v1 ,tempvar)
              (setq ,v2 ,tempvar)))))
~~~

Now `(setq2 x y (+ x 2))` expands to

~~~lisp
(let ((#:g2003 (+ x 2)))
  (progn (setq x #:g2003) (setq y #:g2003)))
~~~

Here `gensym` has returned the symbol `#:g2003`, which prints in this funny way because it won't be recognized by the reader. (Nor is there any need for the reader to recognize it, since it exists only long enough for the code that contains it to be compiled.)

Exercise: Verify that this new version works correctly for the case `(setq2 x y (pop l1))`.

Exercise: Try writing the new version of the macro without using backquote. If you can't do it, you have done the exercise correctly, and learned what backquote is for!

The moral of this section is to think carefully about which expressions in a macro get evaluated and when. Be on the lookout for situations where the same expression gets plugged into the output twice (as `e` was in my original macro design). For complex macros, watch out for cases where the order that expressions are evaluated differs from the order in which they are written. This is sure to trip up some user of the macro - even if you are the only user.<a name="LtohTOCentry-4"></a>

# What Macros are For

Macros are for making syntactic extensions to Lisp. One often hears it said that macros are a bad idea, that users can't be trusted with them, and so forth. Balderdash. It is just as reasonable to extend a language syntactically as to extend it by defining your own procedures. It may be true that the casual reader of your code can't understand the code without seeing the macro definitions, but then the casual reader can't understand it without seeing function definitions either. Having [`defmethod`](http://www.lispworks.com/documentation/HyperSpec/Body/m_defmet.htm)s strewn around several files contributes far more to unclarity than macros ever have, but that's a different diatribe.

Before surveying what sorts of syntactic extensions I have found useful, let me point out what sorts of syntactic extensions are generally _not_ useful, or best accomplished using means other than macros. Some novices think macros are useful for open-coding functions. So, instead of defining

~~~lisp
(defun sqone (x)
  (let ((y (+ x 1))) (* y y)))
~~~

they might define

~~~lisp
(defmacro sqone (x)
  `(let ((y (+ ,x 1))) (* y y)))
~~~

So that `(sqone (* z 13))` might expand into

~~~lisp
(let ((y (+ (* z 13) 1)))
  (* y y))
~~~

This is correct, but a waste of effort. For one thing, the amount of time saved is almost certainly negligible. If it's really important that `sqone` be expanded inline, one can put `(declaim (inline sqone))` before `sqone` is defined (although the compiler is not obligated to honor this declaration). For another, once `sqone` is defined as a macro, it becomes impossible to write `(mapcar #'sqone ll)`, or to do anything else with it except call it.

But macros have a thousand and one legitimate uses. Why write `(lambda (x) ...)` when you can write `(\\ (x) ...)`? Just define `\\` as a macro: <code>(defmacro \\ (&rest list) `(lambda ,@list))</code>.

Many people find `mapcar` and `mapcan` a bit too obscure, especially when used with large `lambda` expressions. Rather than write something like

~~~lisp
(mapcar (lambda (x)
          (let ((y (hairy-fun1 x))
                (z (hairy-fun2 x)))
            (dolist (y1 y)
              (dolist (z1 z)
                _... and further meaningless_
                _space-filling nonsense..._
                ))))
        list)
~~~

we might prefer to write

~~~lisp
(for (x :in list)
     (let ((y (hairy-fun1 x))
           (z (hairy-fun2 x)))
       (dolist (y1 y)
         (dolist (z1 z)
           _... and further meaningless_
           _space-filling nonsense..._
           ))))
~~~

This macro might be defined thus:

~~~lisp
(defmacro for (listspec exp)
  ;;           ^^ listspec = (x :in list), a list of length 3.
  ;;                    ^^ exp = the rest of the code.
  (cond
    ((and (= (length listspec) 3)
          (symbolp (first listspec))
          (eq (second listspec) ':in))
     `(mapcar (lambda (,(first listspec))
                ,exp)
              ,(third listspec)))
    (t (error "Ill-formed for spec: ~A" listspec)))))
~~~

(This is a simplified version of a macro by Chris Riesbeck.)

It's worth stopping for a second to discuss the role the keyword `:in` plays in this macro. It serves as a sort of "local syntax marker," in that it has no meaning as far as Lisp is concerned, but does serve as a syntactic guidepost for the macro itself. I will refer to these markers as _guide symbols_. (Here its job may seem trivial, but if we generalized the `for` macro to allow multiple list arguments and an implicit `progn` in the body the `:in`s would be crucial in telling us where the arguments stopped and the body began.)

It is not strictly necessary for the guide symbols of a macro to be in the [keyword package](http://www.lispworks.com/documentation/HyperSpec/Body/11_abc.htm), but it is a good idea, for two reasons. First, they highlight to the reader that something idiosyncratic is going on. A form like `(for ((x in (foobar a b 'oof))) (something-hairy x (list x)))` looks a bit wrong already, because of the double parentheses before the `x`. But using "`:in`" makes it more obvious.

Second, notice that I wrote `(eq (second listspec) ':in)` in the macro definition to check for the presence of the guide symbol. If I had used `in` instead, I would have had to think about which package _my_ `in` lives in and which package the macro user's `in` lives in. One way to avoid trouble would be to write

~~~lisp
(and (symbolp (second listspec))
     (eq (intern (symbol-name (second listspec))
                 :keyword)
         ':in))
~~~

Another would be to write

~~~lisp
(and (symbolp (second listspec))
     (string= (symbol-name (second listspec)) (symbol-name 'in)))
~~~

which neither of which is particularly clear or aesthetic. The keyword package is there to provide a home for symbols whose home is not per se relevant to anything; you might as well use it. (Note: In ANSI Lisp, I could have written `"IN"` instead of `(symbol-name 'in)`, but there are Lisp implementations that do not convert symbols' names to uppercase. Since I think the whole uppercase conversion idea is an embarrassing relic, I try to write code that is portable to those implementations.)

Let's look at another example, both to illustrate a nice macro, and to provide an auxiliary function for some of the discussion below. One often wants to create new symbols in Lisp, and `gensym` is not always adequate for building them. Here is a description of an alternative facility called `build-symbol`:

> <code>(build-symbol [(:package <em>p</em>)] <em>-pieces-</em>)</code> builds a symbol by concatenating the given _pieces_ and interns it as specified by _p_. For each element of _pieces_, if it is a ...
>
> *   ... string: The string is added to the new symbol's name.
> *   ... symbol: The name of the symbol is added to the new symbol's name.
> *   ... expression of the form <code>(:< <em>e</em>)</code>: _e_ should evaluate to a string, symbol, or number; the characters of the value of _e_ (as printed by `princ`) are concatenated into the new symbol's name.
> *   ... expression of the form <code>(:++ <em>p</em>)</code>: _p_ should be a place expression (i.e., appropriate as the first argument to `setf`), whose value is an integer; the value is incremented by 1, and the new value is concatenated into the new symbol's name.
>
> If the `:package` specification is omitted, it defaults to the value of `*package*`. If _p_ is `nil`, the symbol is interned nowhere. Otherwise, it should evaluate to a package designator (usually, a keyword whose name is the same of a package).

For example, `(build-symbol (:< x) "-" (:++ *x-num*))`, when `x` = `foo` and `*x-num*` = 8, sets `*x-num*` to 9 and evaluates to `FOO-9`. If evaluated again, the result will be `FOO-10`, and so forth.

Obviously, `build-symbol` can't be implemented as a function; it has to be a macro. Here is an implementation:

~~~lisp
(defmacro build-symbol (&rest list)
  (let ((p (find-if (lambda (x)
                      (and (consp x)
                           (eq (car x) ':package)))
                    list)))
    (when p
      (setq list (remove p list)))
    (let ((pkg (cond ((eq (second p) 'nil)
                      nil)
                     (t `(find-package ',(second p))))))
      (cond (p
             (cond (pkg
                    `(values (intern ,(symstuff list) ,pkg)))
                   (t
                    `(make-symbol ,(symstuff list)))))
            (t
             `(values (intern ,(symstuff list))))))))

(defun symstuff (list)
  `(concatenate 'string
                ,@(for (x :in list)
                       (cond ((stringp x)
                              `',x)
                             ((atom x)
                              `',(format nil "~a" x))
                             ((eq (car x) ':<)
                              `(format nil "~a" ,(second x)))
                             ((eq (car x) ':++)
                              `(format nil "~a" (incf ,(second x))))
                             (t
                              `(format nil "~a" ,x))))))
~~~

(Another approach would be have `symstuff` return a single call of the form <code>(format nil <em>format-string -forms-</em>)</code>, where the _forms_ are derived from the _pieces_, and the _format-string_ consists of interleaved ~a's and strings.)

Sometimes a macro is needed only temporarily, as a sort of syntactic scaffolding. Suppose you need to define 12 functions, but they fall into 3 stereotyped groups of 4:

~~~lisp
(defun make-a-zip (y z)
  (vector 2 'zip y z))
(defun test-whether-zip (x)
  (and (vectorp x) (eq (aref x 1) 'zip)))
(defun zip-copy (x) ...)
(defun zip-deactivate (x) ...)

(defun make-a-zap (u v w)
  (vector 3 'zap u v w))
(defun test-whether-zap (x) ...)
(defun zap-copy (x) ...)
(defun zap-deactivate (x) ...)

(defun make-a-zep ()
  (vector 0 'zep))
(defun test-whether-zep (x) ...)
(defun zep-copy (x) ...)
(defun zep-deactivate (x) ...)
~~~

Where the omitted pieces are the same in all similarly named functions. (That is, the "..." in `zep-deactivate` is the same code as the "..." in `zip-deactivate`, and so forth.) Here, for the sake of concreteness, if not plausibility, `zip`, `zap`, and `zep` are behaving like odd little data structures. The functions could be rather large, and it would get tedious keeping them all in sync as they are debugged. An alternative would be to use a macro:

~~~lisp
(defmacro odd-define (name buildargs)
  `(progn (defun ,(build-symbol make-a- (:< name))
                                ,buildargs
            (vector ,(length buildargs) ',name ,@buildargs))
          (defun ,(build-symbol test-whether- (:< name)) (x)
            (and (vectorp x) (eq (aref x 1) ',name))
          (defun ,(build-symbol (:< name) -copy) (x)
            ...)
          (defun ,(build-symbol (:< name) -deactivate) (x)
            ...))))

(odd-define zip (y z))
(odd-define zap (u v w))
(odd-define zep ())
~~~

If all the uses of this macro are collected in this one place, it might be clearer to make it a local macro using [macrolet](http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm):

~~~lisp
(macrolet ((odd-define (name buildargs)
             `(progn (defun ,(build-symbol make-a- (:< name))
                                           ,buildargs
                       (vector ,(length buildargs)
                               ',name
                                ,@buildargs))
                     (defun ,(build-symbol test-whether- (:< name))
                            (x)
                       (and (vectorp x) (eq (aref x 1) ',name))
                     (defun ,(build-symbol (:< name) -copy) (x)
                       ...)
                     (defun ,(build-symbol (:< name) -deactivate) (x)
                       ...)))))
(odd-define zip (y z))
(odd-define zap (u v w))
(odd-define zep ()))
~~~

Finally, macros are essential for defining "command languages." A _command_ is a function with a short name for use by users in interacting with Lisp's read-eval-print loop. A short name is useful and possible because we want it to be easy to type and we don't care much whether the name clashes some other command; if two command names clash, we can change one of them.

As an example, let's define a little command language for debugging macros. (You may actually find this useful.) There are just two commands, `ex` and `fi`. They keep track of a "current form," the thing to be macro-expanded or the result of such an expansion:

1.  <code>(ex [<em>form</em>])</code>: Apply `macroexpand-1` to _form_ (if supplied) or the current form, and make the result the current form. Then pretty-print the current form.
2.  <code>(fi <em>s</em> [<em>k</em>])</code>: Find the _k_'th subexpression of the current form whose `car` is _s_. (_k_ defaults to 0.) Make that subexpression the current form and pretty-print it.

Suppose you're trying to debug a macro `hair-squared` that expands into something complex containing a subform that is itself a macro form beginning with the symbol `odd-define`. You suspect there is a bug in the subform. You might issue the following commands:

~~~lisp
(ex (hair-squared ...))
(PROGN (DEFUN ...)
         (ODD-DEFINE ZIP (U V W))
         ...)

(fi odd-define)
(ODD-DEFINE ZIP (U V W))

(ex)
(PROGN (DEFUN MAKE-A-ZIP (U V W) ...)
   ...)
~~~

Once again, it is clear that `ex` and `fi` cannot be functions, although they could easily be made into functions if we were willing to type a quote before their arguments. But using "quote" often seems inappropriate in commands. For one thing, having to type it is a nuisance in a context where we are trying to save keystrokes, especially if the argument in question is always quoted. For another, in many cases it just seems inappropriate. If we had a command that took a symbol as one of its arguments and set it to a value, it would just be strange to write <code>(<em>command</em> 'x ...)</code> instead of <code>(<em>command</em> x ...)</code>, because we want to think of the command as a variant of `setq`.

Here is how `ex` and `fi` might be defined:

~~~lisp
(defvar *current-form*)

(defmacro ex (&optional (form nil form-supplied))
  `(progn
     (pprint (setq *current-form*
                   (macroexpand-1
                    ,(cond (form-supplied
                            `',form)
                           (t '*current-form*)))))
     (values)))

(defmacro fi (s &optional (k 0))
  `(progn
     (pprint (setq *current-form*
                   (find-nth-occurrence ',s *current-form* ,k)))
     (values)))
~~~

The `ex` macro expands to a form containing a call to `macroexpand-1`, a built-in function that does one step of macro expansion to a form whose `car` is the name of a macro. (If given some other form, it returns the form unchanged.) `pprint` is a built-in function that pretty-prints its argument. Because we are using `ex` and `fi` at a read-eval-print loop, any value returned by their expansions will be printed. Here the expansion is executed for side effect, so we arrange to return no values at all by having the expansion return `(values)`.

In some Lisp implementations, read-eval-print loops routinely print results using `pprint`. In those implementations we could simplify `ex` and `fi` by having them print nothing, but just return the value of `*current-form*`, which the read-eval-print loop will then print prettily. Use your judgment.

I leave the definition of `find-nth-occurrence` as an exercise. You might also want to define a command that just sets and prints the current form: <code>(cf <em>e</em>)</code>.

One caution: In general, command languages will consist of a mixture of macros and functions, with convenience for their definer (and usually sole user) being the main consideration. If a command seems to "want" to evaluate some of its arguments sometimes, you have to decide whether to define two (or more) versions of it, or just one, a function whose arguments must be quoted to prevent their being evaluated. For the `cf` command mentioned in the previous paragraph, some users might prefer `cf` to be a function, some a macro.


# See also

* [A gentle introduction to Compile-Time Computing — Part 1](https://medium.com/@MartinCracauer/a-gentle-introduction-to-compile-time-computing-part-1-d4d96099cea0)

* [Safely dealing with scientific units of variables at compile time (a gentle introduction to Compile-Time Computing — part 3)](https://medium.com/@MartinCracauer/a-gentle-introduction-to-compile-time-computing-part-3-scientific-units-8e41d8a727ca)

* The following video, from the series ["Little bits of
Lisp"](https://www.youtube.com/user/CBaggers/playlists) by
[cbaggers](https://github.com/cbaggers/), is a two hours long talk on
macros, showing simple to advanced concepts such as compiler macros:
[https://www.youtube.com/watch?v=ygKXeLKhiTI](https://www.youtube.com/watch?v=ygKXeLKhiTI)
It also shows how to manipulate macros (and their expansion) in Emacs.

[![video](http://img.youtube.com/vi/ygKXeLKhiTI/0.jpg)](https://www.youtube.com/watch?v=ygKXeLKhiTI)

* the article "Reader macros in Common Lisp": https://lisper.in/reader-macros
