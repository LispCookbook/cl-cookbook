---
title: Macros
---


<a name="LtohTOCentry-1"></a>

## How Macros Work

The word _macro_ is used generally in computer science to mean a syntactic extension to a programming language. (Note: The name comes from the word "macro-instruction," which was a useful feature of many second-generation assembly languages. A macro-instruction looked like a single instruction, but expanded into a sequence of actual instructions. The basic idea has since been used many times, notably in the C preprocessor. The name "macro" is perhaps not ideal, since it connotes nothing relevant to what it names, but we're stuck with it.) Although many languages have a macro facility, none of them are as powerful as Lisp's. The basic mechanism of Lisp macros is simple, but has subtle complexities, so learning your way around it takes a bit of practice.

A macro is an ordinary piece of Lisp code that operates on _another piece of putative Lisp code,_ translating it into (a version closer to) executable Lisp. That may sound a bit complicated, so let's give a simple example. Suppose you want a version of [`setq`](http://www.lispworks.com/documentation/HyperSpec/Body/s_setq.htm) that sets two variables to the same value. So if you write

~~~lisp
(setq2 x y (+ z 3))
~~~

when `z`=8 both `x` and `y` are set to 11. (I can't think of any use for this, but it's just an example.)

It should be obvious that we can't define `setq2` as a function. If `x`=50 and `y`=_-5_, this function would receive the values 50, _-5_, and 11; it would have no knowledge of what variables were supposed to be set. What we really want to say is, When you (the Lisp system) see <code>(setq2 <i>v<sub>1</sub></i> <i>v<sub>2</sub></i> <i>e</i>)</code>, treat it as equivalent to <code>(progn (setq <i>v<sub>1</sub></i> <i>e</i>) (setq <i>v<sub>2</sub></i> <i>e</i>))</code>. Actually, this isn't quite right, but it will do for now. A macro allows us to do precisely this, by specifying a program for transforming the input pattern <code>(setq2 <i>v<sub>1</sub></i> <i>v<sub>2</sub></i> <i>e</i>)</code> into the output pattern `(progn ...)`.

Here's how we could define the `setq2` macro:

~~~lisp
(defmacro setq2 (v1 v2 e)
  (list 'progn (list 'setq v1 e) (list 'setq v2 e)))
~~~

This is very close to the following function definition:

~~~lisp
(defun setq2F (v1 v2 e)
  (list 'progn (list 'setq v1 e) (list 'setq v2 e)))
~~~

If we evaluated `(setq2F 'x 'y '(+ z 3))`, we would get `(progn (setq x (+ z 3)) (setq y (+ z 3)))`. This is a perfectly ordinary Lisp computation, whose sole point of interest is that its output is a piece of executable Lisp code. What `defmacro` does is create this function implicitly and arrange that whenever an expression of the form `(setq2 x y (+ z 3))` is seen, `setq2F` is called with the pieces of the form as arguments, namely `x`, `y`, and `(+ z 3)`. The resulting piece of code then replaces the call to `setq2`, and execution resumes as if the new piece of code had occurred in the first place. The macro form is said to _expand_ into the new piece of code.

This is all there is to it, except, of course, for the myriad subtle consequences. The main consequence is that _run time for the `setq2` macro is compile time for its context._ That is, suppose the Lisp system is compiling a function, and midway through it finds the expression `(setq2 x y (+ z 3))`. The job of the compiler is, of course, to translate source code into something executable, such as machine language or perhaps byte code. Hence it doesn't execute the source code, but operates on it in various mysterious ways. However, once the compiler sees the `setq2` expression, it must suddenly switch to executing the body of the `setq2` macro. As I said, this is an ordinary piece of Lisp code, which can in principle do anything any other piece of Lisp code can do. That means that when the compiler is running, the entire Lisp (run-time) system must be present.

Novices often make the following sort of mistake. Suppose that the `setq2` macro needs to do some complex transformation on its `e` argument before plugging it into the result. Suppose this transformation can be written as a Lisp procedure `comtran`. The novice will often write:

~~~lisp
(defmacro setq2 (v1 v2 e)
  (let ((e1 (comtran e)))
    (list 'progn (list 'setq v1 e1) (list 'setq v2 e1))))

(defmacro comtran (exp) ...) ;; _Wrong!_
~~~

The mistake is to suppose that once a macro is called, the Lisp system enters a "macro world," so naturally everything in that world must be defined using `defmacro`. This is the wrong picture. The right picture is that `defmacro` enables a step into the _ordinary Lisp world_, but in which the principal object of manipulation is Lisp code. Once that step is taken, one uses ordinary Lisp function definitions:

~~~lisp
(defmacro setq2 (v1 v2 e)
  (let ((e1 (comtran e)))
    (list 'progn (list 'setq v1 e1) (list 'setq v2 e1))))

(defun comtran (exp) ...) ;; _Right!_
~~~

One possible explanation for this mistake may be that in other languages, such as C, invoking a preprocessor macro _does_ get you into a different world; you can't run an arbitrary C program. It might be worth pausing to think about what it might mean to be able to.

Another subtle consequence is that we must spell out how the arguments to the macro get distributed to the hypothetical behind-the-scenes function (called `setq2F` in my example). In most cases, it is easy to do so: In defining a macro, we use all the usual `lambda`-list syntax, such as `&optional`, `&rest`, `&key`, but what gets bound to the formal parameters are pieces of the macro form, not their values (which are mostly unknown, this being compile time for the macro form). So if we defined a macro thus:

~~~lisp
(defmacro foo (x &optional y &key (cxt 'null)) ...)
~~~

then

_If we call it thus ..._     |_The parameters' values are ..._
-----------------------------|-----------------------------------
`(foo a)`                    |`x=a, y=nil, cxt=null`
`(foo (+ a 1) (- y 1))`      |`x=(+ a 1), y=(- y 1), cxt=null`
`(foo a b :cxt (zap zip))`   |`x=a, y=b, cxt=(zap zip)`


Note that the values of the variables are the actual expressions `(+ a 1)` and `(zap zip)`. There is no requirement that these expressions' values be known, or even that they have values. The macro can do anything it likes with them. For instance, here's an even more useless variant of `setq`: <code>(setq-reversible <i>e<sub>1</sub></i> <i>e<sub>2</sub></i> <i>d</i>)</code> behaves like <code>(setq <i>e<sub>1</sub></i> <i>e<sub>2</sub></i>)</code> if <i>d=</i><code>:normal</code>, and behaves like <code>(setq <i>e<sub>2</sub></i> <i>e<sub>1</sub></i>)</code> if _d=_`:backward`. It could be defined thus:

~~~lisp
(defmacro setq-reversible (e1 e2 d)
  (case d
    (:normal (list 'setq e1 e2))
    (:backward (list 'setq e2 e1))
    (t (error ...))))
~~~


<a name="LtohTOCentry-2"></a>

## Backquote

Before taking another step, we need to introduce a piece of Lisp notation that is indispensable to defining macros, even though technically it is quite independent of macros. This is the _backquote facility_. As we saw above, the main job of a macro, when all is said and done, is to define a piece of Lisp code, and that means evaluating expressions such as `(list 'prog (list 'setq ...) ...)`. As these expressions grow in complexity, it becomes hard to read them and write them. What we find ourselves wanting is a notation that provides the skeleton of an expression, with some of the pieces filled in with new expressions. That's what backquote provides. Instead of the the `list` expression given above, one writes

~~~lisp
`(progn (setq ,v1 ,e) (setg ,v2 ,e))
~~~

The backquote (`) character signals that in the expression that follows, every subexpression _not_ preceded by a comma is to be quoted, and every subexpression preceded by a comma is to be evaluated.

That's mostly all there is to backquote. There are just two extra items to point out. First, if you write "`,@e`" instead of "`,e`" then the value of _e_ is _spliced_ into the result. So if `v=(oh boy)`, then `(zap ,@v ,v)` evaluates to `(zap oh boy (oh boy))`. The second occurrence of `v` is replaced by its value. The first is replaced by the elements of its value. If `v` had had value `()`, it would have disappeared entirely: the value of `(zap ,@v ,v)` would have been `(zap ())`, which is the same as `(zap nil)`.

Second, one might wonder what happens if a backquote expression occurs inside another backquote. The answer is that the backquote becomes essentially unreadable and unwriteable; using nested backquote is usually a tedious debugging exercise. The reason, in my not-so-humble opinion, is that backquote is defined wrong. A comma pairs up with the innermost backquote when the default should be that it pairs up with the outermost. But this is not the place for a rant or tutorial; consult your favorite Lisp reference for the exact behavior of nested backquote plus some examples.

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

then backquote may well treat <code>`(low)</code> as if it were `'(low)`; the list will be allocated at load time, and every time the `lambda` is evaluated, that same chunk of storage will be returned. So if we evaluate the expression with `some-list` = `(a 6 15)`, we will get `((a) low 15 15)`, but as a side effect the constant `(low)` will get clobbered to become `(low 15 15)`. If we then evaluate the expression with, say, `some-list` = `(8 oops)`, the result will be `(low 15 15 (oops))`, and now the "constant" that started off as `'(low)` will be `(low 15 15 (oops))`. (Note: The bug exemplified here takes other forms, and has often bit newbies - as well as experienced programmers - in the ass. The general form is that a constant list is produced as the value of something that is later destructively altered. The first line of defense against this bug is never to destructively alter any list. For newbies, this is also the last line of defense. For those of us who imagine we're more sophisticated, the next line of defense is to think very carefully any time you use [`nconc`](http://www.lispworks.com/documentation/HyperSpec/Body/f_nconc.htm) or `mapcan`.)

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

If `sk` is being used as a stack, that is, it's going to be [`pop`](http://www.lispworks.com/documentation/HyperSpec/Body/m_pop.htm)ped in the normal course of things, I would write tt (push x sk). If not, I would write `(setq sk (cons x sk))`.<a name="LtohTOCentry-3"></a>


## Getting Macros Right

I said in [the first section](#LtohTOCentry-1) that my definition of `setq2` wasn't quite right, and now it's time to fix it.

Suppose we write `(setq2 x y (+ x 3))`, when `x`_=8_. Then according to the definition given above, this form will expand into `(progn (setq x (+ x 3)) (setq y (+ x 3)))`, so that `x` will have value 11 and `y` will have value 14\. Chances are that isn't what the macro is expected to do (although you never know). Another problematic case is `(setq2 x y (pop l))`, which causes `l` to be popped twice; again, probably not right.

The solution is to evaluate `e` just once, save it in a temporary variable, and then set `v1` and `v2` to it. To make temporary variables, we use the `gensym` function, which returns a fresh variable guaranteed to appear nowhere else. Here is what the macro should look like:

~~~lisp
(defmacro setq2 (v1 v2 e)
  (let ((tempvar (gensym)))
    `(let ((,tempvar ,e))
       (progn (setq ,v1 ,tempvar)
              (setq ,v2 ,tempvar)))))
~~~

Now `(setq2 x y (+ x 3))` expands to

~~~lisp
(let ((#:g2003 (+ x 3)))
  (progn (setq x #:g2003) (setq y #:g2003)))
~~~

Here `gensym` has returned the symbol `#:g2003`, which prints in this funny way because it won't be recognized by the reader. (Nor is there any need for the reader to recognize it, since it exists only long enough for the code that contains it to be compiled.)

Exercise: Verify that this new version works correctly for the case `(setq2 x y (pop l1))`.

Exercise: Try writing the new version of the macro without using backquote. If you can't do it, you have done the exercise correctly, and learned what backquote is for!

The moral of this section is to think carefully about which expressions in a macro get evaluated and when. Be on the lookout for situations where the same expression gets plugged into the output twice (as `e` was in my original macro design). For complex macros, watch out for cases where the order that expressions are evaluated differs from the order in which they are written. This is sure to trip up some user of the macro - even if you are the only user.<a name="LtohTOCentry-4"></a>

### What Macros are For

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

But macros have a thousand and one legitimate uses. Why write `(lambda (x) ...)` when you can write `(\\ (x) ...)`? Just define `\\` as a macro: <code>(defmacro \\ (&rest l) `(lambda ,@l))</code>.

Many people find `mapcar` and `mapcan` a bit too obscure, especially when used with large `lambda` expressions. Rather than write something like

~~~lisp
(mapcar (lambda (x)
          (let ((y (hairy-fun1 x)) (z (hairy-fun2 x)))
            (dolist (y1 y)
              (dolist (z1 z)
                _... and further meaningless_
                _space-filling nonsense..._
                ))))
        l)
~~~

we might prefer to write

~~~lisp
(for (x :in l)
     (let ((y (hairy-fun1 x)) (z (hairy-fun2 x)))
       (dolist (y1 y)
         (dolist (z1 z)
           _... and further meaningless_
           _space-filling nonsense..._
           ))))
~~~

This macro might be defined thus:

~~~lisp
(defmacro for (listspec exp)
  (cond ((and (= (length listspec) 3)
              (symbolp (car listspec))
              (eq (cadr listspec) ':in))
         `(mapcar (lambda (,(car listspec))
                    ,exp)
                  ,(caddr listspec)))
        (t (error "Ill-formed: %s" `(for ,listspec ,exp)))))
~~~

(This is a simplified version of a macro by Chris Riesbeck.)

It's worth stopping for a second to discuss the role the keyword `:in` plays in this macro. It serves as a sort of "local syntax marker," in that it has no meaning as far as Lisp is concerned, but does serve as a syntactic guidepost for the macro itself. I will refer to these markers as _guide symbols_. (Here its job may seem trivial, but if we generalized the `for` macro to allow multiple list arguments and an implicit `progn` in the body the `:in`s would be crucial in telling us where the arguments stopped and the body began.)

It is not strictly necessary for the guide symbols of a macro to be in the [keyword package](http://www.lispworks.com/documentation/HyperSpec/Body/11_abc.htm), but it is a good idea, for two reasons. First, they highlight to the reader that something idiosyncratic is going on. A form like `(for ((x in (foobar a b 'oof))) (something-hairy x (list x)))` looks a bit wrong already, because of the double parentheses before the `x`. But using "`:in`" makes it more obvious.

Second, notice that I wrote `(eq (cadr listspec) ':in)` in the macro definition to check for the presence of the guide symbol. If I had used `in` instead, I would have had to think about which package _my_ `in` lives in and which package the macro user's `in` lives in. One way to avoid trouble would be to write

~~~lisp
(and (symbolp (cadr listspec))
     (eq (intern (symbol-name (cadr listspec))
                 :keyword)
         ':in))
~~~

Another would be to write

~~~lisp
(and (symbolp (cadr listspec))
     (string= (symbol-name (cadr listspec)) (symbol-name 'in)))
~~~

which neither of which is particularly clear or aesthetic. The keyword package is there to provide a home for symbols whose home is not per se relevant to anything; you might as well use it. (Note: In ANSI Lisp, I could have written `"IN"` instead of `(symbol-name 'in)`, but there are Lisp implementations that do not convert symbols' names to uppercase. Since I think the whole uppercase conversion idea is an embarrassing relic, I try to write code that is portable to those implementations.)

Let's look at another example, both to illustrate a nice macro, and to provide an auxiliary function for some of the discussion below. One often wants to create new symbols in Lisp, and `gensym` is not always adequate for building them. Here is a description of an alternative facility called `build-symbol`:

> <code>(build-symbol [(:package <em>p</em>)] <em>-pieces-</em>)</code> builds a symbol by concatenating the given _pieces_ and interns it as specified by _p_. For each element of _pieces_, if it is a ...
> 
> *   ... string: The string is added to the new symbol's name.
> *   ... symbol: The name of the symbol is added to the new symbol's name.
> *   ... expression of the form <code>(:< <em>e</em>)</code>: _e_ should evaluate to a string, symbol, or number; the characters of the value of _e_ (as printed by `princ`) are concatenated into the new symbol's name.
> *   ... expression of the form <code>(:++ <em>p</em>)</code>: _p_ should be a place expression (i.e., appropriate as the first argument to `setf`), whose value is an integer; the value is incremented by 1, and the new value is concatenated intot he new symbol's name.
> 
> If the `:package` specification is omitted, it defaults to the value of `*package*`. If _p_ is `nil`, the symbol is interned nowhere. Otherwise, it should evaluate to a package designator (usually, a keyword whose name is the same of a package).

For example, `(build-symbol (:< x) "-" (:++ *x-num*))`, when `x` = `foo` and `*x-num*` = 8, sets `*x-num*` to 9 and evaluates to `FOO-9`. If evaluated again, the result will be `FOO-10`, and so forth.

Obviously, `build-symbol` can't be implemented as a function; it has to be a macro. Here is an implementation:

~~~lisp
(defmacro build-symbol (&rest l)
  (let ((p (find-if (lambda (x) (and (consp x) (eq (car x) ':package)))
                    l)))
    (cond (p
           (setq l (remove p l))))
    (let ((pkg (cond ((eq (cadr p) 'nil)
                      nil)
                     (t `(find-package ',(cadr p))))))
      (cond (p
             (cond (pkg
                    `(values (intern ,(symstuff l) ,pkg)))
                   (t
                    `(make-symbol ,(symstuff l)))))
            (t
             `(values (intern ,(symstuff l))))))))

(defun symstuff (l)
  `(concatenate 'string
                ,@(for (x :in l)
                       (cond ((stringp x)
                              `',x)
                             ((atom x)
                              `',(format nil "~a" x))
                             ((eq (car x) ':<)
                              `(format nil "~a" ,(cadr x)))
                             ((eq (car x) ':++)
                              `(format nil "~a" (incf ,(cadr x))))
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

Where the omitted pieces are the same in all similarly named functions. (That is, the "..." in `zep-deactivate` is the same code as the "..." in `zip-deactivate`, and so forth.) Here, for the sake of concreteness, if not plausibility, `zip`, `zap`, and `zep` are behaving like odd little data structures. The functions could be rather large, and it would get tedious keeping them all in synch as they are debugged. An alternative would be to use a macro:

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

If all the uses of this macro are collected in this one place, it might be clearer to make it a local macro using `macrolet`:

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

1.  <code>(ex [<em>form</em>])</code>: Apply `macro-expand1` to _form_ (if supplied) or the current form, and make the result the current form. Then pretty-print the current form.
2.  <code>(fi <em>s</em> [<em>k</em>])</code>: Find the _k_'th subexpression of the current form whose `car` is _s_. (_k_ defaults to 0.) Make that subexpression the current form and pretty-print it.

Suppose you're trying to debug a macro `hair-squared` that expands into something complex containing a subform that is itself a macro form beginning with the symbol `odd-define`. You suspect there is a bug in the subform. You might issue the following commands:

~~~lisp
> (ex (hair-squared ...))
(PROGN (DEFUN ...)
         (ODD-DEFINE ZIP (U V W))
         ...)
> (fi odd-define)
(ODD-DEFINE ZIP (U V W))
> (ex)
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

One caution: In general, command languages will consist of a mixture of macros and functions, with convenience for their definer (and usually sole user) being the main consideration. If a command seems to "want" to evaluate some of its arguments sometimes, you have to decide whether to define two (or more) versions of it, or just one, a function whose arguments must be quoted to prevent their being evaluated. For the `cf` command mentioned in the prevous paragraph, some users might prefer `cf` to be a function, some a macro.
