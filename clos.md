---
title: Fundamentals of CLOS
---


CLOS is the "Common Lisp Object System", arguably one of the most
powerful object systems available in any language.

Some of its features include:

* it is **dynamic**, making it a joy to work with in a Lisp REPL. For
  example, changing a class definition will update the existing
  objects, given certain rules which we have control upon.
* it supports **multiple dispatch** and **multiple inheritance**,
* it is different from most object systems in that class and method
  definitions are not tied together,
* it has excellent **introspection** capabilities,
* it is provided by a **meta-object protocol**, which provides a
  standard interface to the CLOS, and can be used to create new object
  systems.

The functionality belonging to this name was added to the Common Lisp
language between the publication of Steele's first edition of "Common
Lisp, the Language" in 1984 and the formalization of the language as
an ANSI standard ten years later.

This page aims to give a good understanding of how to use CLOS, but
only a brief introduction to the MOP.

To learn the subjects in depth, you will need two books:

- [Object-Oriented Programming in Common Lisp: a Programmer's Guide to CLOS](http://www.communitypicks.com/r/lisp/s/17592186046723-object-oriented-programming-in-common-lisp-a-programmer), by Sonya Keene,
- [the Art of the Metaobject Protocol](http://www.communitypicks.com/r/lisp/s/17592186045709-the-art-of-the-metaobject-protocol), by Gregor Kiczales, Jim des Rivières et al.

But see also

- the introduction in [Practical Common Lisp](http://www.gigamonkeys.com/book/object-reorientation-generic-functions.html) (online), by Peter Seibel.
-  [Common Lisp, the Language](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node260.html#SECTION003200000000000000000)
- and for reference, the complete [CLOS-MOP specifications](https://clos-mop.hexstreamsoft.com/).


#  Classes and instances

## Diving in

Let's dive in with an example showing class definition, creation of
objects, slot access, methods specialized for a given class, and
inheritance.

~~~lisp
(defclass person ()
  ((name
    :initarg :name
    :accessor name)
   (lisper
    :initform nil
    :accessor lisper)))

;; => #<STANDARD-CLASS PERSON>

(defvar p1 (make-instance 'person :name "me" ))
;;                                 ^^^^ initarg
;; => #<PERSON {1006234593}>

(name p1)
;;^^^ accessor
;; => "me"

(lisper p1)
;; => nil
;;    ^^ initform (slot unbound by default)

(setf (lisper p1) t)


(defclass child (person)
  ())

(defclass child (person)
  ((can-walk-p
     :accessor can-walk-p
     :initform t)))
;; #<STANDARD-CLASS CHILD>

(can-walk-p (make-instance 'child))
;; T
~~~

## Defining classes (defclass)

The macro used for defining new data types in CLOS is `defclass`.

We used it like this:

~~~lisp
(defclass person ()
  ((name
    :initarg :name
    :accessor name)
   (lisper
    :initform nil
    :accessor lisper)))
~~~

This gives us a CLOS type (or class) called `person` and two slots,
named `name` and `lisper`.

~~~lisp
(class-of p1)
#<STANDARD-CLASS PERSON>

(type-of p1)
PERSON
~~~

The general form of `defclass` is:

```
(defclass <class-name> (list of super classes)
  ((slot-1
     :slot-option slot-argument)
   (slot-2, etc))
  (:optional-class-option
   :another-optional-class-option))
```

So, our `person` class doesn't explicitly inherit from another class
(it gets the empty parentheses `()`). However it still inherits by default from
the class `t` and from `standard-object`. See below under
"inheritance".

We could write a minimal class definition without slots options like this:

~~~lisp
(defclass point ()
  (x y z))
~~~

or even without slots specifiers: `(defclass point () ())`.

## Creating objects (make-instance)

We create instances of a class with `make-instance`:

~~~lisp
(defvar p1 (make-instance 'person :name "me" ))
~~~

It is generally good practice to define a constructor:

~~~lisp
(defun make-person (name &key lisper)
  (make-instance 'person :name name :lisper lisper))
~~~

This has the direct advantage that you can control the required
arguments. You should now export the constructor from your package and
not the class itself.

## Slots

### A function that always works (slot-value)

The function to access any slot anytime is `(slot-value <object> <slot-name>)`.

Given our `point` class above, which didn't define any slot accessors:


```lisp
(defvar pt (make-instance 'point))

(inspect pt)
The object is a STANDARD-OBJECT of type POINT.
0. X: "unbound"
1. Y: "unbound"
2. Z: "unbound"
```

We got an object of type `POINT`, but **slots are unbound by
default**: trying to access them will raise an `UNBOUND-SLOT`
condition:

~~~lisp
(slot-value pt 'x) ;; => condition: the slot is unbound
~~~


`slot-value` is `setf`-able:

~~~lisp
(setf (slot-value pt 'x) 1)
(slot-value pt 'x) ;; => 1
~~~

### Initial and default values (initarg, initform)

- `:initarg :foo` is the keyword we can pass to `make-instance` to
  give a value to this slot:

~~~lisp
(make-instance 'person :name "me")
~~~

(again: slots are unbound by default)

- `:initform <val>` is the *default value* in case we didn't specify
 an initarg.  This form is evaluated each time it's needed, in the
 lexical environment of the `defclass`.

Sometimes we see the following trick to clearly require a slot:

~~~lisp
(defclass foo ()
    ((a
      :initarg :a
      :initform (error "you didn't supply an initial value for slot a"))))
;; #<STANDARD-CLASS FOO>

(make-instance 'foo) ;; => enters the debugger.
~~~


### Getters and setters (accessor, reader, writer)

- `:accessor foo`: an accessor is both a **getter** and a
  **setter**. Its argument is a name that will become a **generic
  function**.

~~~lisp
(name p1) ;; => "me"

(type-of #'name)
STANDARD-GENERIC-FUNCTION
~~~

- `:reader` and `:writer` do what you expect. Only the `:writer` is `setf`-able.

If you don't specify any of these, you can still use `slot-value`.

You can give a slot more than one `:accessor`, `:reader` or `:initarg`.


We introduce two macros to make the access to slots shorter in some situations:

1- `with-slots` allows to abbreviate several calls to slot-value. The
first argument is a list of slot names. The second argument evaluates
to a CLOS instance. This is followed by optional declarations and an
implicit `progn`. Lexically during the evaluation of the body, an
access to any of these names as a variable is equivalent to accessing
the corresponding slot of the instance with `slot-value`.


~~~lisp
(with-slots (name lisper)
    c1
  (format t "got ~a, ~a~&" name lisper))
~~~

or

~~~lisp
(with-slots ((n name)
             (l lisper))
    c1
  (format t "got ~a, ~a~&" n l))
~~~

2- `with-accessors` is equivalent, but instead of a list of slots it
takes a list of accessor functions. Any reference to the variable
inside the macro is equivalent to a call to the accessor function.

~~~lisp
(with-accessors ((name        name)
                  ^^variable  ^^accessor
                 (lisper lisper))
            p1
          (format t "name: ~a, lisper: ~a" name lisper))
~~~

### Class VS instance slots

`:allocation` specifies whether this slot is *local* or *shared*.

* a slot is *local* by default, that means it can be different for each instance of the class. In that case `:allocation` equals `:instance`.

* a *shared* slot will always be equal for all instances of the
    class. We set it with `:allocation :class`.

In the following example, note how changing the value of the class
slot `species` of `p2` affects all instances of the
class (whether or not those instances exist yet).

~~~lisp
(defclass person ()
  ((name :initarg :name :accessor name)
   (species
      :initform 'homo-sapiens
      :accessor species
      :allocation :class)))

;; Note that the slot "lisper" was removed in existing instances.
(inspect p1)
;; The object is a STANDARD-OBJECT of type PERSON.
;; 0. NAME: "me"
;; 1. SPECIES: HOMO-SAPIENS
;; > q

(defvar p2 (make-instance 'person))

(species p1)
(species p2)
;; HOMO-SAPIENS

(setf (species p2) 'homo-numericus)
;; HOMO-NUMERICUS

(species p1)
;; HOMO-NUMERICUS

(species (make-instance 'person))
;; HOMO-NUMERICUS

(let ((temp (make-instance 'person)))
    (setf (species temp) 'homo-lisper))
;; HOMO-LISPER
(species (make-instance 'person))
;; HOMO-LISPER
~~~

### Slot documentation

Each slot accepts one `:documentation` option.

### Slot type

The `:type` slot option may not do the job you expect it does. If you
are new to the CLOS, we suggest you skip this section and use your own
constructors to manually check slot types.

Indeed, whether slot types are being checked or not is undefined. See the [Hyperspec](http://www.lispworks.com/documentation/HyperSpec/Body/m_defcla.htm#defclass).

Few implementations will do it. Clozure CL does it, SBCL does it since
its version 1.5.9 (November, 2019) or when safety is high (`(declaim
(optimise safety))`).

To do it otherwise, see [this Stack-Overflow answer](https://stackoverflow.com/questions/51723992/how-to-force-slots-type-to-be-checked-during-make-instance), and see also [quid-pro-quo](https://github.com/sellout/quid-pro-quo), a contract programming library.


## find-class, class-name, class-of

~~~lisp
(find-class 'point)
;; #<STANDARD-CLASS POINT 275B78DC>

(class-name (find-class 'point))
;; POINT

(class-of my-point)
;; #<STANDARD-CLASS POINT 275B78DC>

(typep my-point (class-of my-point))
;; T
~~~

CLOS classes are also instances of a CLOS class, and we can find out
what that class is, as in the example below:

~~~lisp
(class-of (class-of my-point))
;; #<STANDARD-CLASS STANDARD-CLASS 20306534>
~~~

<u>Note</u>: this is your first introduction to the MOP. You don't need that to get started !

The object `my-point` is an instance of the class named `point`, and the
class named `point` is itself an instance of the class named
`standard-class`. We say that the class named `standard-class` is
the *metaclass* (i.e. the class of the class) of
`my-point`. We can make good uses of metaclasses, as we'll see later.



## Subclasses and inheritance

As illustrated above, `child` is a subclass of `person`.

All objects inherit from the class `standard-object` and `t`.

Every child instance is also an instance of `person`.

~~~lisp
(type-of c1)
;; CHILD

(subtypep (type-of c1) 'person)
;; T

(ql:quickload "closer-mop")
;; ...

(closer-mop:subclassp (class-of c1) 'person)
;; T
~~~

The [closer-mop](https://github.com/pcostanza/closer-mop) library is *the*
portable way to do CLOS/MOP operations.


A subclass inherits all of its parents slots, and it can override any
of their slot options. Common Lisp makes this process dynamic, great
for REPL session, and we can even control parts of it (like, do
something when a given slot is removed/updated/added, etc).

The **class precedence list** of a `child` is thus:

    child <- person <-- standard-object <- t

Which we can get with:

~~~lisp
(closer-mop:class-precedence-list (class-of c1))
;; (#<standard-class child>
;;  #<standard-class person>
;;  #<standard-class standard-object>
;;  #<sb-pcl::slot-class sb-pcl::slot-object>
;;  #<sb-pcl:system-class t>)
~~~

However, the **direct superclass** of a `child` is only:

~~~lisp
(closer-mop:class-direct-superclasses (class-of c1))
;; (#<standard-class person>)
~~~

We can further inspect our classes with
`class-direct-[subclasses, slots, default-initargs]` and many more functions.

How slots are combined follows some rules:

- `:accessor` and `:reader` are combined by the **union** of accessors
   and readers from all the inherited slots.

- `:initarg`: the **union** of initialization arguments from all the
  inherited slots.

- `:initform`: we get **the most specific** default initial value
  form, i.e. the first `:initform` for that slot in the precedence
  list.

- `:allocation` is not inherited. It is controlled solely by the class
  being defined and defaults to `:instance`.


Last but not least, be warned that inheritance is fairly easy to
misuse, and multiple inheritance is multiply so, so please take a
little care. Ask yourself whether `foo` really wants to inherit from
`bar`, or whether instances of `foo` want a slot containing a `bar`. A
good general guide is that if `foo` and `bar` are "same sort of thing"
then it's correct to mix them together by inheritance, but if they're
really separate concepts then you should use slots to keep them apart.


## Multiple inheritance

CLOS supports multiple inheritance.


~~~lisp
(defclass baby (child person)
  ())
~~~

The first class on the list of parent classes is the most specific
one, `child`'s slots will take precedence over the `person`'s. Note
that both `child` and `person` have to be defined prior to defining
`baby` in this example.


## Redefining and changing a class

This section briefly covers two topics:

- redefinition of an existing class, which you might already have done
  by following our code snippets, and what we do naturally during
  development, and
- changing an instance of one class into an instance of another,
  a powerful feature of CLOS that you'll probably won't use very often.

We'll gloss over the details. Suffice it to say that everything's
configurable by implementing methods exposed by the MOP.

To redefine a class, simply evaluate a new `defclass` form. This then
takes the place of the old definition, the existing class object is
updated, and **all instances of the class** (and, recursively, its
subclasses) **are lazily updated to reflect the new definition**. You don't
have to recompile anything other than the new `defclass`, nor to
invalidate any of your objects. Think about it for a second: this is awesome !

For example, with our `person` class:

~~~lisp
(defclass person ()
  ((name
    :initarg :name
    :accessor name)
   (lisper
    :initform nil
    :accessor lisper)))

(setf p1 (make-instance 'person :name "me" ))
~~~

Changing, adding, removing slots,...

~~~lisp
(lisper p1)
;; NIL

(defclass person ()
  ((name
    :initarg :name
    :accessor name)
   (lisper
    :initform t        ;; <-- from nil to t
    :accessor lisper)))

(lisper p1)
;; NIL (of course!)

(lisper (make-instance 'person :name "You"))
;; T

(defclass person ()
  ((name
    :initarg :name
    :accessor name)
   (lisper
    :initform nil
    :accessor lisper)
   (age
    :initarg :arg
    :initform 18
    :accessor age)))

(age p1)
;; => slot unbound error. This is different from "slot missing":

(slot-value p1 'bwarf)
;; => "the slot bwarf is missing from the object #<person…>"

(setf (age p1) 30)
(age p1) ;; => 30

(defclass person ()
  ((name
    :initarg :name
    :accessor name)))

(slot-value p1 'lisper) ;; => slot lisper is missing.
(lisper p1) ;; => there is no applicable method for the generic function lisper when called with arguments #(lisper).
~~~


To change the class of an instance, use `change-class`:

~~~lisp
(change-class p1 'child)
;; we can also set slots of the new class:
(change-class p1 'child :can-walk-p nil)

(class-of p1)
;; #<STANDARD-CLASS CHILD>

(can-walk-p p1)
;; T
~~~

In the above example, I became a `child`, and I inherited the `can-walk-p` slot, which is true by default.


## Pretty printing

Every time we printed an object so far we got an output like

    #<PERSON {1006234593}>

which doesn't say much.

What if we want to show more information ? Something like

    #<PERSON me lisper: t>

Pretty printing is done by specializing the generic `print-object` method for this class:

~~~lisp
(defmethod print-object ((obj person) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((name name)
                         (lisper lisper))
            obj
          (format stream "~a, lisper: ~a" name lisper))))
~~~

It gives:

~~~lisp
p1
;; #<PERSON me, lisper: T>
~~~

`print-unreadable-object` prints the `#<...>`, that says to the reader
that this object can not be read back in. Its `:type t` argument asks
to print the object-type prefix, that is, `PERSON`. Without it, we get
`#<me, lisper: T>`.

We used the `with-accessors` macro, but of course for simple cases this is enough:

~~~lisp
(defmethod print-object ((obj person) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a, lisper: ~a" (name obj) (lisper obj))))
~~~

Caution: trying to access a slot that is not bound by default will
lead to an error. Use `slot-boundp`.


For reference, the following reproduces the default behaviour:

~~~lisp
(defmethod print-object ((obj person) stream)
  (print-unreadable-object (obj stream :type t :identity t)))
~~~

Here, `:identity` to `t` prints the `{1006234593}` address.

## Classes of traditional lisp types

Where we approach that we don't need CLOS objects to use CLOS.

Generously, the functions introduced in the last section also work on
lisp objects which are <u>not</u> CLOS instances:

~~~lisp
(find-class 'symbol)
;; #<BUILT-IN-CLASS SYMBOL>
(class-name *)
;; SYMBOL
(eq ** (class-of 'symbol))
;; T
(class-of ***)
;; #<STANDARD-CLASS BUILT-IN-CLASS>
~~~

We see here that symbols are instances of the system class
`symbol`. This is one of 75 cases in which the language requires a
class to exist with the same name as the corresponding lisp
type. Many of these cases are concerned with CLOS itself (for
example, the correspondence between the type `standard-class` and
the CLOS class of that name) or with the condition system (which
might or might not be built using CLOS classes in any given
implementation). However, 33 correspondences remain relating to
"traditional" lisp types:


|`array`|`hash-table`|`readtable`|
|`bit-vector`|`integer`|`real`|
|`broadcast-stream`|`list`|`sequence`|
|`character`|`logical-pathname`|`stream`|
|`complex`|`null`|`string`|
|`concatenated-stream`|`number`|`string-stream`|
|`cons`|`package`|`symbol`|
|`echo-stream`|`pathname`|`synonym-stream`|
|`file-stream`|`random-state`|`t`|
|`float`|`ratio`|`two-way-stream`|
|`function`|`rational`|`vector`|


Note that not all "traditional" lisp types are included in this
list. (Consider: `atom`, `fixnum`, `short-float`, and any type not
denoted by a symbol.)


The presence of `t` is interesting. Just as every lisp
object is of type `t`, every lisp object is also a member
of the class named `t`. This is a simple example of
membership of more then one class at a time, and it brings into
question the issue of *inheritance*, which we will consider
in some detail later.

~~~lisp
(find-class t)
;; #<BUILT-IN-CLASS T 20305AEC>
~~~

In addition to classes corresponding to lisp types, there is also a
    CLOS class for every structure type you define:

~~~lisp
(defstruct foo)
FOO

(class-of (make-foo))
;; #<STRUCTURE-CLASS FOO 21DE8714>
~~~

The metaclass of a `structure-object` is the class
    `structure-class`. It is implementation-dependent whether
    the metaclass of a "traditional" lisp object is
    `standard-class`, `structure-class`, or
    `built-in-class`. Restrictions:

|`built-in-class`| May not use `make-instance`, may not use `slot-value`, may not use `defclass` to modify, may not create subclasses.|
|`structure-class`| May not use `make-instance`, might work with `slot-value` (implementation-dependent). Use `defstruct` to subclass application structure types. Consequences of modifying an existing `structure-class` are undefined: full recompilation may be necessary.|
|`standard-class`|None of these restrictions.|


## Introspection

we already saw some introspection functions.

Your best option is to discover the
[closer-mop](https://github.com/pcostanza/closer-mop) library and to
keep the [CLOS & MOP specifications](https://clos-mop.hexstreamsoft.com/) at
hand.

More functions:

```
closer-mop:class-default-initargs
closer-mop:class-direct-default-initargs
closer-mop:class-direct-slots
closer-mop:class-direct-subclasses
closer-mop:class-direct-superclasses
closer-mop:class-precedence-list
closer-mop:class-slots
closer-mop:classp
closer-mop:extract-lambda-list
closer-mop:extract-specializer-names
closer-mop:generic-function-argument-precedence-order
closer-mop:generic-function-declarations
closer-mop:generic-function-lambda-list
closer-mop:generic-function-method-class
closer-mop:generic-function-method-combination
closer-mop:generic-function-methods
closer-mop:generic-function-name
closer-mop:method-combination
closer-mop:method-function
closer-mop:method-generic-function
closer-mop:method-lambda-list
closer-mop:method-specializers
closer-mop:slot-definition
closer-mop:slot-definition-allocation
closer-mop:slot-definition-initargs
closer-mop:slot-definition-initform
closer-mop:slot-definition-initfunction
closer-mop:slot-definition-location
closer-mop:slot-definition-name
closer-mop:slot-definition-readers
closer-mop:slot-definition-type
closer-mop:slot-definition-writers
closer-mop:specializer-direct-generic-functions
closer-mop:specializer-direct-methods
closer-mop:standard-accessor-method
```


## See also

### defclass/std: write shorter classes

The library [defclass/std](https://github.com/EuAndreh/defclass-std)
provides a macro to write shorter `defclass` forms.

By default, it adds an accessor, an initarg and an initform to `nil` to your slots definition:

This:

~~~lisp
(defclass/std example ()
  ((slot1 slot2 slot3)))
~~~

expands to:

~~~lisp
(defclass example ()
  ((slot1
    :accessor slot1
    :initarg :slot1
    :initform nil)
   (slot2
     :accessor slot2
     :initarg :slot2
     :initform nil)
   (slot3
     :accessor slot3
     :initarg :slot3
     :initform nil)))
~~~

It does much more and it is very flexible, however it is seldom used
by the Common Lisp community: use at your own risks©.


# Methods

## Diving in
Recalling our `person` and `child` classes from the beginning:

~~~lisp
(defclass person ()
  ((name
    :initarg :name
    :accessor name)))
;; => #<STANDARD-CLASS PERSON>

(defclass child (person)
  ())
;; #<STANDARD-CLASS CHILD>

(setf p1 (make-instance 'person :name "me"))
(setf c1 (make-instance 'child :name "Alice"))
~~~

Below we create methods, we specialize them, we use method combination
(before, after, around), and qualifiers.

~~~lisp
(defmethod greet (obj)
  (format t "Are you a person ? You are a ~a.~&" (type-of obj)))
;; style-warning: Implicitly creating new generic function common-lisp-user::greet.
;; #<STANDARD-METHOD GREET (t) {1008EE4603}>

(greet :anything)
;; Are you a person ? You are a KEYWORD.
;; NIL
(greet p1)
;; Are you a person ? You are a PERSON.

(defgeneric greet (obj)
  (:documentation "say hello"))
;; STYLE-WARNING: redefining COMMON-LISP-USER::GREET in DEFGENERIC
;; #<STANDARD-GENERIC-FUNCTION GREET (2)>

(defmethod greet ((obj person))
  (format t "Hello ~a !~&" (name obj)))
;; #<STANDARD-METHOD GREET (PERSON) {1007C26743}>

(greet p1) ;; => "Hello me !"
(greet c1) ;; => "Hello Alice !"

(defmethod greet ((obj child))
  (format t "ur so cute~&"))
;; #<STANDARD-METHOD GREET (CHILD) {1008F3C1C3}>

(greet p1) ;; => "Hello me !"
(greet c1) ;; => "ur so cute"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method combination: before, after, around.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod greet :before ((obj person))
  (format t "-- before person~&"))
#<STANDARD-METHOD GREET :BEFORE (PERSON) {100C94A013}>

(greet p1)
;; -- before person
;; Hello me

(defmethod greet :before ((obj child))
  (format t "-- before child~&"))
;; #<STANDARD-METHOD GREET :BEFORE (CHILD) {100AD32A43}>
(greet c1)
;; -- before child
;; -- before person
;; ur so cute

(defmethod greet :after ((obj person))
  (format t "-- after person~&"))
;; #<STANDARD-METHOD GREET :AFTER (PERSON) {100CA2E1A3}>
(greet p1)
;; -- before person
;; Hello me
;; -- after person

(defmethod greet :after ((obj child))
  (format t "-- after child~&"))
;; #<STANDARD-METHOD GREET :AFTER (CHILD) {10075B71F3}>
(greet c1)
;; -- before child
;; -- before person
;; ur so cute
;; -- after person
;; -- after child

(defmethod greet :around ((obj child))
  (format t "Hello my dear~&"))
;; #<STANDARD-METHOD GREET :AROUND (CHILD) {10076658E3}>
(greet c1) ;; Hello my dear


;; call-next-method

(defmethod greet :around ((obj child))
  (format t "Hello my dear~&")
  (when (next-method-p)
    (call-next-method)))
;; #<standard-method greet :around (child) {100AF76863}>

(greet c1)
;; Hello my dear
;; -- before child
;; -- before person
;; ur so cute
;; -- after person
;; -- after child

;;;;;;;;;;;;;;;;;
;; Adding in &key
;;;;;;;;;;;;;;;;;

;; In order to add "&key" to our generic method, we need to remove its definition first.
(fmakunbound 'greet)  ;; with Slime: C-c C-u (slime-undefine-function)
(defmethod greet ((obj person) &key talkative)
  (format t "Hello ~a~&" (name obj))
  (when talkative
    (format t "blah")))

(defgeneric greet (obj &key &allow-other-keys)
  (:documentation "say hi"))

(defmethod greet (obj &key &allow-other-keys)
  (format t "Are you a person ? You are a ~a.~&" (type-of obj)))

(defmethod greet ((obj person) &key talkative &allow-other-keys)
  (format t "Hello ~a !~&" (name obj))
  (when talkative
    (format t "blah")))

(greet p1 :talkative t) ;; ok
(greet p1 :foo t) ;; still ok


;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric greet (obj)
  (:documentation "say hello")
  (:method (obj)
    (format t "Are you a person ? You are a ~a~&." (type-of obj)))
  (:method ((obj person))
    (format t "Hello ~a !~&" (name obj)))
  (:method ((obj child))
    (format t "ur so cute~&")))

;;;;;;;;;;;;;;;;
;;; Specializers
;;;;;;;;;;;;;;;;

(defgeneric feed (obj meal-type)
  (:method (obj meal-type)
    (declare (ignorable meal-type))
    (format t "eating~&")))

(defmethod feed (obj (meal-type (eql :dessert)))
    (declare (ignorable meal-type))
    (format t "mmh, dessert !~&"))

(feed c1 :dessert)
;; mmh, dessert !

(defmethod feed ((obj child) (meal-type (eql :soup)))
    (declare (ignorable meal-type))
    (format t "bwark~&"))

(feed p1 :soup)
;; eating
(feed c1 :soup)
;; bwark
~~~


## Generic functions (defgeneric, defmethod)

A `generic function` is a lisp function which is associated
with a set of methods and dispatches them when it's invoked. All
the methods with the same function name belong to the same generic
function.

The `defmethod` form is similar to a `defun`. It associates a body of
code with a function name, but that body may only be executed if the
types of the arguments match the pattern declared by the lambda list.

They can have optional, keyword and `&rest` arguments.

The `defgeneric` form defines the generic function. If we write a
`defmethod` without a corresponding `defgeneric`, a generic function
is automatically created (see examples).

It is generally a good idea to write the `defgeneric`s. We can add a
default implementation and even some documentation.

~~~lisp
(defgeneric greet (obj)
  (:documentation "says hi")
  (:method (obj)
    (format t "Hi")))
~~~

The required parameters in the method's lambda list may take one of
the following three forms:

1- a simple variable:

~~~lisp
(defmethod greet (foo)
  ...)
~~~

This method can take any argument, it is always applicable.

The variable `foo` is bound to the corresponding argument value, as
usual.

2- a variable and a **specializer**, as in:

~~~lisp
(defmethod greet ((foo person))
  ...)
~~~

In this case, the variable `foo` is bound to the corresponding
argument only if that argument is of specializer class `person` *or a subclass*,
like `child` (indeed, a "child" is also a "person").

If any argument fails to match its
specializer then the method is not *applicable* and it cannot be
executed with those arguments.We'll get an error message like
"there is no applicable method for the generic function xxx when
called with arguments yyy".

**Only required parameters can be specialized**. We can't specialize on optional `&key` arguments.


3- a variable and an **eql specializer**

~~~lisp
(defmethod feed ((obj child) (meal-type (eql :soup)))
    (declare (ignorable meal-type))
    (format t "bwark~&"))

(feed c1 :soup)
;; "bwark"
~~~

In place of a simple symbol (`:soup`), the eql specializer can be any
lisp form. It is evaluated at the same time of the defmethod.

You can define any number of methods with the same function name but
with different specializers, as long as the form of the lambda list is
*congruent* with the shape of the generic function. The system chooses
the most *specific* applicable method and executes its body. The most
specific method is the one whose specializers are nearest to the head
of the `class-precedence-list` of the argument (classes on the left of
the lambda list are more specific). A method with specializers is more
specific to one without any.


**Notes:**

-   It is an error to define a method with the same function name as
    an ordinary function. If you really want to do that, use the
    shadowing mechanism.

-   To add or remove `keys` or `rest` arguments to an existing generic
    method's lambda list, you will need to delete its declaration with
    `fmakunbound` (or `C-c C-u` (slime-undefine-function) with the
    cursor on the function in Slime) and start again. Otherwise,
    you'll see:

```
attempt to add the method
  #<STANDARD-METHOD NIL (#<STANDARD-CLASS CHILD>) {1009504233}>
to the generic function
  #<STANDARD-GENERIC-FUNCTION GREET (2)>;
but the method and generic function differ in whether they accept
&REST or &KEY arguments.
```

-   Methods can be redefined (exactly as for ordinary functions).

-   The order in which methods are defined is irrelevant, although
    any classes on which they specialize must already exist.

-   An unspecialized argument is more or less equivalent to being
    specialized on the class `t`. The only difference is that
    all specialized arguments are implicitly taken to be "referred to" (in
    the sense of `declare ignore`.)

-   Each `defmethod` form generates (and returns) a CLOS
    instance, of class `standard-method`.

- An `eql` specializer won't work as is with strings. Indeed, strings
need `equal` or `equalp` to be compared. But, we can assign our string
to a variable and use the variable both in the `eql` specializer and
for the function call.

- All the methods with the same function name belong to the same generic function.

- All slot accessors and readers defined by `defclass` are methods. They can override or be overridden by other methods on the same generic function.


See more about [defmethod on the CLHS](http://www.lispworks.com/documentation/lw70/CLHS/Body/m_defmet.htm).

## Multimethods

Multimethods explicitly specialize more than one of the generic
function's required parameters.

They don't belong to a particular class. Meaning, we don't have to
decide on the class that would be best to host this method, as we might
have to in other languages.

~~~lisp
(defgeneric hug (a b)
   (:documentation "Hug between two persons."))
;; #<STANDARD-GENERIC-FUNCTION HUG (0)>

(defmethod hug ((a person) (b person))
  :person-person-hug)

(defmethod hug ((a person) (b child))
  :person-child-hug)
~~~

Read more on [Practical Common Lisp](http://www.gigamonkeys.com/book/object-reorientation-generic-functions.html#multimethods).

## Controlling setters (setf-ing methods)

In Lisp, we can define `setf` counterparts of functions or methods. We
might want this to have more control on how to update an object.

~~~lisp
(defmethod (setf name) (new-val (obj person))
  (if (equalp new-val "james bond")
    (format t "Dude that's not possible.~&")
    (setf (slot-value obj 'name) new-val)))

(setf (name p1) "james bond") ;; -> no rename
~~~

If you know Python, this behaviour is provided by the `@property` decorator.


## Dispatch mechanism and next methods


When a generic function is invoked, the application cannot directly invoke a method. The dispatch mechanism proceeds as follows:

1.  compute the list of applicable methods
2.  if no method is applicable then signal an error
3.  sort the applicable methods in order of specificity
4.  invoke the most specific method.

Our `greet` generic function has three applicable methods:

~~~lisp
(closer-mop:generic-function-methods #'greet)
(#<STANDARD-METHOD GREET (CHILD) {10098406A3}>
 #<STANDARD-METHOD GREET (PERSON) {1009008EC3}>
 #<STANDARD-METHOD GREET (T) {1008E6EBB3}>)
~~~

During the execution of a method, the remaining applicable methods
are still accessible, via the *local function*
`call-next-method`. This function has lexical scope within
the body of a method but indefinite extent. It invokes the next most
specific method, and returns whatever value that method returned. It
can be called with either:

*   no arguments, in which case the *next method* will
    receive exactly the same arguments as this method did, or

*   explicit arguments, in which case it is required that the
    sorted set of methods applicable to the new arguments must be the same
    as that computed when the generic function was first called.

For example:

~~~lisp
(defmethod greet ((obj child))
  (format t "ur so cute~&")
  (when (next-method-p)
    (call-next-method)))
;; STYLE-WARNING: REDEFINING GREET (#<STANDARD-CLASS CHILD>) in DEFMETHOD
;; #<STANDARD-METHOD GREET (child) {1003D3DB43}>

(greet c1)
;; ur so cute
;; Hello Alice !
~~~

Calling `call-next-method` when there is no next method
signals an error. You can find out whether a next method exists by
calling the local function `next-method-p` (which also has
has lexical scope and indefinite extent).

Note finally that the body of every method establishes a block with the same name as the method’s generic function. If you `return-from` that name you are exiting the current method, not the call to the enclosing generic function.


## Method qualifiers (before, after, around)

In our "Diving in" examples, we saw some use of the `:before`, `:after` and `:around` *qualifiers*:

- `(defmethod foo :before (obj) (...))`
- `(defmethod foo :after (obj) (...))`
- `(defmethod foo :around (obj) (...))`

By default, in the *standard method combination* framework provided by
CLOS, we can only use one of those three qualifiers, and the flow of control is as follows:

- a **before-method** is called, well, before the applicable primary
  method. If they are many before-methods, **all** are called. The
  most specific before-method is called first (child before person).
- the most specific applicable **primary method** (a method without
  qualifiers) is called (only one).
- all applicable **after-methods** are called. The most specific one is
  called *last* (after-method of person, then after-method of child).

**The generic function returns the value of the primary method**. Any
values of the before or after methods are ignored. They are used for
their side effects.

And then we have **around-methods**. They are wrappers around the core
mechanism we just described. They can be useful to catch return values
or to set up an environment around the primary method (set up a catch,
a lock, timing an execution,…).

If the dispatch mechanism finds an around-method, it calls it and
returns its result. If the around-method has a `call-next-method`, it
calls the next most applicable around-method. It is only when we reach
the primary method that we start calling the before and after-methods.

Thus, the full dispatch mechanism for generic functions is as follows:

1.  compute the applicable methods, and partition them into
    separate lists according to their qualifier;
2.  if there is no applicable primary method then signal an
    error;
3.  sort each of the lists into order of specificity;
4.  execute the most specific `:around` method and
    return whatever that returns;
5.  if an `:around` method invokes
    `call-next-method`, execute the next most specific
    `:around` method;
6.  if there were no `:around` methods in the first
    place, or if an `:around` method invokes
    `call-next-method` but there are no further
    `:around` methods to call, then proceed as follows:

    a.  run all the `:before` methods, in order,
            ignoring any return values and not permitting calls to
            `call-next-method` or
            `next-method-p`;

    b.  execute the most specific primary method and return
            whatever that returns;

    c.  if a primary method invokes `call-next-method`,
            execute the next most specific primary method;

    d.  if a primary method invokes `call-next-method`
            but there are no further primary methods to call then signal an
            error;

    e.  after the primary method(s) have completed, run all the
            `:after` methods, in **<u>reverse</u>**
            order, ignoring any return values and not permitting calls to
            `call-next-method` or
            `next-method-p`.

Think of it as an onion, with all the `:around`
    methods in the outermost layer, `:before` and
    `:after` methods in the middle layer, and primary methods
    on the inside.


## Other method combinations

The default method combination type we just saw is named `standard`,
but other method combination types are available, and no need to say
that you can define your own.

The built-in types are:

    progn + list nconc and max or append min

You notice that these types are named after a lisp operator. Indeed,
what they do is they define a framework that combines the applicable
primary methods inside a call to the lisp operator of that name. For
example, using the `progn` combination type is equivalent to calling **all**
the primary methods one after the other:

~~~lisp
(progn
  (method-1 args)
  (method-2 args)
  (method-3 args))
~~~

Here, unlike the standard mechanism, all the primary methods
applicable for a given object are called, the most specific
first.

To change the combination type, we set the `:method-combination`
option of `defgeneric` and we use it as the methods' qualifier:

~~~lisp
(defgeneric foo (obj)
  (:method-combination progn))

(defmethod foo progn ((obj obj))
   (...))
~~~

An example with **progn**:

~~~lisp
(defgeneric dishes (obj)
   (:method-combination progn)
   (:method progn (obj)
     (format t "- clean and dry.~&"))
   (:method progn ((obj person))
     (format t "- bring a person's dishes~&"))
   (:method progn ((obj child))
     (format t "- bring the baby dishes~&")))
;; #<STANDARD-GENERIC-FUNCTION DISHES (3)>

(dishes c1)
;; - bring the baby dishes
;; - bring a person's dishes
;; - clean and dry.

(greet c1)
;; ur so cute  --> only the most applicable method was called.
~~~

Similarly, using the `list` type is equivalent to returning the list
of the values of the methods.

~~~lisp
(list
  (method-1 args)
  (method-2 args)
  (method-3 args))
~~~

~~~lisp
(defgeneric tidy (obj)
  (:method-combination list)
  (:method list (obj)
    :foo)
  (:method list ((obj person))
    :books)
  (:method list ((obj child))
    :toys))
;; #<STANDARD-GENERIC-FUNCTION TIDY (3)>

(tidy c1)
;; (:toys :books :foo)
~~~

**Around methods** are accepted:

~~~lisp
(defmethod tidy :around (obj)
   (let ((res (call-next-method)))
     (format t "I'm going to clean up ~a~&" res)
     (when (> (length res)
              1)
       (format t "that's too much !~&"))))

(tidy c1)
;; I'm going to clean up (toys book foo)
;; that's too much !
~~~

Note that these operators don't support `before`, `after` and `around`
methods (indeed, there is no room for them anymore). They do support
around methods, where `call-next-method` is allowed, but they don't
support calling `call-next-method` in the primary methods (it would
indeed be redundant since all primary methods are called, or clunky to
*not* call one).

CLOS allows us to define a new operator as a method combination type, be
it a lisp function, macro or special form. We'll let you refer to the
books if you feel the need.


## Debugging: tracing method combination

It is possible to [trace](http://www.xach.com/clhs?q=trace) the method
combination, but this is implementation dependent.

In SBCL, we can use `(trace foo :methods t)`. See [this post by an SBCL core developer](http://christophe.rhodes.io/notes/blog/posts/2018/sbcl_method_tracing/).

For example, given a generic:

~~~lisp
(defgeneric foo (x)
  (:method (x) 3))
(defmethod foo :around ((x fixnum))
  (1+ (call-next-method)))
(defmethod foo ((x integer))
  (* 2 (call-next-method)))
(defmethod foo ((x float))
  (* 3 (call-next-method)))
(defmethod foo :before ((x single-float))
  'single)
(defmethod foo :after ((x double-float))
 'double)
~~~

Let's trace it:

~~~lisp
(trace foo :methods t)

(foo 2.0d0)
  0: (FOO 2.0d0)
    1: ((SB-PCL::COMBINED-METHOD FOO) 2.0d0)
      2: ((METHOD FOO (FLOAT)) 2.0d0)
        3: ((METHOD FOO (T)) 2.0d0)
        3: (METHOD FOO (T)) returned 3
      2: (METHOD FOO (FLOAT)) returned 9
      2: ((METHOD FOO :AFTER (DOUBLE-FLOAT)) 2.0d0)
      2: (METHOD FOO :AFTER (DOUBLE-FLOAT)) returned DOUBLE
    1: (SB-PCL::COMBINED-METHOD FOO) returned 9
  0: FOO returned 9
9
~~~


# MOP

We gather here some examples that make use of the framework provided
by the meta-object protocol, the configurable object system that rules
Lisp's object system. We touch advanced concepts so, new reader, don't
worry: you don't need to understand this section to start using the
Common Lisp Object System.

We won't explain much about the MOP here, but hopefully sufficiently
to make you see its possibilities or to help you understand how some
CL libraries are built. We invite you to read the books referenced in
the introduction.


## Metaclasses

Metaclasses are needed to control the behaviour of other classes.

*As announced, we won't talk much. See also Wikipedia for [metaclasses](https://en.wikipedia.org/wiki/Metaclass) or [CLOS](https://en.wikipedia.org/wiki/Common_Lisp_Object_System)*.

The standard metaclass is `standard-class`:

~~~lisp
(class-of p1) ;; #<STANDARD-CLASS PERSON>
~~~

But we'll change it to one of our own, so that we'll be able to
**count the creation of instances**. This same mechanism could be used
to auto increment the primary key of a database system (this is
how the Postmodern or Mito libraries do), to log the creation of objects,
etc.

Our metaclass inherits from `standard-class`:

~~~lisp
(defclass counted-class (standard-class)
  ((counter :initform 0)))
#<STANDARD-CLASS COUNTED-CLASS>

(unintern 'person)
;; this is necessary to change the metaclass of person.
;; or (setf (find-class 'person) nil)
;; https://stackoverflow.com/questions/38811931/how-to-change-classs-metaclass#38812140

(defclass person ()
  ((name
    :initarg :name
    :accessor name))
  (:metaclass counted-class)) ;; <- metaclass
;; #<COUNTED-CLASS PERSON>
;;   ^^^ not standard-class anymore.
~~~

The `:metaclass` class option can appear only once.

Actually you should have gotten a message asking to implement
`validate-superclass`. So, still with the `closer-mop` library:

~~~lisp
(defmethod closer-mop:validate-superclass ((class counted-class)
                                           (superclass standard-class))
  t)
~~~

Now we can control the creation of new `person` instances:

~~~lisp
(defmethod make-instance :after ((class counted-class) &key)
  (incf (slot-value class 'counter)))
;; #<STANDARD-METHOD MAKE-INSTANCE :AFTER (COUNTED-CLASS) {1007718473}>
~~~

See that an `:after` qualifier is the safest choice, we let the
standard method run as usual and return a new instance.

The `&key` is necessary, remember that `make-instance` is given initargs.

Now testing:

~~~lisp
(defvar p3 (make-instance 'person :name "adam"))
#<PERSON {1007A8F5B3}>

(slot-value p3 'counter)
;; => error. No, our new slot isn't on the person class.
(slot-value (find-class 'person) 'counter)
;; 1

(make-instance 'person :name "eve")
;; #<PERSON {1007AD5773}>
(slot-value (find-class 'person) 'counter)
;; 2
~~~

It's working.


## Controlling the initialization of instances (initialize-instance)

To further control the creation of object instances, we can specialize the method
`initialize-instance`. It is called by `make-instance`, just after
a new instance was created but wasn't initialized yet with the
default initargs and initforms.

It is recommended (Keene) to create an after method, since creating a
primary method would prevent slots' initialization.

~~~lisp
(defmethod initialize-instance :after ((obj person) &key) ;; note &key
  (do something with obj))
~~~

A typical example would be to validate the initial values. Here we'll
check that the person's name is longer than 3 characters:

~~~lisp
(defmethod initialize-instance :after ((obj person) &key)
  (with-slots (name) obj
    (assert (>= (length name) 3))))
~~~

So this call doesn't work anymore:

~~~lisp
(make-instance 'person :name "me" )
;; The assertion (>= #1=(LENGTH NAME) 3) failed with #1# = 2.
;;   [Condition of type SIMPLE-ERROR]
~~~

We are prompted into the interactive debugger and we are given a
choice of restarts (continue, retry, abort).

So while we're at it, here's an assertion that uses the debugger
features to offer to change "name":

~~~lisp
(defmethod INITIALIZE-INSTANCE :after ((obj person) &key)
  (with-slots (name) obj
    (assert (>= (length name) 3)
            (name)  ;; creates a restart that offers to change "name"
            "The value of name is ~a. It should be longer than 3 characters." name)))
~~~

We get:

```
The value of name is me. It should be longer than 3 characters.
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [CONTINUE] Retry assertion with new value for NAME.    <--- new restart
 1: [RETRY] Retry SLIME REPL evaluation request.
 2: [*ABORT] Return to SLIME's top level.
```


Another rationale. The CLOS implementation of
    `make-instance` is in two stages: allocate the new object,
    and then pass it along with all the `make-instance` keyword
    arguments, to the generic function
    `initialize-instance`. Implementors and application writers
    define `:after` methods on
    `initialize-instance`, to initialize the slots of the
    instance. The system-supplied primary method does this with regard to
    (a) `:initform` and `:initarg` values supplied
    with the class was defined and (b) the keywords passed through from
    `make-instance`. Other methods can extend this behaviour as
    they see fit. For example, they might accept an additional keyword
    which invokes a database access to fill certain slots. The lambda list
    for `initialize-instance` is:

~~~
initialize-instance instance &rest initargs &key &allow-other-keys
~~~

See more in the books !
