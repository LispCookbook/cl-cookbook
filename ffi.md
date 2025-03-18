---
title: Foreign Function Interfaces
---

The ANSI Common Lisp standard doesn't mention this topic. So almost everything that can be said here depends on your OS and your implementation. However these days, we can use the [CFFI](https://github.com/cffi/cffi) library, a portable and easy-to-use C foreign function interface.

> CFFI, the Common Foreign Function Interface, purports to be a portable FFI for Common Lisp. It abstracts away the differences between the API of the native FFI's of the various Common Lisp implementations.

We'll see an example right now.


## CFFI: calling a C function from the `math.h` header file.

Let's use `defcfun` to interface with the foreign [ceil](https://en.cppreference.com/w/c/numeric/math/ceil) C function from `math.h`.

[defcfun](https://cffi.common-lisp.dev/manual/html_node/defcfun.html) is a macro in the cffi library that generates a function with the name you give it.

~~~lisp
CL-USER> (cffi:defcfun ("ceil" c-ceil) :double (number :double))
~~~

We say that the "ceil" C function will be called "c-ceil" on our Lisp side, it takes one argument that is a double float, and it returns a number that is also a double float.

Here is the above function macroexpanded with `macrostep-expand`:

~~~lisp
(progn
  nil
  (defun c-ceil (number)
    (let ((#:g312 number))
      (cffi-sys:%foreign-funcall "ceil" (:double #:g312 :double) :convention
				 :cdecl :library :default))))
~~~

The reason we called it `c-ceil` and not `ceil` is only for the example, so we know this is a wrapper around C. You can name it "ceil", since it doesn't designate a built-in Common Lisp function or macro.

Now that we have a c-ceil function from `math.h`, let's use it! We must give it double float.

~~~lisp
CL-USER> (c-ceil 5.4d0)
6.0d0
~~~

As you can see, it works! The double gets rounded up to `6.0d0` as expected.

Let's try another one! This time, we'll use [floor](https://en.cppreference.com/w/c/numeric/math/floor), and we couldn't name it "floor" because this Common Lisp function exists.

~~~lisp
CL-USER> (cffi:defcfun ("floor" c-floor) :double (number :double))
C-FLOOR
CL-USER> (c-floor 5d0)
5.0d0
CL-USER> (c-floor 5.4d0)
5.0d0
~~~

Great!

One more, let's try `sqrt` from math.h, still with double floats:

~~~lisp
CL-USER> (cffi:defcfun ("sqrt" c-sqrt) :double (number :double))
C-SQRT
CL-USER> (c-sqrt 36.50d0)
6.041522986797286d0
~~~

We can do arithmetic with our new `c-sqrt`:

~~~lisp
CL-USER> (+ 2 (c-sqrt 3d0))
3.732050807568877d0
~~~

We can even use our new shiny `c-sqrt` to map over a list of doubles and take the square root of all of them!

~~~lisp
CL-USER> (mapcar #'c-sqrt '(3d0 4d0 5d0 6d0 7.5d0 12.75d0))
(1.7320508075688772d0 2.0d0 2.23606797749979d0 2.449489742783178d0
 2.7386127875258306d0 3.570714214271425d0)
~~~
