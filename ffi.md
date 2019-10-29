---
title: Foreign Function Interfaces
---

The ANSI Common Lisp standard doesn't mention this topic. So almost everything that can be said here depends on your OS and your implementation.<a name="clisp-gethost">### Example: Calling 'gethostname' from CLISP

Note: You should read the</a> [relevant chapter](http://clisp.sourceforge.net/impnotes.html#dffi) from the CLISP implementation notes before you proceed.

`int gethostname(char *name, int len)` follows a typical pattern of C "out"-parameter convention - it expects a pointer to a buffer it's going to fill. So you must view this parameter as either `:OUT` or `:IN-OUT`. Additionally, one must tell the function the size of the buffer. Here `len` is just an `:IN` parameter. Sometimes this will be an `:IN-OUT` parameter, returning the number of bytes actually filled in.

So `name` is actually a pointer to an array of up to `len` characters, regardless of what the poor "`char *`" C prototype says, to be used like a C string (0-termination). How many elements are in the array? Luckily, in our case, you can find it out without calculating the `sizeof()` a C structure. It's a hostname that will be returned. The Solaris 2.x manpage says "Host names are limited to MAXHOSTNAMELEN characters, currently 256."

Also, in the present example, you can use allocation `:ALLOCA`, like you'd do in C: stack-allocate a temporary. Why make things worse when using Lisp than when using C?

This yields the following useful signature for your foreign function:

~~~lisp
(ffi:def-c-call-out gethostname
     (:arguments (name (ffi:c-ptr (ffi:c-array-max ffi:char 256))
     :out :alloca)
     (len ffi:int))
     ;; (:return-type BOOLEAN) could have been used here
     ;; (Solaris says it's either 0 or -1).
     (:return-type ffi:int))

     (defun myhostname ()
     (multiple-value-bind (success name)
     ;; :OUT or :IN-OUT parameters are returned via multiple values
     (gethostname 256)
     (if (zerop success)
     (subseq name 0 (position #\null name))
     (error ... ; errno may be set
     ...))))
     (defvar hostname (myhostname))
~~~

Possibly `SUBSEQ` and `POSITION` are superfluous, thanks to `C-ARRAY-MAX` as opposed to `C-ARRAY`:

~~~lisp
(defun myhostname ()
     (multiple-value-bind (success name)
     ;; :out or :in-out parameters are returned via multiple values
     (gethostname 256)
     (if (zerop success) name
     (error ... ; errno may be set
     ...))))
~~~

<a name="alisp-gethost"></a>

### Example: Calling 'gethostname' from Allegro CL

This is how the same example above would be written in Allegro Common Lisp version 6 and above. ACL doesn't explicitly distinguish between `input` and `output` arguments. The way to declare an argument as `output` (i.e., modifiable by C) is to use an array, since arrays are passed by reference and C therefore receives a pointer to a memory location (which is what it expects). In this case things are made even easier by the fact that `gethostname()` expects an array of char, and a `SIMPLE-ARRAY` of `CHARACTER` represents essentially the same thing in Lisp. The foreign function definition is therefore the following:

~~~lisp
(def-foreign-call (c-get-hostname "gethostname")
     ((name (* :char) (simple-array 'character (*)))
     (len :int integer))
     :returning :int)
~~~

Let's read this line by line: this form defines a Lisp function called `C-GET-HOSTNAME` that calls the C function `gethostname()`. It takes two arguments: the first one, called `NAME`, is a pointer to a char (`*char` in C), and a `SIMPLE-ARRAY` of characters in Lisp; the second one is called `LEN`, and is an integer. The function returns an integer value.

And now the Lisp side:

~~~lisp
(defun get-hostname ()
     (let* ((name (make-array 256 :element-type 'character))
     (result (c-get-hostname name 256)))
     (if (zerop result)
     (let ((pos (position #\null name)))
     (subseq name 0 pos))
     (error "gethostname() failed."))))
~~~

This function creates the `NAME` array, calls `C-GET-HOSTNAME` to fill it and then checks the returned value. If the value is zero, then the call was successful, and we return the contents of `NAME` up to the first 0 character (the string terminator in C), otherwise we signal an error. Note that, unlike the previous example, we allocate the string in Lisp, and we rely on the Lisp garbage collector to get rid of it after the function terminates. Here is a usage example:

~~~lisp
* (get-hostname)
     "terminus"
~~~

Working with strings is, in general, easier than the previous example showed. Let's say you want to call `getenv()` from Lisp to access the value of an environment variable. `getenv()` takes a string argument (the variable name) and returns another string (the variable value). To be more precise, the argument is a _pointer_ to a sequence of characters that should have been allocated by the caller, and the return value is a pointer to an already-existing sequence of chars (in the environment). Here is the definition of `C-GETENV`:

~~~lisp
(def-foreign-call (c-getenv "getenv")
     ((var (* :char) string))
     :returning :int
     :strings-convert t)
~~~

The argument in this case is still a pointer to char in C, but we can declare it a `STRING` to Lisp. The return value is a pointer, so we declare it as integer. Finally, the `:STRINGS-CONVERT` keyword argument specifies that ACL should automatically translate the Lisp string passed as the first argument into a C string. Here is how it's used:

~~~lisp
* (c-getenv "SHELL")
     -1073742215
~~~

If you are surprised by the return value, just remember that `C-GETENV` returns a pointer, and we must tell Lisp how to interpret the contents of the memory location pointed to by it. Since in this case we know that it will point to a C string, we can use the `FF:NATIVE-TO-STRING` function to convert it to a Lisp string:

~~~lisp
* (native-to-string (c-getenv "SHELL"))
     "/bin/tcsh"
     9
     9
~~~

(The second and third values are the number of characters and bytes copied, respectively). One caveat: if you ask for the value of a non-existent variable, `C-GETENV` will return 0, and `NATIVE-TO-STRING` will fail. So a safer example would be:

~~~lisp
* (let ((ptr (c-getenv "NOSUCHVAR")))
     (unless (zerop ptr)
     (native-to-string ptr)))
     NIL
~~~
