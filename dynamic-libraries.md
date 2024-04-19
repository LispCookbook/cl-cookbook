---
title: Building Dynamic Libraries
---

Although the vast majority of Common Lisp implementations have
some kind of [foreign function interface](ffi.html) which allows you to
call functions from libraries which use C ABI, the other way around,
i.e. compiling your CL library as a library callable via C ABI from
other languages, might be rare.

Commercial implementations like LispWorks and Allegro CL usually
offer this functionality, and they are well documented.

This chapter describes a project called [SBCL-Librarian](https://github.com/quil-lang/sbcl-librarian),
an opinionated way to create libraries callable from C (anything which has C FFI) and Python using
an open-source and free-to-use implementation [Steel Bank Common Lisp](https://www.sbcl.org).
SBCL-Librarian does support callbacks so you can integrate your Lisp library with any code,
including Python code which might use its great machine learning and statistical libraries.

The way SBCL-Librarian works is that it generates C source files, a C header and a Python module.

The C source file is compiled first into a dynamic library which is (using the provided header file)
loadable from any C project or by any project in a language which supports loading C libraries.

The generated Python module loads the compiled library into Python process. This means that the C
library needs to be compiled before your Lisp library is used from Python code. This fact has two
main consequences - on one hand the Lisp library is all efficient native code, which is great.
Python interpreter can be quite slow and many libraries, especially the ones for machine learning
and statistics, are all compiled native code. You can achieve the same efficiency with Common Lisp.
On the other hand, your library can only use C interface to communicate with Python - primitive data
types from C, structures, functions and pointers (including pointers to functions). Some basic knowledge
of C is required.

## Preparing Environment

### Make SBCL Shared Library

Binary distributions of SBCL usually do not come with SBCL built as a shared library, which is necessary for SBCL-Librarian.
You can download it either from the [SBCL git repository](https://github.com/sbcl/sbcl)
or by [using Roswell](getting-started.html#with-roswell) and running the command `ros install sbcl-source`.

SBCL also requires a working Common Lisp system to bootstrap the compilation process. An easy trick is to download a binary installation from Roswell and add it to your `PATH` variable.

SBCL depends on the `zstd` library. On Linux-based systems, you can obtain both the library and its header files from the package manager, where it is usually named `libzstd-dev`. On Windows, the recommended approach is to use
[MSYS2](https://www.msys2.org) which includes Roswell, `zstd`, and its headers.

Navigate to the directory with the sources and run:

~~~bash
# Bash

# (assuming the version of your SBCL installed via Roswell is 2.4.1)
export PATH=~/.roswell/impls/x86-64/linux/sbcl-bin/2.4.1/bin/:$PATH

./make-config.sh --fancy
./make.sh --fancy
./make-shared-library.sh --fancy
~~~

Note that the shared library has a `.so` extension even on Windows and Mac, but it seems to work just fine. If you use Roswell in MSYS2, it can use your Windows home directory rather than your MSYS2 home directory, which are different paths. Therefore, the path to Roswell might be `$USERPROFILE/.roswell` (or `/C/Users/<username>/.roswell`),
not `~/.roswell/`.

### Download and Setup SBCL-Librarian

Clone the SBCL-Librarian repostiory:

~~~bash
git clone https://github.com/quil-lang/sbcl-librarian.git
~~~

## Hello World from Lisp

Although SBCL-Librarian comes with some documentation and a couple of examples, it doesn't really have anything like a basic tutorial. In this chapter we'll make a basic function which adds two numbers and we'll call it from Python.

Let's set a couple of environment variables for convenience:

~~~bash
# Directory with SBCL sources
export SBCL_SRC=~/.roswell/src/sbcl-2.4.1
# Directory with this project, don't forget the double slash at the end
# or it might not work
export CL_SOURCE_REGISTRY="~/prg/sbcl-librarian//"
~~~

Libraries are usually not searched for in the current directory on more modern Linux-based systems. The paths which Python searches for libraries are usually not set to the current working directory either. Let's set them this way for convenience.

~~~bash
export LD_LIBRARY_PATH=.:
export PATH=.:$PATH
~~~

Now we can create a file `helloworld.lisp` with following content:

~~~lisp
(require '#:asdf)
(asdf:load-system :sbcl-librarian)

(defpackage libhelloworld
  (:use :cl :sbcl-librarian))

(in-package libhelloworld)

;; will be called from Python
(defun hello-world (a b)
  (+ a b))


;; error enum to be used in C/Python code for error handling
(define-enum-type error-type "err_t"
  ("ERR_SUCCESS" 0)
  ("ERR_FAIL" 1))

;; mapping Common Lisp conditions to C enums
;; in this simple example, all conditions are mapped to number 1
;; which is "ERR_FAIL" in `error-type` enum
(define-error-map error-map error-type 0
  ((t (lambda (condition)
        (declare (ignore condition))
        (return-from error-map 1)))))

;; structure of the generated C source file
(define-api libhelloworld-api (:error-map error-map              ; error enum
                               :function-prefix "helloworld_")   ; prefix for all function names (C doesn't have namespaces)
  (:literal "/* types */")        ; just a comment (whatever is there will be printed as-is)
  (:type error-type)              ; outputs the error enum
  (:literal "/* functions */")
  (:function                      ; function declaration - name, return type, argument types
     (hello-world :int ((a :int) (b :int)))))

;; definition of the whole library - what is there
(define-aggregate-library libhelloworld (:function-linkage "LIBHELLOWORLD_API")
  sbcl-librarian:handles sbcl-librarian:environment libhelloworld-api)

;; builds the bindings
(build-bindings libhelloworld ".")
(build-python-bindings libhelloworld ".")

;; outputs the Lisp core
(build-core-and-die libhelloworld "." :compression t)
~~~

The macro `define-enum-type` creates a mapping between conditions signaled by Common Lisp functions and a return type for the wrapping C functions. If a condition is signaled from Common Lisp, it is translated into a number — a C function return value — within `define-error-map`. The enumeration type adds a C `enum`, so instead of:

~~~C
if (1 == cl_function()) {
~~~

you can write:

~~~C
if (ERR_FAIL == cl_function()) {
~~~

which is more readable.

`define-api` outlines the structure of the library code to be created, specifying the error map, types, functions, and their order (`:literal` is used for comments in this case).

`define-aggregate-library` defines the entire library, specifying what should be included and in what order.

You can compile the file with following commands:

~~~bash
$SBCL_SRC/run-sbcl.sh --script "helloworld.lisp"
cc -shared -fpic -o libhelloworld.so libhelloworld.c -L$SBCL_SRC/src/runtime -lsbcl
cp $SBCL_SRC/src/runtime/libsbcl.so .
~~~


You can run Python console and check that `helloworld` module was created successfully

~~~python
import helloworld

dir(helloworld)
~~~

Function `helloworld_hello_world` should be present in the printed dictionary.

This function follows a C standard that return value of the function is its error code
(0 is for success, other numbers should be defined in `err_t` class which follows the `error-map` definitions),
the last parameter of the function is its return value. Since this is a pointer to integer in this case,
an integer needs to be created using `ctypes` library and `helloworld_hello_world` has to be called with
a pointer to the result value.

The following program should print 11.

~~~python
import helloworld
import ctypes

rv = ctypes.c_int(0)
helloworld.helloworld_hello_world(5, 6, ctypes.pointer(rv))
print(rv.value)
~~~

There are two common problems which can occur, depending on your system.

First is a rather cryptic error from Python

~~~
>>> import helloworld
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
ImportError: dynamic module does not define module export function (PyInit_helloworld)
~~~

This means that Python tries to open `helloworld.so` as a Python module rather than `helloworld.py`. Since
`helloworld.so` is just an ordinary dynamic library and not a natively-compiled Python module, it
will not work.

~~~bash
cp ./helloworld.py ./py_helloworld.py
~~~

and in Python `import py_helloworld` .

If you experience a following exception being raised:

~~~
Traceback (most recent call last):
  ...
    raise Exception('Unable to locate libhelloworld') from e
Exception: Unable to locate libhelloworld
~~~

First, check that all required dependencies - `libsbcl` and `libzstd` in this case - are either copied
to the output directory or are in path which your operating system loads libraries from. If it still doesn't
work, it might be a problem with the mechanism Python locates libraries on your particular system.

Open `helloworld.py` (or `py_helloworld.py` if you renamed it as suggested earlier) and change the line

~~~Python
libpath = Path(find_library('libcallback')).resolve()
~~~

to a path for your operating system, e.g.

~~~Python
libpath = Path('./libhelloworld.so').resolve()
~~~


## More Complex Example: Callback Example

SBCL-Librarian includes several examples, one of which is a simple callback to Python code. This example comes with `Makefile` and with properly defined system using `ASD`.

### ASD File

The ASD file `libcallback.asd` declares a dependency on SBCL-Librarian:

~~~lisp
:defsystem-depends-on (#:sbcl-librarian)
:depends-on (#:sbcl-librarian)
~~~

The ASDF system needs to know where to find the SBCL-Librarian sources. One way to specify this is by setting the `CL_SOURCE_REGISTRY` environment variable to include its directory.

### Bindings.lisp

`bindings.lisp` contains the crucial elements for generating the C bindings:

~~~lisp
(defun call-callback (callback outbuffer)
  (sb-alien:with-alien ((str sb-alien:c-string "I guess "))
    (sb-alien:alien-funcall callback str outbuffer)))
~~~

This function is key to the example; it is invoked from Python code and calls back a Python method (the`callback` parameter). As SBCL-Librarian generates both a C library and a Python module that wraps it, this function can be called from either C or Python. This example focuses on Python.

SBCL-Librarian utilizes `sb-alien`, an SBCL package for interfacing with C functions. `with-alien` creates a resource (here it is `str` of type `c-string`) that is valid within its scope and is automatically disposed of afterward, preventing memory leaks. `alien-funcall` is used to call a C function, in this case `callback`, with a newly created string and a string buffer passed in as arguments.

~~~lisp
(sbcl-librarian::define-type :callback
  :c-type "void*"
  :alien-type (sb-alien:* (sb-alien:function sb-alien:void sb-alien:c-string (sb-alien:* sb-alien:char)))
  :python-type "c_void_p")

(sbcl-librarian::define-type :char-buffer
  :c-type "char*"
  :alien-type (sb-alien:* sb-alien:char)
  :python-type "c_char_p")
~~~

This section defines the types `callback` and `char-buffer` in C, Python, and Common Lisp. The C and Python types for both are `void*` and `char*`, respectively. The Common Lisp type for callback specifies a function prototype: a pointer to a function that returns `void` and takes two parameters, a `c-string` and a pointer to a `char`. The `sb-alien:*` indicates a pointer, so `:callback` is a pointer to a function. The `:char-buffer` type represents a `char*` in all three languages.

The rest of this file is similar to what was described in the `Hello World` section.

### Compile LISP Code

`script.lisp` is a straightforward Lisp script for compiling the Lisp sources and outputting the wrapper code and the Lisp core.

~~~lisp
(require '#:asdf)

(asdf:load-system '#:libcallback)

(in-package #:sbcl-librarian/example/libcallback)

(build-bindings libcallback ".")
(build-python-bindings libcallback ".")
(build-core-and-die libcallback "." :compression t)
~~~

Now you have a couple of new files.

`libcallback.c` is the source code for the library:

~~~c
#define CALLBACKING_API_BUILD

#include "libcallback.h"

void (*lisp_release_handle)(void* handle);
int (*lisp_handle_eq)(void* a, void* b);
void (*lisp_enable_debugger)();
void (*lisp_disable_debugger)();
void (*lisp_gc)();
err_t (*callback_call_callback)(void* fn, char* out_buffer);

extern int initialize_lisp(int argc, char **argv);

CALLBACKING_API int init(char* core) {
  static int initialized = 0;
  char *init_args[] = {"", "--core", core, "--noinform", };
  if (initialized) return 1;
  if (initialize_lisp(4, init_args) != 0) return -1;
  initialized = 1;
  return 0; }
~~~

At the top, you'll find several SBCL-related functions, such as `lisp_gc`, which signals to the Lisp garbage collector that it is a good time to run. Then there is a pointer to the `callback_call_callback` function. Finally, the `init` function, which should be run before executing any Lisp code.

SBCL (as of version 2.4.2) didn't support de-initialize the Lisp core so there are no functions for doing so.

`libcallback.h ` is a header file that should be included in both `lispcallback.c` and any calling C code. It contains prototypes of functions and function pointers in `lispcallback.c`, includes the error `enum`, and any comments added in `bindings.lisp`:

~~~C
typedef enum { ERR_SUCCESS = 0, ERR_FAIL = 1, } err_t;
~~~

The last file, `lispcallback.py`, is a Python wrapper around the library. The most notable part is this:

~~~Python
from ctypes import *
from ctypes.util import find_library

try:
    libpath = Path(find_library('libcallback')).resolve()
except TypeError as e:
    raise Exception('Unable to locate libcallback') from e
~~~

The rest of the file is similar to the C header file.

This setup loads a compiled C library (shared object, DLL, dylib) and informs the Python interpreter about the functions and types included in the library. It also initializes the Lisp core when loaded by the Python interpreter. The initialization needs to be called manually when the generated library is called from C.


### Compile C Code

~~~bash
cc -shared -fpic -o libcallback.so libcallback.c -L$SBCL_SRC/src/runtime -lsbcl
~~~

On Mac OS the command might be a bit different:

~~~bash
cc -dynamiclib -o libcallback.dylib libcallback.c -L$SBCL_SRC/src/runtime -lsbcl
~~~

If you do not have `$SBCL_SRC/src/runtime` in your `$PATH`, you should copy the `$SBCL_SRC/src/runtime/libsbcl.so` file to the current directory.

### Run

Now that everything is set up, you can run the example code using the following command:

~~~bash
$ python3 ./example.py 
~~~

If it's successful, you should see the output:

~~~bash
I guess  it works!
~~~

## Makefile

Each example comes with a Makefile designed for building on Mac. It even automatically builds the `libsbcl.so` library and copies it into the current directory. However, the command for building the project (e.g., `libcallback`) needs to be modified to work on Linux-based operating systems and on Windows (with MSYS2).

## CMake

Using CMake is relatively straightforward. Unfortunately, there is currently no CMake-aware library or a `vcpkg`/`conan` package, so you'll need to use `HINTS` with `find_library` to locate the necessary libraries.

Assuming you would like to compile a project named `my_project` and would like to add a LISP library, you could proceed as follows:

~~~CMake
# If there is a better way, let me know.
if(WIN32)
    set(DIR_SEPARATOR ";")
else()
    set(DIR_SEPARATOR ":")
endif()

# Set the ENV Vars for building the LISP part
set(SBCL_SRC "$ENV{SBCL_SRC}" CACHE PATH "Path to SBCL sources directory.")
set(SBCL_LIBRARIAN_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../sbcl-librarian" CACHE PATH "Source codes of SBCL-LIBRARIAN project.")
set(CL_SOURCE_REGISTRY "${CMAKE_CURRENT_SOURCE_DIR}${DIR_SEPARATOR}${SBCL_LIBRARIAN_DIR}" CACHE PATH "ASDF registry for building of the libray.")

# Find the SBCL library
find_library(libsbcl NAMES sbcl HINTS ${SBCL_SRC}/src/runtime/)

# Link the library to the C project
target_link_libraries(my_project ${libsbcl})

# Build LISP part of the project
add_custom_command(OUTPUT my_project-lisp.core my_project-lisp.c my_project-lisp.h my_project-lisp.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMAND ${CMAKE_COMMAND} -E env CL_SOURCE_REGISTRY="${CL_SOURCE_REGISTRY}"
        ${SBCL_SRC}/run-sbcl.sh ARGS --script script.lisp
    COMMAND ${CMAKE_COMMAND} -E copy_if_different my_project-lisp.core $<TARGET_FILE_DIR:my_project>
    COMMAND ${CMAKE_COMMAND} -E copy_if_different my_project-lisp.c $<TARGET_FILE_DIR:my_project>
    COMMAND ${CMAKE_COMMAND} -E copy_if_different my_project-lisp.h $<TARGET_FILE_DIR:my_project>
    COMMAND ${CMAKE_COMMAND} -E copy_if_different my_project-lisp.py $<TARGET_FILE_DIR:my_project>
    COMMAND ${CMAKE_COMMAND} -E rm my_project-lisp.core my_project-lisp.c my_project-lisp.h my_project-lisp.py

# Copy SBCL library if newer
add_custom_command(TARGET my_project POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${libsbcl}"
        $<TARGET_FILE_DIR:my_project>)
~~~
