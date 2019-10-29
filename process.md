---
title: Threads
---


<a name="intro"></a>

## Introduction

By _threads_, we mean separate execution strands within a single Lisp
process, sharing the same address space. Typically, execution is
automatically switched between these strands by the system (either by
the lisp kernel or by the operating system) so that tasks appear to be
completed in parallel (asynchronously). This page discusses the
creation and management of threads and some aspects of interactions
between them. For information about the interaction between lisp and
other _processes_, see [Interfacing with your OS](os.html).

An instant pitfall for the unwary is that most implementations refer
(in nomenclature) to threads as _processes_ - this is a historical
feature of a language which has been around for much longer than the
term _thread_. Call this maturity a sign of stable implementations, if
you will.

The ANSI Common Lisp standard doesn't mention this topic. We will
present here the portable
[bordeaux-threads](https://github.com/sionescu/bordeaux-threads)
library, [SBCL threads](http://www.sbcl.org/manual/index.html#sb_002dconcurrency) and the [lparallel](https://lparallel.org)
library.

Bordeaux-threads is a de-facto standard portable library, that exposes
rather low-level primitives. Lparallel builds on it and features:

-  a simple model of task submission with receiving queue
-  constructs for expressing fine-grained parallelism
-  **asynchronous condition handling** across thread boundaries
-  **parallel versions of map, reduce, sort, remove**, and many others
-  **promises**, futures, and delayed evaluation constructs
-  computation trees for parallelizing interconnected tasks
-  bounded and unbounded FIFO **queues**
-  **channels**
-  high and low priority tasks
-  task killing by category
-  integrated timeouts

For more libraries on parallelism and concurrency, see the [awesome CL list](https://github.com/CodyReichert/awesome-cl#parallelism-and-concurrency)
and [Quickdocs](http://quickdocs.org/).

<a name="why_bother"></a>

### Why bother?

The first question to resolve is: why bother with threads? Sometimes
your answer will simply be that your application is so straightforward
that you need not concern yourself with threads at all. But in many
other cases it's difficult to imagine how a sophisticated application
can be written without multi-threading. For example:

*   you might be writing a server which needs to be able to respond to
    more than one user / connection at a time (for instance: a web
    server) on the Sockets page);
*   you might want to perform some background activity, without
    halting the main application while this is going on;
*   you might want your application to be notified when a certain time
    has elapsed;
*   you might want to keep the application running and active while
    waiting for some system resource to become available;
*   you might need to interface with some other system which requires
    multithreading (for example, "windows" under Windows which
    generally run in their own threads);
*   you might want to associate different contexts (e.g. different
    dynamic bindings) with different parts of the application;
*   you might even have the simple need to do two things at once.


<a name="emergency"></a>

### What is Concurrency? What is Parallelism?

*Credit: The following was first written on
[z0ltan.wordpress.com](https://z0ltan.wordpress.com/2016/09/02/basic-concurrency-and-parallelism-in-common-lisp-part-3-concurrency-using-bordeaux-and-sbcl-threads/)
by Timmy Jose.*

Concurrency is a way of running different, possibly related, tasks
seemingly simultaneously. What this means is that even on a single
processor machine, you can simulate simultaneity using threads (for
instance) and context-switching them.

In the case of system (native OS) threads, the scheduling and context
switching is ultimately determined by the OS. This is the case with
Java threads and Common Lisp threads.

In the case of “green” threads, that is to say threads that are
completely managed by the program, the scheduling can be completely
controlled by the program itself. Erlang is a great example of this
approach.

So what is the difference between Concurrency and Parallelism?
Parallelism is usually defined in a very strict sense to mean
independent tasks being run in parallel, simultaneously, on different
processors or on different cores. In this narrow sense, you really
cannot have parallelism on a single-core, single-processor machine.

It rather helps to differentiate between these two related concepts on
a more abstract level – concurrency primarily deals with providing the
illusion of simultaneity to clients so that the system doesn’t appear
locked when a long running operation is underway. GUI systems are a
wonderful example of this kind of system. Concurrency is therefore
concerned with providing good user experience and not necessarily
concerned with performance benefits.

Java’s Swing toolkit and JavaScript are both single-threaded, and yet
they can give the appearance of simultaneity because of the context
switching behind the scenes. Of course, concurrency is implemented
using multiple threads/processes in most cases.

Parallelism, on the other hand, is mostly concerned with pure
performance gains. For instance, if we are given a task to find the
squares of all the even numbers in a given range, we could divide the
range into chunks which are then run in parallel on different cores or
different processors, and then the results can be collated together to
form the final result. This is an example of Map-Reduce in action.

So now that we have separated the abstract meaning of Concurrency from
that of Parallelism, we can talk a bit about the actual mechanism used
to implement them. This is where most of the confusion arise for a lot
of people. They tend to tie down abstract concepts with specific means
of implementing them. In essence, both abstract concepts may be
implemented using the same mechanisms! For instance, we may implement
concurrent features and parallel features using the same basic thread
mechanism in Java. It’s only the conceptual intertwining or
independence of tasks at an abstract level that makes the difference
for us.

For instance, if we have a task where part of the work can be done on
a different thread (possibly on a different core/processor), but the
thread which spawns this thread is logically dependent on the results
of the spawned thread (and as such has to “join” on that thread), it
is still Concurrency!

So the bottomline is this – Concurrency and Parallelism are different
concepts, but their implementations may be done using the same
mechanisms — threads, processes, etc.


## Bordeaux threads

The Bordeaux library provides a platform independent way to handle
basic threading on multiple Common Lisp implementations. The
interesting bit is that it itself does not really create any native
threads — it relies entirely on the underlying implementation to do
so.

On there other hand, it does provide some useful extra features in its
own abstractions over the lower-level threads.

Also, you can see from the demo programs that a lot of the Bordeaux
functions seem quite similar to those used in SBCL. I don’t really
think that this is a coincidence.

You can refer to the documentation for more details (check the
“Wrap-up” section).

### Installing Bordeaux Threads

First let’s load up the Bordeaux library using Quicklisp:

~~~lisp
CL-USER> (ql:quickload :bt-semaphore)
To load "bt-semaphore":
  Load 1 ASDF system:
    bt-semaphore
; Loading "bt-semaphore"

(:BT-SEMAPHORE)
~~~


### Checking for thread support in Common Lisp

Regardless of the Common Lisp implementation, there is a standard way
to check for thread support availability:

~~~lisp
CL-USER> (member :thread-support *FEATURES*)
(:THREAD-SUPPORT :SWANK :QUICKLISP :ASDF-PACKAGE-SYSTEM :ASDF3.1 :ASDF3 :ASDF2
 :ASDF :OS-MACOSX :OS-UNIX :NON-BASE-CHARS-EXIST-P :ASDF-UNICODE :64-BIT
 :64-BIT-REGISTERS :ALIEN-CALLBACKS :ANSI-CL :ASH-RIGHT-VOPS :BSD
 :C-STACK-IS-CONTROL-STACK :COMMON-LISP :COMPARE-AND-SWAP-VOPS
 :COMPLEX-FLOAT-VOPS :CYCLE-COUNTER :DARWIN :DARWIN9-OR-BETTER :FLOAT-EQL-VOPS
 :FP-AND-PC-STANDARD-SAVE :GENCGC :IEEE-FLOATING-POINT :INLINE-CONSTANTS
 :INODE64 :INTEGER-EQL-VOP :LINKAGE-TABLE :LITTLE-ENDIAN
 :MACH-EXCEPTION-HANDLER :MACH-O :MEMORY-BARRIER-VOPS :MULTIPLY-HIGH-VOPS
 :OS-PROVIDES-BLKSIZE-T :OS-PROVIDES-DLADDR :OS-PROVIDES-DLOPEN
 :OS-PROVIDES-PUTWC :OS-PROVIDES-SUSECONDS-T :PACKAGE-LOCAL-NICKNAMES
 :PRECISE-ARG-COUNT-ERROR :RAW-INSTANCE-INIT-VOPS :SB-DOC :SB-EVAL :SB-LDB
 :SB-PACKAGE-LOCKS :SB-SIMD-PACK :SB-SOURCE-LOCATIONS :SB-TEST :SB-THREAD
 :SB-UNICODE :SBCL :STACK-ALLOCATABLE-CLOSURES :STACK-ALLOCATABLE-FIXED-OBJECTS
 :STACK-ALLOCATABLE-LISTS :STACK-ALLOCATABLE-VECTORS
 :STACK-GROWS-DOWNWARD-NOT-UPWARD :SYMBOL-INFO-VOPS :UD2-BREAKPOINTS :UNIX
 :UNWIND-TO-FRAME-AND-CALL-VOP :X86-64)
~~~

If there were no thread support, it would show “NIL” as the value of the expression.

Depending on the specific library being used, we may also have
different ways of checking for concurrency support, which may be used
instead of the common check mentioned above.

For instance, in our case, we are interested in using the Bordeaux
library. To check whether there is support for threads using this
library, we can see whether the *supports-threads-p* global variable
is set to NIL (no support) or T (support available):

~~~lisp
CL-USER> bt:*supports-threads-p*
T
~~~

Okay, now that we’ve got that out of the way, let’s test out both the
platform-independent library (Bordeaux) as well as the
platform-specific support (SBCL in this case).

To do this, let us work our way through a number of simple examples:

-    Basics — list current thread, list all threads, get thread name
-    Update a global variable from a thread
-    Print a message onto the top-level using a thread
-    Print a message onto the top-level — fixed
-    Print a message onto the top-level — better
-    Modify a shared resource from multiple threads
-    Modify a shared resource from multiple threads — fixed using locks
-    Modify a shared resource from multiple threads — using atomic operations
-    Joining on a thread, destroying a thread example

### Basics — list current thread, list all threads, get thread name

~~~lisp
    ;;; Print the current thread, all the threads, and the current thread's name
    (defun print-thread-info ()
      (let* ((curr-thread (bt:current-thread))
             (curr-thread-name (bt:thread-name curr-thread))
             (all-threads (bt:all-threads)))
        (format t "Current thread: ~a~%~%" curr-thread)
        (format t "Current thread name: ~a~%~%" curr-thread-name)
        (format t "All threads:~% ~{~a~%~}~%" all-threads))
      nil)
~~~

And the output:

~~~lisp
    CL-USER> (print-thread-info)
    Current thread: #<THREAD "repl-thread" RUNNING {10043B8003}>

    Current thread name: repl-thread

    All threads:
     #<THREAD "repl-thread" RUNNING {10043B8003}>
    #<THREAD "auto-flush-thread" RUNNING {10043B7DA3}>
    #<THREAD "swank-indentation-cache-thread" waiting on: #<WAITQUEUE  {1003A28103}> {1003A201A3}>
    #<THREAD "reader-thread" RUNNING {1003A20063}>
    #<THREAD "control-thread" waiting on: #<WAITQUEUE  {1003A19E53}> {1003A18C83}>
    #<THREAD "Swank Sentinel" waiting on: #<WAITQUEUE  {1003790043}> {1003788023}>
    #<THREAD "main thread" RUNNING {1002991CE3}>

    NIL
~~~

Update a global variable from a thread:

~~~lisp
    (defparameter *counter* 0)

    (defun test-update-global-variable ()
      (bt:make-thread
       (lambda ()
         (sleep 1)
         (incf *counter*)))
      *counter*)
~~~

We create a new thread using `bt:make-thread`, which takes a lambda
abstraction as a parameter. Note that this lambda abstraction cannot
take any parameters.

Another point to note is that unlike some other languages (Java, for
instance), there is no separation from creating the thread object and
starting/running it. In this case, as soon as the thread is created,
it is executed.

The output:

~~~lisp
    CL-USER> (test-update-global-variable)

    0
    CL-USER> *counter*
    1
~~~

As we can see, because the main thread returned immediately, the
initial value of `*counter*` is 0, and then around a second later, it
gets updated to 1 by the anonymous thread.

### Create a thread: print a message onto the top-level

~~~lisp
    ;;; Print a message onto the top-level using a thread
    (defun print-message-top-level-wrong ()
      (bt:make-thread
       (lambda ()
         (format *standard-output* "Hello from thread!"))
       :name "hello")
      nil)
~~~

And the output:

~~~lisp
    CL-USER> (print-message-top-level-wrong)
    NIL
~~~

So what went wrong? The problem is variable binding. Now, the ’t’
parameter to the format function refers to the top-level, which is a
Common Lisp term for the main console stream, also referred to by the
global variable `*standard-output*`. So we could have expected the
output to be shown on the main console screen.

The same code would have run fine if we had not run it in a separate
    thread. What happens is that each thread has its own stack where
    the variables are rebound. In this case, even for
    `*standard-output*`, which being a global variable, we would assume
    should be available to all threads, is rebound inside each thread!
    This is similar to the concept of ThreadLocal storage in Java.
    Print a message onto the top-level — fixed:

So how do we fix the problem of the previous example? By binding the top-level at the time of thread creation of course. Pure lexical scoping to the rescue!

~~~lisp
    ;;; Print a message onto the top-level using a thread — fixed
    (defun print-message-top-level-fixed ()
      (let ((top-level *standard-output*))
        (bt:make-thread
         (lambda ()
           (format top-level "Hello from thread!"))
         :name "hello")))
      nil)
~~~

Which produces:

~~~lisp
    CL-USER> (print-message-top-level-fixed)
    Hello from thread!
    NIL
~~~

Phew! However, there is another way of producing the same result using
a very interesting reader macro as we’ll see next.

### Print a message onto the top-level — read-time eval macro

Let’s take a look at the code first:

~~~lisp
    ;;; Print a message onto the top-level using a thread - reader macro

    (eval-when (:compile-toplevel)
      (defun print-message-top-level-reader-macro ()
        (bt:make-thread
         (lambda ()
           (format #.*standard-output* "Hello from thread!")))
        nil))

    (print-message-top-level-reader-macro)
~~~

And the output:

~~~lisp
    CL-USER> (print-message-top-level-reader-macro)
    Hello from thread!
    NIL
~~~

So it works, but what’s the deal with the eval-when and what is that
strange #. symbol before `*standard-output*`?

eval-when controls when evaluation of Lisp expressions takes place. We
can have three targets — :compile-toplevel, :load-toplevel, and
:execute.

The `#.` symbol is what is called a “Reader macro”. A reader (or read)
macro is called so because it has special meaning to the Common Lisp
Reader, which is the component that is responsible for reading in
Common Lisp expressions and making sense out of them. This specific
reader macro ensures that the binding of `*standard-output*` is done
at read time.

Binding the value at read-time ensures that the original value of
`*standard-output*` is maintained when the thread is run, and the output
is shown on the correct top-level.

Now this is where the eval-when bit comes into play. By wrapping the
whole function definition inside the eval-when, and ensuring that
evaluation takes place during compile time, the correct value of
`*standard-output*` is bound. If we had skipped the eval-when, we would
see the following error:

~~~lisp
      error:
        don't know how to dump #<SWANK/GRAY::SLIME-OUTPUT-STREAM {100439EEA3}> (default MAKE-LOAD-FORM method called).
        ==>
          #<SWANK/GRAY::SLIME-OUTPUT-STREAM {100439EEA3}>

      note: The first argument never returns a value.
      note:
        deleting unreachable code
        ==>
          "Hello from thread!"


    Compilation failed.
~~~

And that makes sense because SBCL cannot make sense of what this
output stream returns since it is a stream and not really a defined
value (which is what the ‘format’ function expects). That is why we
see the “unreachable code” error.

Note that if the same code had been run on the REPL directly, there
would be no problem since the resolution of all the symbols would be
done correctly by the REPL thread.


### Modify a shared resource from multiple threads

Suppose we have the following setup with a minimal bank-account class (no error checks):

~~~lisp
    ;;; Modify a shared resource from multiple threads

    (defclass bank-account ()
      ((id :initarg :id
           :initform (error "id required")
           :accessor :id)
       (name :initarg :name
             :initform (error "name required")
             :accessor :name)
       (balance :initarg :balance
                :initform 0
                :accessor :balance)))

    (defgeneric deposit (account amount)
      (:documentation "Deposit money into the account"))

    (defgeneric withdraw (account amount)
      (:documentation "Withdraw amount from account"))

    (defmethod deposit ((account bank-account) (amount real))
      (incf (:balance account) amount))

    (defmethod withdraw ((account bank-account) (amount real))
      (decf (:balance account) amount))
~~~

And we have a simple client which apparently does not believe in any form of synchronisation:

~~~lisp
    (defparameter *rich*
      (make-instance 'bank-account
                     :id 1
                     :name "Rich"
                     :balance 0))
    ; compiling (DEFPARAMETER *RICH* ...)

    (defun demo-race-condition ()
      (loop repeat 100
         do
           (bt:make-thread
            (lambda ()
              (loop repeat 10000 do (deposit *rich* 100))
              (loop repeat 10000 do (withdraw *rich* 100))))))
~~~

This is all we are doing – create a new bank account instance (balance
0), and then create a 100 threads, each of which simply deposits an
amount of 100 10000 times, and then withdraws the same amount the same
number of times. So the final result should be the same as that of the
opening balance, which is 0, right? Let’s check that and see.

On a sample run, we might get the following results:

~~~lisp
    CL-USER> (:balance *rich*)
    0
    CL-USER> (dotimes (i 5)
               (demo-race-condition))
    NIL
    CL-USER> (:balance *rich*)
    22844600
~~~

Whoa! The reason for this discrepancy is that incf and decf are not
atomic operations — they consist of multiple sub-operations, and the
order in which they are executed is not in our control.

This is what is called a “race condition” — multiple threads
contending for the same shared resource with at least one modifying
thread which, more likely than not, reads the wrong value of the
object while modifying it. How do we fix it? One simple way it to use
locks (mutex in this case, could be semaphores for more complex
situations).

### Modify a shared resource from multiple threads — fixed using locks

Let’s rest the balance for the account back to 0 first:

~~~lisp
    CL-USER> (setf (:balance *rich*) 0)
    0
    CL-USER> (:balance *rich*)
    0
~~~

Now let’s modify the demo-race-condition function to access the shared resource using locks (created using bt:make-lock and used as shown):

~~~lisp
    (defvar *lock* (bt:make-lock))
    ; compiling (DEFVAR *LOCK* …)

    (defun demo-race-condition-locks ()
      (loop repeat 100
         do
           (bt:make-thread
            (lambda ()
              (loop repeat 10000 do (bt:with-lock-held (*lock*)
                                      (deposit *rich* 100)))
              (loop repeat 10000 do (bt:with-lock-held (*lock*)
                                      (withdraw *rich* 100)))))))
    ; compiling (DEFUN DEMO-RACE-CONDITION-LOCKS ...)
~~~

And let’s do a bigger sample run this time around:

~~~lisp
    CL-USER> (dotimes (i 100)
               (demo-race-condition-locks))
    NIL
    CL-USER> (:balance *rich*)
    0
~~~

Excellent! Now this is better. Of course, one has to remember that
using a mutex like this is bound to affect performance. There is a
better way in quite a few circumstances — using atomic operations when
possible. We’ll cover that next.

### Modify a shared resource from multiple threads — using atomic operations

Atomic operations are operations that are guaranteed by the system to
all occur inside a conceptual transaction, i.e., all the
sub-operations of the main operation all take place together without
any interference from outside. The operation succeeds completely or
fails completely. There is no middle ground, and there is no
inconsistent state.

Another advantage is that performance is far superior to using locks
to protect access to the shared state. We will see this difference in
the actual demo run.

The Bordeaux library does not provide any real support for atomics, so
we will have to depend on the specific implementation support for
that. In our case, that is SBCL, and so we will have to defer this
demo to the SBCL section.

### Joining on a thread, destroying a thread

To join on a thread, we use the `bt:join-thread` function, and for
destroying a thread (not a recommended operation), we can use the
`bt:destroy-thread` function.

A simple demo:

~~~lisp
    (defmacro until (condition &body body)
      (let ((block-name (gensym)))
        `(block ,block-name
           (loop
               (if ,condition
                   (return-from ,block-name nil)
                   (progn
                       ,@body))))))

    (defun join-destroy-thread ()
      (let* ((s *standard-output*)
            (joiner-thread (bt:make-thread
                            (lambda ()
                              (loop for i from 1 to 10
                                 do
                                   (format s "~%[Joiner Thread]  Working...")
                                   (sleep (* 0.01 (random 100)))))))
            (destroyer-thread (bt:make-thread
                               (lambda ()
                                 (loop for i from 1 to 1000000
                                    do
                                      (format s "~%[Destroyer Thread] Working...")
                                      (sleep (* 0.01 (random 10000))))))))
        (format t "~%[Main Thread] Waiting on joiner thread...")
        (bt:join-thread joiner-thread)
        (format t "~%[Main Thread] Done waiting on joiner thread")
        (if (bt:thread-alive-p destroyer-thread)
            (progn
              (format t "~%[Main Thread] Destroyer thread alive... killing it")
              (bt:destroy-thread destroyer-thread))
            (format t "~%[Main Thread] Destroyer thread is already dead"))
        (until (bt:thread-alive-p destroyer-thread)
               (format t "[Main Thread] Waiting for destroyer thread to die..."))
        (format t "~%[Main Thread] Destroyer thread dead")
        (format t "~%[Main Thread] Adios!~%")))
~~~

And the output on a run:

~~~lisp
    CL-USER> (join-destroy-thread)

    [Joiner Thread]  Working...
    [Destroyer Thread] Working...
    [Main Thread] Waiting on joiner thread...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Main Thread] Done waiting on joiner thread
    [Main Thread] Destroyer thread alive... killing it
    [Main Thread] Destroyer thread dead
    [Main Thread] Adios!
    NIL
~~~

The until macro simply loops around until the condition becomes
true. The rest of the code is pretty much self-explanatory — the main
thread waits for the joiner-thread to finish, but it immediately
destroys the destroyer-thread.

Again, it is not recommended to use `bt:destroy-thread`. Any conceivable
situation which requires this function can probably be done better
with another approach.

Now let’s move onto some more comprehensive examples which tie
together all the concepts discussed thus far.

### Useful functions

Here is a summary of the functions, macros and global variables which
were used in the demo examples along with some extras. These should
cover most of the basic programming scenarios:

-    `bt:*supports-thread-p*` (to check for basic thread support)
-    `bt:make-thread` (create a new thread)
-    `bt:current-thread` (return the current thread object)
-    `bt:all-threads` (return a list of all running threads)
-    `bt:thread-alive-p` (checks if the thread is still alive)
-    `bt:thread-name` (return the name of the thread)
-    `bt:join-thread` (join on the supplied thread)
-    `bt:interrupt-thread` (interrupt the given thread)
-    `bt:destroy-thread` (attempt to abort the thread)
-    `bt:make-lock` (create a mutex)
-    `bt:with-lock-held` (use the supplied lock to protect critical code)


## SBCL threads

SBCL provides support for native threads via its [sb-thread](http://www.sbcl.org/manual/index.html#Threading)
package. These are very low-level functions, but we can build our own
abstractions on top of these as shown in the demo examples.

You can refer to the documentation for more details (check the
“Wrap-up” section).

You can see from the examples below that there is a strong
correspondence between Bordeaux and SBCL Thread functions. In most
cases, the only difference is the change of package name from bt to
sb-thread.

It is evident that the Bordeaux thread library was more or less based
on the SBCL implementation. As such, explanation will be provided only
in those cases where there is a major difference in syntax or
semantics.

### Basics — list current thread, list all threads, get thread name

The code:


~~~lisp
    ;;; Print the current thread, all the threads, and the current thread's name

    (defun print-thread-info ()
      (let* ((curr-thread sb-thread:*current-thread*)
             (curr-thread-name (sb-thread:thread-name curr-thread))
             (all-threads (sb-thread:list-all-threads)))
        (format t "Current thread: ~a~%~%" curr-thread)
        (format t "Current thread name: ~a~%~%" curr-thread-name)
        (format t "All threads:~% ~{~a~%~}~%" all-threads))
      nil)
~~~

And the output:

~~~lisp
    CL-USER> (print-thread-info)
    Current thread: #<THREAD "repl-thread" RUNNING {10043B8003}>

    Current thread name: repl-thread

    All threads:
     #<THREAD "repl-thread" RUNNING {10043B8003}>
    #<THREAD "auto-flush-thread" RUNNING {10043B7DA3}>
    #<THREAD "swank-indentation-cache-thread" waiting on: #<WAITQUEUE  {1003A28103}> {1003A201A3}>
    #<THREAD "reader-thread" RUNNING {1003A20063}>
    #<THREAD "control-thread" waiting on: #<WAITQUEUE  {1003A19E53}> {1003A18C83}>
    #<THREAD "Swank Sentinel" waiting on: #<WAITQUEUE  {1003790043}> {1003788023}>
    #<THREAD "main thread" RUNNING {1002991CE3}>

    NIL
~~~

### Update a global variable from a thread

The code:

~~~lisp
    ;;; Update a global variable from a thread

    (defparameter *counter* 0)

    (defun test-update-global-variable ()
      (sb-thread:make-thread
       (lambda ()
         (sleep 1)
         (incf *counter*)))
      *counter*)
~~~

And the output:

~~~lisp
    CL-USER> (test-update-global-variable)
    0
~~~

### Print a message onto the top-level using a thread

The code:

~~~lisp
    ;;; Print a message onto the top-level using a thread

    (defun print-message-top-level-wrong ()
      (sb-thread:make-thread
       (lambda ()
         (format *standard-output* "Hello from thread!")))
      nil)
~~~

And the output:

~~~lisp
    CL-USER> (print-message-top-level-wrong)
    NIL
~~~

Print a message onto the top-level — fixed:

The code:

~~~lisp
    ;;; Print a message onto the top-level using a thread - fixed

    (defun print-message-top-level-fixed ()
      (let ((top-level *standard-output*))
        (sb-thread:make-thread
         (lambda ()
           (format top-level "Hello from thread!"))))
      nil)
~~~

And the output:

~~~lisp
    CL-USER> (print-message-top-level-fixed)
    Hello from thread!
    NIL
~~~

### Print a message onto the top-level — better

The code:

~~~lisp
    ;;; Print a message onto the top-level using a thread - reader macro

    (eval-when (:compile-toplevel)
      (defun print-message-top-level-reader-macro ()
        (sb-thread:make-thread
         (lambda ()
           (format #.*standard-output* "Hello from thread!")))
        nil))
~~~

And the output:

~~~lisp
    CL-USER> (print-message-top-level-reader-macro)
    Hello from thread!
    NIL
~~~

###    Modify a shared resource from multiple threads

The code:

~~~lisp
    ;;; Modify a shared resource from multiple threads

    (defclass bank-account ()
      ((id :initarg :id
           :initform (error "id required")
           :accessor :id)
       (name :initarg :name
             :initform (error "name required")
             :accessor :name)
       (balance :initarg :balance
                :initform 0
                :accessor :balance)))

    (defgeneric deposit (account amount)
      (:documentation "Deposit money into the account"))

    (defgeneric withdraw (account amount)
      (:documentation "Withdraw amount from account"))

    (defmethod deposit ((account bank-account) (amount real))
      (incf (:balance account) amount))

    (defmethod withdraw ((account bank-account) (amount real))
      (decf (:balance account) amount))

    (defparameter *rich*
      (make-instance 'bank-account
                     :id 1
                     :name "Rich"
                     :balance 0))

    (defun demo-race-condition ()
      (loop repeat 100
         do
           (sb-thread:make-thread
            (lambda ()
              (loop repeat 10000 do (deposit *rich* 100))
              (loop repeat 10000 do (withdraw *rich* 100))))))
~~~

And the output:

~~~lisp
    CL-USER> (:balance *rich*)
    0
    CL-USER> (demo-race-condition)
    NIL
    CL-USER> (:balance *rich*)
    3987400
~~~

###    Modify a shared resource from multiple threads — fixed using locks

The code:

~~~lisp
    (defvar *lock* (sb-thread:make-mutex))

    (defun demo-race-condition-locks ()
      (loop repeat 100
         do
           (sb-thread:make-thread
            (lambda ()
              (loop repeat 10000 do (sb-thread:with-mutex (*lock*)
                                      (deposit *rich* 100)))
              (loop repeat 10000 do (sb-thread:with-mutex (*lock*)
                                      (withdraw *rich* 100)))))))
~~~

The only difference here is that instead of make-lock as in Bordeaux,
we have make-mutex and that is used along with the macro with-mutex as
shown in the example.

And the output:

~~~lisp
    CL-USER> (:balance *rich*)
    0
    CL-USER> (demo-race-condition-locks)
    NIL
    CL-USER> (:balance *rich*)
    0
~~~

### Modify a shared resource from multiple threads — using atomic operations

First, the code:

~~~lisp
    ;;; Modify a shared resource from multiple threads - atomics

    (defgeneric atomic-deposit (account amount)
      (:documentation "Atomic version of the deposit method"))

    (defgeneric atomic-withdraw (account amount)
      (:documentation "Atomic version of the withdraw method"))

    (defmethod atomic-deposit ((account bank-account) (amount real))
      (sb-ext:atomic-incf (car (cons (:balance account) nil)) amount))

    (defmethod atomic-withdraw ((account bank-account) (amount real))
      (sb-ext:atomic-decf (car (cons (:balance account) nil)) amount))

    (defun demo-race-condition-atomics ()
      (loop repeat 100
         do (sb-thread:make-thread
             (lambda ()
               (loop repeat 10000 do (atomic-deposit *rich* 100))
               (loop repeat 10000 do (atomic-withdraw *rich* 100))))))
~~~

And the output:

~~~lisp
    CL-USER> (dotimes (i 5)
               (format t "~%Opening: ~d" (:balance *rich*))
               (demo-race-condition-atomics)
               (format t "~%Closing: ~d~%" (:balance *rich*)))

    Opening: 0
    Closing: 0

    Opening: 0
    Closing: 0

    Opening: 0
    Closing: 0

    Opening: 0
    Closing: 0

    Opening: 0
    Closing: 0
    NIL
~~~

As you can see, SBCL’s atomic functions are a bit quirky. The two
functions used here: `sb-ext:incf` and `sb-ext:atomic-decf` have the
following signatures:


    Macro: atomic-incf [sb-ext] place &optional diff

and


    Macro: atomic-decf [sb-ext] place &optional diff

The interesting bit is that the “place” parameter must be any of the
following (as per the documentation):

- a defstruct slot with declared type (unsigned-byte 64) or aref of a (simple-array (unsigned-byte 64) (*)) The type `sb-ext:word` can be used for these purposes.
- car or cdr (respectively first or REST) of a cons.
- a variable defined using defglobal with a proclaimed type of fixnum.

This is the reason for the bizarre construct used in the
`atomic-deposit` and `atomic-decf` methods.

One major incentive to use atomic operations as much as possible is
performance. Let’s do a quick run of the demo-race-condition-locks and
demo-race-condition-atomics functions over 1000 times and check the
difference in performance (if any):

With locks:

~~~lisp
    CL-USER> (time
                        (loop repeat 100
                          do (demo-race-condition-locks)))
    Evaluation took:
      57.711 seconds of real time
      431.451639 seconds of total run time (408.014746 user, 23.436893 system)
      747.61% CPU
      126,674,011,941 processor cycles
      3,329,504 bytes consed

    NIL
~~~

With atomics:

~~~lisp
    CL-USER> (time
                        (loop repeat 100
                         do (demo-race-condition-atomics)))
    Evaluation took:
      2.495 seconds of real time
      8.175454 seconds of total run time (6.124259 user, 2.051195 system)
      [ Run times consist of 0.420 seconds GC time, and 7.756 seconds non-GC time. ]
      327.66% CPU
      5,477,039,706 processor cycles
      3,201,582,368 bytes consed

    NIL
~~~

The results? The locks version took around 57s whereas the lockless
atomics version took just 2s! This is a massive difference indeed!

### Joining on a thread, destroying a thread example

The code:

~~~lisp
    ;;; Joining on and destroying a thread

    (defmacro until (condition &body body)
      (let ((block-name (gensym)))
        `(block ,block-name
           (loop
               (if ,condition
                   (return-from ,block-name nil)
                   (progn
                       ,@body))))))

    (defun join-destroy-thread ()
      (let* ((s *standard-output*)
            (joiner-thread (sb-thread:make-thread
                            (lambda ()
                              (loop for i from 1 to 10
                                 do
                                   (format s "~%[Joiner Thread]  Working...")
                                   (sleep (* 0.01 (random 100)))))))
            (destroyer-thread (sb-thread:make-thread
                               (lambda ()
                                 (loop for i from 1 to 1000000
                                    do
                                      (format s "~%[Destroyer Thread] Working...")
                                      (sleep (* 0.01 (random 10000))))))))
        (format t "~%[Main Thread] Waiting on joiner thread...")
        (bt:join-thread joiner-thread)
        (format t "~%[Main Thread] Done waiting on joiner thread")
        (if (sb-thread:thread-alive-p destroyer-thread)
            (progn
              (format t "~%[Main Thread] Destroyer thread alive... killing it")
              (sb-thread:terminate-thread destroyer-thread))
            (format t "~%[Main Thread] Destroyer thread is already dead"))
        (until (sb-thread:thread-alive-p destroyer-thread)
               (format t "[Main Thread] Waiting for destroyer thread to die..."))
        (format t "~%[Main Thread] Destroyer thread dead")
        (format t "~%[Main Thread] Adios!~%")))
~~~

And the output:

~~~lisp
    CL-USER> (join-destroy-thread)

    [Joiner Thread]  Working...
    [Destroyer Thread] Working...
    [Main Thread] Waiting on joiner thread...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Joiner Thread]  Working...
    [Main Thread] Done waiting on joiner thread
    [Main Thread] Destroyer thread alive... killing it
    [Main Thread] Destroyer thread dead
    [Main Thread] Adios!
    NIL
~~~

### Useful functions

Here is a summarised list of the functions, macros and global
variables used in the examples along with some extras:

-    `(member :thread-support *features*)` (check thread support)
-    `sb-thread:make-thread` (create a new thread)
-    `sb-thread:*current-thread*` (holds the current thread object)
-    `sb-thread:list-all-threads` (return a list of all running threads)
-    `sb-thread:thread-alive-p` (checks if the thread is still alive)
-    `sb-thread:thread-name` (return the name of the thread)
-    `sb-thread:join-thread` (join on the supplied thread)
-    `sb-thread:interrupt-thread` (interrupt the given thread)
-    `sb-thread:destroy-thread` (attempt to abort the thread)
-    `sb-thread:make-mutex` (create a mutex)
-    `sb-thread:with-mutex` (use supplied lock to protect critical code)

## Wrap-up

As you can see, concurrency support is rather primitive in Common
Lisp, but that’s primarily due to the glaring absence of this
important feature in the ANSI Common Lisp specification. That does not
detract in the least from the support provided by Common Lisp
implementations, nor wonderful libraries like the Bordeaux library.

You should follow up on your own by reading a lot more on this
topic. I share some of my own references here:

-    [Common Lisp Recipes](http://weitz.de/cl-recipes/)
-    [Bordeaux API Reference](https://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation)
-    [SBCL Manual](http://www.sbcl.org/manual/#Threading%E2%80%9D)
-    [The Common Lisp Hyperspec](https://www.lispworks.com/documentation/HyperSpec/Front/)

Next up, the final post in this mini-series: parallelism in Common
Lisp using the **lparallel** library.

## Parallel programming with lparallel

It is important to note that lparallel also provides extensive support
for asynchronous programming, and is not a purely parallel programming
library. As stated before, parallelism is merely an abstract concept
in which tasks are conceptually independent of one another.

The lparallel library is built on top of the Bordeaux threading
library.

As mentioned previously, parallelism and concurrency can be
(and usually are) implemented using the same means — threads,
processes, etc. The difference between lies in their conceptual
differences.

Note that not all the examples shown in this post are necessarily
parallel. Asynchronous constructs such as Promises and Futures are, in
particular, more suited to concurrent programming than parallel
programming.

The modus operandi of using the lparallel library (for a basic use case) is as follows:

- Create an instance of what the library calls a kernel using
  `lparallel:make-kernel`. The kernel is the component that schedules
  and executes tasks.
-    Design the code in terms of futures, promises and other higher
     level functional concepts. To this end, lparallel provides
     support for **channels**, **promises**, **futures**, and **cognates**.
-    Perform operations using what the library calls cognates, which
     are simply functions which have equivalents in the Common Lisp
     language itself. For instance, the `lparallel:pmap` function is
     the parallel equivalent of the Common Lisp `map` function.
-    Finally, close the kernel created in the first step using
     `lparallel:end-kernel`.

Note that the onus of ensuring that the tasks being carried out are
logically parallelisable as well as taking care of all mutable state
is on the developer.

_Credit: this article first appeared on
[z0ltan.wordpress.com](https://z0ltan.wordpress.com/2016/09/09/basic-concurrency-and-parallelism-in-common-lisp-part-4a-parallelism-using-lparallel-fundamentals/)._

### Installation

Let’s check if lparallel is available for download using Quicklisp:

~~~lisp
CL-USER> (ql:system-apropos "lparallel")
#<SYSTEM lparallel / lparallel-20160825-git / quicklisp 2016-08-25>
#<SYSTEM lparallel-bench / lparallel-20160825-git / quicklisp 2016-08-25>
#<SYSTEM lparallel-test / lparallel-20160825-git / quicklisp 2016-08-25>
; No value
~~~

Looks like it is. Let’s go ahead and install it:

~~~lisp
CL-USER> (ql:quickload :lparallel)
To load "lparallel":
  Load 2 ASDF systems:
    alexandria bordeaux-threads
  Install 1 Quicklisp release:
    lparallel
; Fetching #<URL "http://beta.quicklisp.org/archive/lparallel/2016-08-25/lparallel-20160825-git.tgz">
; 76.71KB
==================================================
78,551 bytes in 0.62 seconds (124.33KB/sec)
; Loading "lparallel"
[package lparallel.util]..........................
[package lparallel.thread-util]...................
[package lparallel.raw-queue].....................
[package lparallel.cons-queue]....................
[package lparallel.vector-queue]..................
[package lparallel.queue].........................
[package lparallel.counter].......................
[package lparallel.spin-queue]....................
[package lparallel.kernel]........................
[package lparallel.kernel-util]...................
[package lparallel.promise].......................
[package lparallel.ptree].........................
[package lparallel.slet]..........................
[package lparallel.defpun]........................
[package lparallel.cognate].......................
[package lparallel]
(:LPARALLEL)
~~~

And that’s all it took! Now let’s see how this library actually works.

### Preamble - get the number of cores with a call to CFFI

First, let’s get hold of the number of threads that we are going to
use for our parallel examples. Ideally, we’d like to have a 1:1 match
between the number of worker threads and the number of available
cores.

We can use the wonderful **cffi** library to this end. I plan to have a
detailed blog post for this extremely useful library soon, but for
now, let’s get on with it:

Install CFFI:

~~~lisp
CL-USER> (ql:quickload :cffi)
To load "cffi":
  Load 4 ASDF systems:
    alexandria babel trivial-features uiop
  Install 1 Quicklisp release:
    cffi
; Fetching #<URL "http://beta.quicklisp.org/archive/cffi/2016-03-18/cffi_0.17.1.tgz">
; 234.48KB
==================================================
240,107 bytes in 5.98 seconds (39.22KB/sec)
; Loading "cffi"
[package cffi-sys]................................
[package cffi]....................................
..................................................
[package cffi-features]
(:CFFI)
~~~

Write C code to get the number of logical cores on the machine:

```
#include <stdio.h>
#include <sys/types.h>
#include <sys/sysctl.h>

int get_core_count();

int main()
{
    printf("%d\n", get_core_count());

    return 0;
}

int32_t get_core_count()
{
    const char* s = "hw.logicalcpu";
    int32_t core_count;
    size_t len = sizeof(core_count);

    sysctlbyname(s, &core_count, &len, NULL, 0);

    return core_count;
}
```

Bundle the C code into a shared library (note, I am using Mac OS X
which comes bundled with Clang. For pure gcc, refer to the relevant
documentation): 1

```
Timmys-MacBook-Pro:Parallelism z0ltan$ clang -dynamiclib get_core_count.c -o libcorecount.dylib
```

Invoke the function from Common Lisp:

~~~lisp
CL-USER> (cffi:use-foreign-library "libcorecount.dylib")
#<CFFI:FOREIGN-LIBRARY LIBCORECOUNT.DYLIB-853 "libcorecount.dylib">
CL-USER> (cffi:foreign-funcall "get_core_count" :int)
8
~~~

We can see that the result is 8 cores on the machine (which is
correct) and can be verified from the command line as well:

```
Timmys-MacBook-Pro:Parallelism z0ltan$ sysctl -n "hw.logicalcpu"
```

### Common Setup

In this example, we will go through the initial setup bit, and also
show some useful information once the setup is done.

Load the library:

~~~lisp
CL-USER> (ql:quickload :lparallel)
To load "lparallel":
  Load 1 ASDF system:
    lparallel
; Loading "lparallel"

(:LPARALLEL)
~~~

Initialise the lparallel kernel:

~~~lisp
CL-USER> (setf lparallel:*kernel* (lparallel:make-kernel 8 :name "custom-kernel"))
#<LPARALLEL.KERNEL:KERNEL :NAME "custom-kernel" :WORKER-COUNT 8 :USE-CALLER NIL :ALIVE T :SPIN-COUNT 2000 {1003141F03}>
~~~

Note that the `*kernel*` global variable can be rebound — this allows
multiple kernels to co-exist during the same run. Now, some useful
information about the kernel:

~~~lisp
CL-USER> (defun show-kernel-info ()
           (let ((name (lparallel:kernel-name))
                 (count (lparallel:kernel-worker-count))
                 (context (lparallel:kernel-context))
                 (bindings (lparallel:kernel-bindings)))
             (format t "Kernel name = ~a~%" name)
             (format t "Worker threads count = ~d~%" count)
             (format t "Kernel context = ~a~%" context)
             (format t "Kernel bindings = ~a~%" bindings)))


WARNING: redefining COMMON-LISP-USER::SHOW-KERNEL-INFO in DEFUN
SHOW-KERNEL-INFO

CL-USER> (show-kernel-info)
Kernel name = custom-kernel
Worker threads count = 8
Kernel context = #<FUNCTION FUNCALL>
Kernel bindings = ((*STANDARD-OUTPUT* . #<SLIME-OUTPUT-STREAM {10044EEEA3}>)
                   (*ERROR-OUTPUT* . #<SLIME-OUTPUT-STREAM {10044EEEA3}>))
NIL
~~~

End the kernel (this is important since `*kernel*` does not get
garbage collected until we explicitly end it):

~~~lisp
CL-USER> (lparallel:end-kernel :wait t)
(#<SB-THREAD:THREAD "custom--kernel" FINISHED values: NIL {100723FA83}>
 #<SB-THREAD:THREAD "custom--kernel" FINISHED values: NIL {100723FE23}>
 #<SB-THREAD:THREAD "custom--kernel" FINISHED values: NIL {10072581E3}>
 #<SB-THREAD:THREAD "custom--kernel" FINISHED values: NIL {1007258583}>
 #<SB-THREAD:THREAD "custom--kernel" FINISHED values: NIL {1007258923}>
 #<SB-THREAD:THREAD "custom--kernel" FINISHED values: NIL {1007258CC3}>
 #<SB-THREAD:THREAD "custom--kernel" FINISHED values: NIL {1007259063}>
 #<SB-THREAD:THREAD "custom--kernel" FINISHED values: NIL {1007259403}>)
~~~

Let’s move on to some more examples of different aspects of the lparallel library.

For these demos, we will be using the following initial setup from a coding perspective:

~~~lisp
(require ‘lparallel)
(require ‘bt-semaphore)

(defpackage :lparallel-user
  (:use :cl :lparallel :lparallel.queue :bt-semaphore))

(in-package :lparallel-user)

;;; initialise the kernel
(defun init ()
  (setf *kernel* (make-kernel 8 :name "channel-queue-kernel")))

(init)
~~~

So we will be using a kernel with 8 worker threads (one for each CPU core on the machine).

And once we’re done will all the examples, the following code will be
run to close the kernel and free all used system resources:

~~~lisp
;;; shut the kernel down
(defun shutdown ()
  (end-kernel :wait t))

(shutdown)
~~~

### Using channels and queues

First some definitions are in order.

A **task** is a job that is submitted to the kernel. It is simply a
function object along with its arguments.

A **channel** in lparallel is similar to the same concept in Go. A channel
is simply a means of communication with a worker thread. In our case,
it is one particular way of submitting tasks to the kernel.

A channel is created in lparallel using `lparallel:make-channel`. A
task is submitted using `lparallel:submit-task`, and the results
received via `lparallel:receive-result`.

For instance, we can calculate the square of a number as:

~~~lisp
(defun calculate-square (n)
  (let* ((channel (lparallel:make-channel))
         (res nil))
    (lparallel:submit-task channel #'(lambda (x)
                                       (* x x))
                           n)
    (setf res (lparallel:receive-result channel))
    (format t "Square of ~d = ~d~%" n res)))
~~~

And the output:

~~~lisp
LPARALLEL-USER> (calculate-square 100)
Square of 100 = 10000
NIL
~~~

Now let’s try submitting multiple tasks to the same channel. In this
simple example, we are simply creating three tasks that square, triple,
and quadruple the supplied input respectively.

Note that in case of multiple tasks, the output will be in non-deterministic order:

~~~lisp
(defun test-basic-channel-multiple-tasks ()
  (let ((channel (make-channel))
        (res '()))
    (submit-task channel #'(lambda (x)
                             (* x x))
                 10)
    (submit-task channel #'(lambda (y)
                             (* y y y))
                 10)
    (submit-task channel #'(lambda (z)
                             (* z z z z))
                 10)
     (dotimes (i 3 res)
       (push (receive-result channel) res))))
~~~

And the output:

~~~lisp
LPARALLEL-USER> (dotimes (i 3)
                              (print (test-basic-channel-multiple-tasks)))

(100 1000 10000)
(100 1000 10000)
(10000 1000 100)
NIL
~~~

lparallel also provides support for creating a blocking queue in order
to enable message passing between worker threads. A queue is created
using `lparallel.queue:make-queue`.

Some useful functions for using queues are:

-    `lparallel.queue:make-queue`: create a FIFO blocking queue
-    `lparallel.queue:push-queue`: insert an element into the queue
-    `lparallel.queue:pop-queue`: pop an item from the queue
-    `lparallel.queue:peek-queue`: inspect value without popping it
-    `lparallel.queue:queue-count`: the number of entries in the queue
-    `lparallel.queue:queue-full-p`: check if the queue is full
-    `lparallel.queue:queue-empty-p:chec`k if the queue is empty
-    `lparallel.queue:with-locked-queue`: lock the queue during access

A basic demo showing basic queue properties:

~~~lisp
    (defun test-queue-properties ()
      (let ((queue (make-queue :fixed-capacity 5)))
        (loop
           when (queue-full-p queue)
           do (return)
           do (push-queue (random 100) queue))
         (print (queue-full-p queue))
        (loop
           when (queue-empty-p queue)
           do (return)
           do (print (pop-queue queue)))
        (print (queue-empty-p queue)))
      nil)
~~~

Which produces:

~~~lisp
    LPARALLEL-USER> (test-queue-properties)

    T
    17
    51
    55
    42
    82
    T
    NIL
~~~

Note: `lparallel.queue:make-queue` is a generic interface which is
actually backed by different types of queues. For instance, in the
previous example, the actual type of the queue is
`lparallel.vector-queue` since we specified it to be of fixed size using
the `:fixed-capacity` keyword argument.

The documentation doesn’t actually specify what keyword arguments we
can pass to `lparallel.queue:make-queue`, so let’s and find that out in
a different way:

~~~lisp
    LPARALLEL-USER> (describe 'lparallel.queue:make-queue)
    LPARALLEL.QUEUE:MAKE-QUEUE
      [symbol]

    MAKE-QUEUE names a compiled function:
      Lambda-list: (&REST ARGS)
      Derived type: FUNCTION
      Documentation:
        Create a queue.

        The queue contents may be initialized with the keyword argument
        `initial-contents'.

        By default there is no limit on the queue capacity. Passing a
        `fixed-capacity' keyword argument limits the capacity to the value
        passed. `push-queue' will block for a full fixed-capacity queue.
      Source file: /Users/z0ltan/quicklisp/dists/quicklisp/software/lparallel-20160825-git/src/queue.lisp

    MAKE-QUEUE has a compiler-macro:
      Source file: /Users/z0ltan/quicklisp/dists/quicklisp/software/lparallel-20160825-git/src/queue.lisp
    ; No value
~~~

So, as we can see, it supports the following keyword arguments:
*:fixed-capacity*, and *initial-contents*.

Now, if we do specify `:fixed-capacity`, then the actual type of the
queue will be `lparallel.vector-queue`, and if we skip that keyword
argument, the queue will be of type `lparallel.cons-queue` (which is a
queue of unlimited size), as can be seen from the output of the
following snippet:

~~~lisp
    (defun check-queue-types ()
      (let ((queue-one (make-queue :fixed-capacity 5))
            (queue-two (make-queue)))
        (format t "queue-one is of type: ~a~%" (type-of queue-one))
        (format t "queue-two is of type: ~a~%" (type-of queue-two))))


    LPARALLEL-USER> (check-queue-types)
    queue-one is of type: VECTOR-QUEUE
    queue-two is of type: CONS-QUEUE
    NIL
~~~

Of course, you can always create instances of the specific queue types
yourself, but it is always better, when you can, to stick to the
generic interface and letting the library create the proper type of
queue for you.

Now, let’s just see the queue in action!

~~~lisp
    (defun test-basic-queue ()
      (let ((queue (make-queue))
            (channel (make-channel))
            (res '()))
        (submit-task channel #'(lambda ()
                         (loop for entry = (pop-queue queue)
                            when (queue-empty-p queue)
                            do (return)
                            do (push (* entry entry) res))))
        (dotimes (i 100)
          (push-queue i queue))
        (receive-result channel)
        (format t "~{~d ~}~%" res)))
~~~

Here we submit a single task that repeatedly scans the queue till it’s
empty, pops the available values, and pushes them into the res list.

And the output:

~~~lisp
    LPARALLEL-USER> (test-basic-queue)
    9604 9409 9216 9025 8836 8649 8464 8281 8100 7921 7744 7569 7396 7225 7056 6889 6724 6561 6400 6241 6084 5929 5776 5625 5476 5329 5184 5041 4900 4761 4624 4489 4356 4225 4096 3969 3844 3721 3600 3481 3364 3249 3136 3025 2916 2809 2704 2601 2500 2401 2304 2209 2116 2025 1936 1849 1764 1681 1600 1521 1444 1369 1296 1225 1156 1089 1024 961 900 841 784 729 676 625 576 529 484 441 400 361 324 289 256 225 196 169 144 121 100 81 64 49 36 25 16 9 4 1 0
    NIL
~~~

###    Killing tasks

A small note mentioning the `lparallel:kill-task` function would be
apropos at this juncture. This function is useful in those cases when
tasks are unresponsive. The lparallel documentation clearly states
that this must only be used as a last resort.

All tasks which are created are by default assigned a category of
:default. The dynamic property, `*task-category*` holds this value, and
can be dynamically bound to different values (as we shall see).

~~~lisp
;;; kill default tasks
(defun test-kill-all-tasks ()
  (let ((channel (make-channel))
        (stream *query-io*))
    (dotimes (i 10)
      (submit-task channel #'(lambda (x)
                               (sleep (random 10))
                               (format stream "~d~%" (* x x))) (random 10)))
    (sleep (random 2))
    (kill-tasks :default)))
~~~

Sample run:

~~~lisp
LPARALLEL-USER> (test-kill-all-tasks)
16
1
8
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.
~~~

Since we had created 10 tasks, all the 8 kernel worker threads were
presumably busy with a task each. When we killed tasks of category
:default, all these threads were killed as well and had to be
regenerated (which is an expensive operation). This is part of the
reason why `lparallel:kill-tasks` must be avoided.

Now, in the example above, all running tasks were killed since all of
them belonged to the :default category. Suppose we wish to kill only
specific tasks, we can do that by binding `*task-category*` when we
create those tasks, and then specifying the category when we invoke
`lparallel:kill-tasks`.

For example, suppose we have two categories of tasks – tasks which
square their arguments, and tasks which cube theirs. Let’s assign them
categories ’squaring-tasks and ’cubing-tasks respectively. Let’s then
kill tasks of a randomly chosen category ’squaring-tasks or
’cubing-tasks.

Here is the code:

~~~lisp
;;; kill tasks of a randomly chosen category
(defun test-kill-random-tasks ()
  (let ((channel (make-channel))
        (stream *query-io*))
    (let ((*task-category* 'squaring-tasks))
      (dotimes (i 5)
        (submit-task channel #'(lambda (x)
                                 (sleep (random 5))
                                 (format stream "~%[Squaring] ~d = ~d" x (* x x))) i)))
    (let ((*task-category* 'cubing-tasks))
      (dotimes (i 5)
        (submit-task channel #'(lambda (x)
                                 (sleep (random 5))
                                 (format stream "~%[Cubing] ~d = ~d" x (* x x x))) i)))
    (sleep 1)
    (if (evenp (random 10))
        (progn
          (print "Killing squaring tasks")
          (kill-tasks 'squaring-tasks))
        (progn
          (print "Killing cubing tasks")
          (kill-tasks 'cubing-tasks)))))
~~~

And here is a sample run:

~~~lisp
LPARALLEL-USER> (test-kill-random-tasks)

[Cubing] 2 = 8
[Squaring] 4 = 16
[Cubing] 4
 = [Cubing] 643 = 27
"Killing squaring tasks"
4
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.

[Cubing] 1 = 1
[Cubing] 0 = 0

LPARALLEL-USER> (test-kill-random-tasks)

[Squaring] 1 = 1
[Squaring] 3 = 9
"Killing cubing tasks"
5
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.

[Squaring] 2 = 4
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.

[Squaring] 0 = 0
[Squaring] 4 = 16
~~~

### Using promises and futures

Promises and Futures provide support for Asynchronous Programming.

In lparallel-speak, a `lparallel:promise` is a placeholder for a
result which is fulfilled by providing it with a value. The promise
object itself is created using `lparallel:promise`, and the promise is
given a value using the `lparallel:fulfill` macro.

To check whether the promise has been fulfilled yet or not, we can use
the `lparallel:fulfilledp` predicate function.  Finally, the
`lparallel:force` function is used to extract the value out of the
promise. Note that this function blocks until the operation is
complete.

Let’s solidify these concepts with a very simple example first:

~~~lisp
(defun test-promise ()
  (let ((p (promise)))
    (loop
       do (if (evenp (read))
              (progn
                (fulfill p 'even-received!)
                (return))))
    (force p)))
~~~

Which generates the output:

~~~lisp
LPARALLEL-USER> (test-promise)
5
1
3
10
EVEN-RECEIVED!
~~~

Explanation: This simple example simply keeps looping forever until an
even number has been entered. The promise is fulfilled inside the loop
using `lparallel:fulfill`, and the value is then returned from the
function by forcing it with `lparallel:force`.

Now, let’s take a bigger example. Assuming that we don’t want to have
to wait for the promise to be fulfilled, and instead have the current
do some useful work, we can delegate the promise fulfillment to
external explicitly as seen in the next example.

Consider we have a function that squares its argument. And, for the
sake of argument, it consumes a lot of time doing so. From our client
code, we want to invoke it, and wait till the squared value is
available.


~~~lisp
(defun promise-with-threads ()
  (let ((p (promise))
        (stream *query-io*)
        (n (progn
             (princ "Enter a number: ")
             (read))))
    (format t "In main function...~%")
    (bt:make-thread
     #'(lambda ()
         (sleep (random 10))
         (format stream "Inside thread... fulfilling promise~%")
         (fulfill p (* n n))))
    (bt:make-thread
     #'(lambda ()
         (loop
            when (fulfilledp p)
            do (return)
            do (progn
                 (format stream "~d~%" (random 100))
                 (sleep (* 0.01 (random 100)))))))
    (format t "Inside main function, received value: ~d~%" (force p))))
~~~

And the output:

~~~lisp
LPARALLEL-USER> (promise-with-threads)
Enter a number: 19
In main function...
44
59
90
34
30
76
Inside thread... fulfilling promise
Inside main function, received value: 361
NIL
~~~

Explanation: There is nothing much in this example. We create a
promise object p, and we spawn off a thread that sleeps for some
random time and then fulfills the promise by giving it a value.

Meanwhile, in the main thread, we spawn off another thread that keeps
checking if the promise has been fulfilled or not. If not, it prints
some random number and continues checking. Once the promise has been
fulfilled, we can extract the value using `lparallel:force` in the main
thread as shown.

This shows that promises can be fulfilled by different threads while
the code that created the promise need not wait for the promise to be
fulfilled. This is especially important since, as mentioned before,
`lparallel:force` is a blocking call. We want to delay forcing the
promise until the value is actually available.

Another point to note when using promises is that once a promise has
been fulfilled, invoking force on the same object will always return
the same value. That is to say, a promise can be successfully
fulfilled only once.

For instance:

~~~lisp
(defun multiple-fulfilling ()
  (let ((p (promise)))
    (dotimes (i 10)
      (fulfill p (random 100))
      (format t "~d~%" (force p)))))
~~~

Which produces:

~~~lisp
LPARALLEL-USER> (multiple-fulfilling)
15
15
15
15
15
15
15
15
15
15
NIL
~~~

So how does a future differ from a promise?

A `lparallel:future` is simply a promise that is run in parallel, and as
such, it does not block the main thread like a default use of
`lparallel:promise` would. It is executed in its own thread (by
the lparallel library, of course).

Here is a simple example of a future:

~~~lisp
(defun test-future ()
  (let ((f (future
             (sleep (random 5))
             (print "Hello from future!"))))
    (loop
       when (fulfilledp f)
       do (return)
       do (sleep (* 0.01 (random 100)))
         (format t "~d~%" (random 100)))
    (format t "~d~%" (force f))))
~~~

And the output:

~~~lisp
LPARALLEL-USER> (test-future)
5
19
91
11
Hello from future!
NIL
~~~

Explanation: This exactly is similar to the `promise-with-threads`
example. Observe two differences, however - first of all, the
`lparallel:future` macro has a body as well. This allows the future to
fulfill itself! What this means is that as soon as the body of the
future is done executing, `lparallel:fulfilledp` will always return true
for the future object.

Secondly, the future itself is spawned off on a separate thread by the
library, so it does not interfere with the execution of the current
thread very much unlike promises as could be seen in the
promise-with-threads example (which needed an explicit thread for the
fulfilling code in order to avoid blocking the current thread).

The most interesting bit is that (even in terms of the actual theory
propounded by Dan Friedman and others), a Future is conceptually
something that fulfills a Promise. That is to say, a promise is a
contract that some value will be generated sometime in the future, and
a future is precisely that “something” that does that job.

What this means is that even when using the lparallel library, the
basic use of a future would be to fulfill a promise. This means that
hacks like promise-with-threads need not be made by the user.

Let’s take a small example to demonstrate this point (a pretty
contrived example, I must admit!).

Here’s the scenario: we want to read in a number and calculate its
square. So we offload this work to another function, and continue with
our own work. When the result is ready, we want it to be printed on
the console without any intervention from us.

Here’s how the code looks:

~~~lisp
;;; Callback example using promises and futures
(defun callback-promise-future-demo ()
  (let* ((p (promise))
         (stream *query-io*)
         (n (progn
              (princ "Enter a number: ")
              (read)))
         (f (future
              (sleep (random 10))
              (fulfill p (* n n))
              (force (future
                       (format stream "Square of ~d = ~d~%" n (force p)))))))
    (loop
       when (fulfilledp f)
       do (return)
       do (sleep (* 0.01 (random 100))))))
~~~

And the output:

~~~lisp
LPARALLEL-USER> (callback-promise-future-demo)
Enter a number: 19
Square of 19 = 361
NIL
~~~

Explanation: All right, so first off, we create a promise to hold the
squared value when it is generated. This is the p object. The input
value is stored in the local variable n.

Then we create a future object f. This future simply squares the input
value and fulfills the promise with this value. Finally, since we want
to print the output in its own time, we force an anonymous future
which simply prints the output string as shown.

Note that this is very similar to the situation in an environment like
Node, where we pass callback functions to other functions with the
understanding that the callback will be called when the invoked
function is done with its work.

Finally note that the following snippet is still fine (even if it uses
the blocking `lparallel:force` call because it’s on a separate thread):


~~~lisp
(force (future
(format stream "Square of ~d = ~d~%" n (force p))))
~~~

To summarise, the general idiom of usage is: **define objects which will
hold the results of asynchronous computations in promises, and use
futures to fulfill those promises**.

### Using cognates - parallel equivalents of Common Lisp counterparts

Cognates are arguably the raison d’etre of the lparallel
library. These constructs are what truly provide parallelism in the
lparallel. Note, however, that most (if not all) of these constructs
are built on top of futures and promises.

To put it in a nutshell, cognates are simply functions that are
intended to be the parallel equivalents of their Common Lisp
counterparts. However, there are a few extra lparallel cognates that
have no Common Lisp equivalents.

At this juncture, it is important to know that cognates come in two basic flavours:

-    Constructs for fine-grained parallelism: `defpun`, `plet`, `plet-if`, etc.
-   Explicit functions and macros for performing parallel operations -
    `pmap`, `preduce`, `psort`, `pdotimes`, etc.

In the first case we don’t have much explicit control over the
operations themselves. We mostly rely on the fact that the library
itself will optimise and parallelise the forms to whatever extent it
can. In this post, we will focus on the second category of cognates.

Take, for instance, the cognate function `lparallel:pmap` is exactly
the same as the Common Lisp equivalent, `map`, but it runs in
parallel. Let’s demonstrate that through an example.

Suppose we had a list of random strings of length varying from 3 to
10, and we wished to collect their lengths in a vector.

Let’s first set up the helper functions that will generate the random strings:

~~~lisp
(defvar *chars*
  (remove-duplicates
   (sort
    (loop for c across "The quick brown fox jumps over the lazy dog"
       when (alpha-char-p c)
       collect (char-downcase c))
    #'char<)))

(defun get-random-strings (&optional (count 100000))
  "generate random strings between lengths 3 and 10"
  (loop repeat count
     collect
       (concatenate 'string  (loop repeat (+ 3 (random 8))
                           collect (nth (random 26) *chars*)))))
~~~

And here’s how the Common Lisp map version of the solution might look like:

~~~lisp
;;; map demo
(defun test-map ()
  (map 'vector #'length (get-random-strings 100)))
~~~

And let’s have a test run:

~~~lisp
LPARALLEL-USER> (test-map)
#(7 5 10 8 7 5 3 4 4 10)
~~~

And here’s the `lparallel:pmap` equivalent:

~~~lisp
;;;pmap demo
(defun test-pmap ()
  (pmap 'vector #'length (get-random-strings 100)))
~~~

which produces:

~~~lisp
LPARALLEL-USER> (test-pmap)
#(8 7 6 7 6 4 5 6 5 7)
LPARALLEL-USER>
~~~

As you can see from the definitions of test-map and test-pmap, the
syntax of the `lparallel:map` and `lparallel:pmap` functions are exactly
the same (well, almost - `lparallel:pmap` has a few more optional
arguments).

Some useful cognate functions and macros (all of them are functions
except when marked so explicitly. Note that there are quite a few
cognates, and I have chosen a few to try and represent every category
through an example:

#### lparallel:pmap: parallel version of map.

Note that all the mapping functions (`lparallel:pmap`,
**lparallel:pmapc**,`lparallel:pmapcar`, etc.) take two special keyword
arguments
- `:size`, specifying the number of elements of the input
sequence(s) to process, and
- `:parts` which specifies the number of parallel parts to divide the
sequence(s) into.

~~~lisp
    ;;; pmap - function
    (defun test-pmap ()
      (let ((numbers (loop for i below 10
                        collect i)))
        (pmap 'vector #'(lambda (x)
                          (* x x))
              :parts (length numbers)
              numbers)))
~~~

Sample run:

~~~lisp
    LPARALLEL-USER> (test-pmap)

    #(0 1 4 9 16 25 36 49 64 81)
~~~

#### lparallel:por: parallel version of or.

The behaviour is that it returns the first non-nil element amongst its
arguments. However, due to the parallel nature of this macro, that
element varies.


~~~lisp
    ;;; por - macro
    (defun test-por ()
      (let ((a 100)
            (b 200)
            (c nil)
            (d 300))
        (por a b c d)))
~~~

Sample run:

~~~lisp
    LPARALLEL-USER> (dotimes (i 10)
                      (print (test-por)))

    300
    300
    100
    100
    100
    300
    100
    100
    100
    100
    NIL
~~~

In the case of the normal or operator, it would always have returned
the first non-nil element viz. 100.


#### lparallel:pdotimes: parallel version of dotimes.

Note that this macro also take an optional `:parts` argument.


~~~lisp
    ;;; pdotimes - macro
    (defun test-pdotimes ()
      (pdotimes (i 5)
        (declare (ignore i))
        (print (random 100))))
~~~

Sample run:

~~~lisp
    LPARALLEL-USER> (test-pdotimes)

    39
    29
    81
    42
    56
    NIL
~~~

####  lparallel:pfuncall: parallel version of funcall.


~~~lisp
    ;;; pfuncall - macro
    (defun test-pfuncall ()
      (pfuncall #'* 1 2 3 4 5))
~~~

Sample run:

~~~lisp
    LPARALLEL-USER> (test-pfuncall)

    120
~~~

####    lparallel:preduce: parallel version of reduce.

This very important function also takes two optional keyword
arguments:  `:parts` (same meaning as explained), and `:recurse`. If
`:recurse` is non-nil, it recursively applies `lparallel:preduce` to its
arguments, otherwise it default to using reduce.

~~~lisp
    ;;; preduce - function
    (defun test-preduce ()
      (let ((numbers (loop for i from 1 to 100
                        collect i)))
        (preduce #'+
                 numbers
                 :parts (length numbers)
                 :recurse t)))
~~~

Sample run:

~~~lisp
    LPARALLEL-USER> (test-preduce)

    5050
~~~

####    lparallel:premove-if-not: parallel version of remove-if-not.

This is essentially equivalent to “filter” in Functional Programming parlance.


~~~lisp
    ;;; premove-if-not
    (defun test-premove-if-not ()
      (let ((numbers (loop for i from 1 to 100
                        collect i)))
        (premove-if-not #'evenp numbers)))
~~~

Sample run:

~~~lisp
    LPARALLEL-USER> (test-premove-if-not)

    (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54
     56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100)
~~~

####    lparallel:pevery: parallel version of every.


~~~lisp
    ;;; pevery - function
    (defun test-pevery ()
      (let ((numbers (loop for i from 1 to 100
                        collect i)))
        (list (pevery #'evenp numbers)
              (pevery #'integerp numbers))))
~~~

Sample run:

~~~lisp
    LPARALLEL-USER> (test-pevery)

    (NIL T)
~~~

In this example, we are performing two checks - firstly, whether all
the numbers in the range [1,100] are even, and secondly, whether all
the numbers in the same range are integers.

#### lparallel:count: parallel version of count.

~~~lisp
    ;;; pcount - function
    (defun test-pcount ()
      (let ((chars "The quick brown fox jumps over the lazy dog"))
        (pcount #\e chars)))
~~~

Sample run:

~~~lisp
    LPARALLEL-USER> (test-pcount)

    3
~~~

####    lparallel:psort: parallel version of sort.


~~~lisp
    ;;; psort - function
    (defstruct person
      name
      age)

    (defun test-psort ()
      (let* ((names (list "Rich" "Peter" "Sybil" "Basil" "Candy" "Slava" "Olga"))
             (people (loop for name in names
                        collect (make-person :name name :age (+ (random 20) 20)))))
        (print "Before sorting...")
        (print people)
        (fresh-line)
        (print "After sorting...")
        (psort
         people
         #'(lambda (x y)
             (< (person-age x)
                (person-age y)))
         :test #'=)))
~~~

Sample run:

~~~lisp
    LPARALLEL-USER> (test-psort)

    "Before sorting..."
    (#S(PERSON :NAME "Rich" :AGE 38) #S(PERSON :NAME "Peter" :AGE 24)
     #S(PERSON :NAME "Sybil" :AGE 20) #S(PERSON :NAME "Basil" :AGE 22)
     #S(PERSON :NAME "Candy" :AGE 23) #S(PERSON :NAME "Slava" :AGE 37)
     #S(PERSON :NAME "Olga" :AGE 33))

    "After sorting..."
    (#S(PERSON :NAME "Sybil" :AGE 20) #S(PERSON :NAME "Basil" :AGE 22)
     #S(PERSON :NAME "Candy" :AGE 23) #S(PERSON :NAME "Peter" :AGE 24)
     #S(PERSON :NAME "Olga" :AGE 33) #S(PERSON :NAME "Slava" :AGE 37)
     #S(PERSON :NAME "Rich" :AGE 38))
~~~

In this example, we first define a structure of type person for
storing information about people. Then we create a list of 7 people
with randomly generated ages (between 20 and 39). Finally, we sort
them by age in non-decreasing order.

### Error handling

To see how lparallel handles error handling (hint: with
`lparallel:task-handler-bind`), please read
[https://z0ltan.wordpress.com/2016/09/10/basic-concurrency-and-parallelism-in-common-lisp-part-4b-parallelism-using-lparallel-error-handling/](https://z0ltan.wordpress.com/2016/09/10/basic-concurrency-and-parallelism-in-common-lisp-part-4b-parallelism-using-lparallel-error-handling/).


## Monitoring and controlling threads with Slime

**M-x slime-list-threads** (you can also access it through the
*slime-selector*, shortcut **t**) will list running threads by their
names, and their statuses.

The thread on the current line can be killed with **k**, or if there’s a
lot of threads to kill, several lines can be selected and **k** will kill
all the threads in the selected region.

**g** will update the thread list, but when you have a lot of threads
starting and stopping it may be too cumbersome to always press **g**, so
there’s a variable `slime-threads-update-interval`, when set to a number
X the thread list will be automatically updated each X seconds, a
reasonable value would be 0.5.

Thanks to [Slime tips](https://slime-tips.tumblr.com/).


## References

There are, of course, a lot more functions, objects, and idiomatic
ways of performing parallel computations using the lparallel
library. This post barely scratches the surface on those. However, the
general flow of operation is amply demonstrated here, and for further
reading, you may find the following resources useful:

- [The API docs hosted on Quickdoc](http://quickdocs.org/lparallel/api#package-LPARALLEL)
- [The official homepage of the lparallel library](https://lparallel.org/)
- [The Common Lisp Hyperspec](https://www.lispworks.com/documentation/HyperSpec/Front/), and, of course
- Your Common Lisp implementation’s
  manual. [For SBCL, here is a link to the official manual](https://www.lispworks.com/documentation/HyperSpec/Front/)
- [Common Lisp recipes](http://weitz.de/cl-recipes/) by the venerable Edi Weitz.
