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
library.

<a name="why_bother"></a>

### Why bother?

The first question to resolve is: why bother with threads? Sometimes
your answer will simply be that your application is so straightforward
that you need not concern yourself with threads at all. But in many
other cases it's difficult to imagine how a sophisticated application
can be written without multi-threading. For example:

*   you might be writing a server which needs to be able to respond to
    more than one user / connection at a time (for instance: a web
    server - see [this example](./sockets.html#server) on the Sockets
    page);
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
###  Basics — list current thread, list all threads, get thread name

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

We create a new thread using bt:make-thread, which takes a lambda
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

As we can see, because the main thread returned immediately, the initial value of *counter* is 0, and then around a second later, it gets updated to 1 by the anonymous thread.

### Print a message onto the top-level using a thread

~~~lisp
    ;;; Print a message onto the top-level using a thread

    (defun print-message-top-level-wrong ()
      (bt:make-thread
       (lambda ()
         (format *standard-output* "Hello from thread!")))
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
           (format top-level "Hello from thread!"))))
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

The `#.` symbol is what is called a “Reader macro”. (I will be posting
a whole post (or maybe a series of posts!) on reader macros in the
future). A reader (or read) macro is called so because it has special
meaning to the Common Lisp Reader, which is the component that is
responsible for reading in Common Lisp expressions and making sense
out of them. This specific reader macro ensures that the binding of
`*standard-output*` is done at read time.

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

I have already posted a comprehensive post on eval-when and other advanced Common Lisp constructs. You can find them with a simple search in my blog (use the search box).

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

To join on a thread, we use the bt:join-thread function, and for
destroying a thread (not a recommended operation), we can use the
bt:destroy-thread function.

A simple demo:

~~~lisp
    (defmacro until (condition &body body)
      (let ((block-name (gensym)))
        `(block ,block-name
           (if ,condition
               (return-from ,block-name nil)
               (progn
                 ,@body)))))

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

Again, it is not recommended to use bt:destroy-thread. Any conceivable
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

SBCL provides support for native threads via its **sb-thread**
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

- a defstruct slot with declared type (unsigned-byte 64) or aref of a (simple-array (unsigned-byte 64) (*)) The type sb-ext:word can be used for these purposes.
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
           (if ,condition
               (return-from ,block-name nil)
               (progn
                 ,@body)))))

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
-    [Bordeaux API Reference](https://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation%E2%80%9D)
-    [SBCL Manual](https://www.sbcl.org/manual/#Threading%E2%80%9D)
-    [The Common Lisp Hyperspec](https://www.lispworks.com/documentation/HyperSpec/Front/)

Next up, the final post in this mini-series: parallelism in Common
Lisp using the **lparallel** library.
