---
title: Threads
---


<a name="intro"></a>

## Introduction

By _threads_, I mean separate execution strands within a single Lisp process, sharing the same address space. Typically, execution is automatically switched between these strands by the system (either by the lisp kernel or by the operating system) so that tasks appear to be completed in parallel (asynchronously). This page discusses the creation and management of threads and some aspects of interactions between them. For information about the interaction between lisp and other _processes_, see [Interfacing with your OS](os.html).

Unfortunately, an instant pitfall for the unwary is that most implementations refer (in nomenclature) to threads as _processes_ - this is a historical feature of a language which has been around for much longer than the term _thread_. Call this maturity a sign of stable implementations, if you will.

The ANSI Common Lisp standard doesn't mention this topic. So almost everything that can be said here depends on your OS and your implementation. It's not just a question of different symbols in different packages (although that might be enough to get you started, so see the [acl-compat](http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/portableaserve/portableaserve/acl-compat/) package at Sourceforge and in particular the files whose names match "acl-mp-*.lisp") - concepts can vary too.

Speaking of implementations, the following discussion currently refers only to [LispWorks](http://www.lispworks.com/). It has been tested on LispWorks for Windows Professional version 4.2.6 running on Windows NT (SP 6). All the examples below (apart from those few which require resaving the lisp image) should work without difficulty on the (free) Personal edition running under either Linux or Windows.

Your mileage may vary. I will add "support" for other implementations and operating systems to this page later, if I have the time and access to the appropriate materials.

In any case, read the manual! In this instance, the manual concerned is the chapter on "The MP Package" in the "LispWorks Reference Manual".

Pre-requisite: I assume familiarity with Common Lisp, in particular with closures, the loop macro, and lambda forms.


<a name="why_bother"></a>

## Why bother?

The first question to resolve is: why bother with threads? Sometimes your answer will simply be that your application is so straightforward that you need not concern yourself with threads at all. But in many other cases it's difficult to imagine how a sophisticated application can be written without multi-threading. For example:

*   you might be writing a server which needs to be able to respond to more than one user / connection at a time (for instance: a web server - see [this example](./sockets.html#server) on the Sockets page);
*   you might want to perform some background activity, without halting the main application while this is going on;
*   you might want your application to be notified when a certain time has elapsed;
*   you might want to keep the application running and active while waiting for some system resource to become available;
*   you might need to interface with some other system which requires multithreading (for example, "windows" under Windows which generally run in their own threads);
*   you might want to associate different contexts (e.g. different dynamic bindings) with different parts of the application;
*   you might even have the simple need to do two things at once.


<a name="emergency"></a>

## Emergency exits

It's fairly likely when you're learning to work with threads that - every now and again - one of them will run away from you and either fall motionless at your feet or make some CPU-intensive break for freedom. If you've managed to get your lisp tangled you may be short on options for halting a runaway thread, short of killing the lisp altogether.

Before you go any further, you might want to locate the "Process Browser". Aptly enough, you'll find it on the LispWorks podium under the icon of somebody running away. Open one **before** you get into trouble. To kill a process which won't respond to polite coercion, select it in the Process Browser and then click the "skull and crossbones" icon.

**Exercise:** Type `(mp:process-run-function "Foo" () (lambda () (loop)))` into a listener and use the Process Browser to kill the thread.


<a name="basics"></a>

## Basics

In order to run a function in a separate thread, you need to do two things.

1.  Make sure that multithreading is running. By default, multithreading is always running in the LispWorks for Windows environment so in this case there is nothing to do. See [initializing multithreading](#initializing) below to find out how to get threads running on other platforms.
2.  Now call your function, in its new thread. For example:

    ~~~lisp
    (defvar *foo* 0)
    (defun foo () (incf *foo*))
    (mp:process-run-function "Incrementing *foo*" nil 'foo)
    *foo*  => 1
    ~~~

In the above example, you created a new thread called `"Incrementing *foo*"`. The function `foo` was invoked (with no arguments) in that thread. When `foo` returned, the thread no longer had any work to do and so was terminated.

Note the following:

*   The first argument to `mp:process-run-function` is a string naming the thread. You don't have to make the names unique, but it's a smart move if you do in actual code, as it's a great way to tell your threads apart when you're debugging them. When playing around in the listener, you'll often find no harm in using an empty string here.
*   The second argument to `mp:process-run-function` is a list of _process initialization keywords_. You'll almost always leave this empty (so you needn't bother about any details).
*   The third argument is the function to invoke in the new thread. This can be any function designator, for example, an `fboundp` symbol or a `lambda` form.
*   Any remaining arguments to `mp:process-run-function` are passed to your function. So calls to `mp:process-run-function` look like calls to `funcall` with two additional arguments at the beginning.
*   A call to `mp:process-run-function` returns immediately (the return value is of type `mp:process`), while the new thread executes asynchronously.

Another simple example:

~~~lisp
CL-USER 15 > (mp:process-run-function "sleep in the background" nil 'sleep 10)
#<MP:PROCESS Name "sleep in the background" Priority 850000 State "Running">

CL-USER 16 > (mp:find-process-from-name "sleep in the background")
#<MP:PROCESS Name "sleep in the background" Priority 0 State "Sleeping on mailbox">

CL-USER 17 > (sleep 10)  ;; At this point the listener sleeps for ten seconds -
NIL                      ;; long enough for the new thread to run to completion.

CL-USER 18 > (mp:find-process-from-name "sleep in the background")
NIL                      ;; The sleeping thread has finished its job and terminated.

CL-USER 19 >
~~~

**Closures! Warning!** Note the difference between:

~~~lisp
(dotimes (i 20)
(mp:process-run-function "One closure" ()
(lambda () (print i #.*standard-output*))))
~~~

and

~~~lisp
(dotimes (i 20)
(mp:process-run-function "Twenty different bindings" ()
                         (lambda (j) (print j #.*standard-output*))
                         i))
~~~

In the first case, all twenty threads share the same closure variable `i`. The threads execute _asynchronously_ - in other words there's no way to tell exactly when or in what order they'll execute, or how they'll interleave with the thread which created them. In this case (try it!) you will observe that LispWorks initialises all 20 threads before any of them have a chance to start running. By far the easiest way of dealing with this is to ensure that variables which need to be private to each thread are bound on a per-thread basis.

**Exercise:** get two or three new threads running simultaneously, and convince yourself they're all there.


<a name="output"></a>

## Where's my output?

An obvious way to test whether threads are behaving as you imagine they ought, is to get them to print messages to the listener. For example, you might feel justified in trying something like:

~~~lisp
CL-USER 23 > (mp:process-run-function "test" () (lambda () (print 99)))
#<MP:PROCESS Name "test" Priority 850000 State "Running">

CL-USER 24 > ;; Where's my output?
~~~

Where indeed is your output? The answer is that your new thread has a different `*standard-output*` to the listener, and that's where your output has gone to. Here is how you might find out where precisely that is:

~~~lisp
CL-USER 25 > (mp:process-run-function
              "" ()
              (lambda ()
                (print *standard-output* #.*standard-output*)))
#<MP:PROCESS Name "" Priority 850000 State "Running">

#<Synonym stream to *TERMINAL-IO*>
CL-USER 26 >
~~~

**Exercise:** Open the console (i.e. `*terminal-io*`) by evaluating `(read-char *terminal-io*)`; type a `#\Newline` into the console to return from `read-char`. Now you can prove directly where a thread's output goes (by sending something there). **Warning!** Don't be tempted to close the console window, or you'll lose your lisp.


<a name="waiting"></a>

## Waiting

In all the above examples, a thread is created to run a simple function and then halt. In a typical application at least some of your threads will run an _event loop_ of some sort. An event loop is a function which repeatedly waits for an external event to occur. When an event is noticed, it is dispatched (maybe to another thread) for processing and the event loop cycles back to its waiting state. The "other thread" here might already exist (perhaps running an event loop of its own) or might be created specifically to perform this task (i.e. handle a single event) and then terminate.

It might be tempting to construct an event loop using `cl:sleep`. For example:

~~~lisp
(defun bogus-event-loop ()
(loop
   (sleep 1)                            ; THIS IS WRONG
(when (something-has-happened)
  (act-on-that-thing))))

(mp:process-run-function "Bogus event loop" ()
                         'bogus-event-loop)

~~~

This is a poor choice, because you're condemned to waiting for the `sleep` to return before you can perform the wake-up test. (Also, it's possible that your implementation cannot sleep one single thread without sleeping the whole lisp process. In such cases, any processing required before the predicate `something-has-happened` can return true will never happen.)

Consider instead:

~~~lisp
(defun improved-event-loop ()
  (loop
     (mp:process-wait "Waiting for something to happen"
                      'something-has-happened)
     (act-on-that-thing)))
~~~

The arguments to `mp:process-wait` are a string (which you should use for describing what this thread is waiting for), a function and optionally arguments to that function. The current thread will effectively sleep until the function returns true. The function can watch either internal state...

~~~lisp
(defun flush-entries-to-file (entries-symbol max-length file)
  (loop
     ;; Wait until we have enough entries to justify going to disk.
     (mp:process-wait (format nil "Waiting for ~a entr~:@p" max-length)
                      (lambda ()
                        (>= (length (symbol-value entries-symbol)) max-length)))
     ;; In this example, don't bother to spawn off a new thread to
     ;; perform the task.
     (let ((entries (shiftf (symbol-value entries-symbol) nil)))
       (with-open-file (ostream file
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
         (format ostream "~%Flushing entries:")
         (dolist (entry (reverse entries))
           (print entry ostream))))))

;; Test bed to drive the event loop
(defvar *test-entries* nil)
(defvar *test-file* "c:/temp/test-flush-entries.txt")
(defun test-flush-entries-to-file ()
  (let ((tester
         (mp:process-run-function "Test writing entries to file" ()
                                  'flush-entries-to-file
                                  '*test-entries*
                                  10
                                  *test-file*)))
    (dotimes (i 100)
      (push i *test-entries*)
      ;; Without the delay introduced by sleep, all 100 entries are
      ;; generated before the flusher has a chance to wake up.
      (sleep 0.1))
    (mp:process-kill tester)))
~~~

... or external state...

~~~lisp
(defun flush-entries-from-file (file reporting-stream)
  (loop
     ;; Wait for given file to exist on disk.
     (mp:process-wait (format nil "Waiting for ~a" file)
                      'probe-file file)
     ;; Empty the file to the reporting-stream, being careful to
     ;; allow more contents to accumulate while this is happening.
     (format reporting-stream "~&Reading ~a:~%" file)
     (let ((temp-file (format nil "~a.temp" file)))
       (rename-file file temp-file)
       (with-open-file (istream temp-file)
         (loop (let ((line (read-line istream nil)))
                 (unless line
                   (return))
                 (write-line line reporting-stream))))
       (delete-file temp-file))))

(defun test-flush-entries-from-file ()
  (delete-file *test-file*)
  (let ((tester
         (mp:process-run-function "Test reading entries from file" ()
                                  'flush-entries-from-file
                                  *test-file*
                                  *standard-output*)))
    ;; Use the previous example to create test data for this test.
    (test-flush-entries-to-log-file)
    (mp:process-kill tester)))
~~~

Although both of these examples were somewhat contrived, note their intentions to preserve their data from external modification once the waiting threads have woken up. In the first example, `shiftf` is used to atomically retrieve a symbol-value and reset it. In the above examples, this level of care didn't particularly matter, but in some applications it may be important to keep threads from trampling on each other's data.

Note also the use of `mp:process-kill` to terminate unwanted threads when each test is complete.

**Exercise:** Create and test a thread which will wait until the symbol `foo` is `boundp` and then announce the fact.


<a name="state"></a>

## Per-thread state

Every thread in your lisp system has its own execution stack and hence its own private state. The following aspects of state will therefore vary between different threads:

* bindings of dynamic variables;
* `catch` tags;
* condition handlers, `unwind-protect`, etc.;

On the other hand, the following are globally **set** rather than **bound** in a lisp system and so will not vary between threads:

* definitions of functions, methods, classes, conditions, packages;
* contents of any "compound" objects: cons cells, arrays, structure objects, CLOS objects, hash-tables;
* values of lexical variables in shared closures;
* state of any stream;
* symbol property-lists.

To see how per-thread variable bindings can lead you astray, consider the following examples:

~~~lisp
CL-USER 34 > (defvar *foo* nil)
*FOO*

CL-USER 35 > (mp:process-run-function "" () (lambda () (setf *foo* 1)))
#<MP:PROCESS Name "" Priority 850000 State "Running">

CL-USER 36 > *foo*
1

CL-USER 37 > (mp:process-run-function
              "Bind in new thread, can't see elsewhere." ()
              (lambda ()
                (let ((*foo* 2))
                  (sleep 5)
                  (setf *foo* 3)
                  (print 'done #.*standard-output*))))
#<MP:PROCESS Name "" Priority 850000 State "Running">

CL-USER 38 > *foo*
1

DONE
CL-USER 39 > (let ((*foo* 4))
               (mp:process-run-function
                "Bind in old thread, can't see elsewhere." ()
                (lambda () (print *foo* #.*standard-output*))))
#<MP:PROCESS Name "" Priority 850000 State "Running">

1
CL-USER 40 >
~~~

In both lines 34 and 35, `*foo*` is globally **set**. This means that every thread shares the symbol's value. In line 37, `*foo*` is **bound** only within the new thread; the original thread (i.e. the listener) does not see this binding or the result of a `setf` within the binding. Similarly, in line 39 the **binding** is present only in the original thread. To create a binding in a new thread, use `mp:*process-initial-bindings*`:

~~~lisp
CL-USER 40 > (let ((mp:*process-initial-bindings*
                    ;; Note the "dotted list" format - an unpleasant trap
                    ;; for the unwary. Note also that new value is pushed
                    ;; onto existing list so that system defaults aren't lost.
                    (cons '(*foo* . 5) mp:*process-initial-bindings*)))
               (mp:process-run-function
                "Bind around new thread." ()
                (lambda ()
                  (print *foo* #.*standard-output*))))
#<MP:PROCESS Name "Bind around new thread." Priority 850000 State "Running">

5
CL-USER 41 >
~~~

**Exercise:** Explain from examination of `mp:*process-initial-bindings*` why calling `in-package` in one listener does not affect the package in another.


<a name="mailbox"></a>

## Mailboxes

The mailbox is a structure designed to facilitate the transfer of data between threads. A number of operations are defined on mailboxes and are guaranteed to be _thread safe_ - that is, different threads can invoke any number of these operations "at the same time" without corrupting the mailbox structure.

The following example uses mailboxes to transfer "data" from ten "generating" threads to one "processing" thread. The function `mp:mailbox-send` takes a mailbox and an arbitrary lisp object as its arguments. Objects sent to the mailbox are queued in _FIFO_ (first in, first out) order, and are retrieved by calling `mp:mailbox-read`. Note that this this function will hang if the mailbox is empty when it is invoked; the wait reason and timeout are optional arguments. Note also how the timeout is used here to terminate the "processing" thread after the data have stopped arriving.

~~~lisp
(in-package "CL-USER")

;; process-data creates a thread which watches for data being
;; sent to its mailbox and then "processes" them.
(defun process-data (ostream)
  (let ((mailbox))
    (mp:process-run-function
     "Process data" ()
     (lambda ()
       ;; Create a mailbox
       (setf mailbox (mp:make-mailbox))
       (loop
          ;; Wait for someone to write to the mailbox
          (let ((datum (mp:mailbox-read mailbox
                                        "Waiting for data to process"
                                        5)))
            ;; "Process" the result
            (if datum
                (format ostream "~&Processing ~a.~%" datum)
                ;; Looks like everyone else went away. Terminate self.
                (return))))))
    ;; See the Slightly Tougher Exercise below...
    (mp:process-wait "Waiting for mailbox to exist."
                     (lambda () mailbox))
    ;; Return mailbox so that others can share it.
    mailbox))

;; generate-data is called by each "generator" thread, to send
;; 100 data to the mailbox. Each thread will die when its call to this
;; function returns.
(defun generate-data (id mailbox)
  (loop for count to 100 do
       (let ((datum (cons id count)))
         (sleep (random 1.0))
         (mp:mailbox-send mailbox datum))))

;; Pass mailbox from "processor" to various "generators".
(defun mailbox-demo ()
  (let ((mailbox (process-data *standard-output*)))
    (loop for id to 10 do
         (mp:process-run-function
          (format nil "Generator ~d." id) ()
          'generate-data
          id
          mailbox))))
~~~

**Slightly Tougher Exercise:** Consider the call to `mp:process-wait` in the example above. Explain what will happen -- and why -- if (a) the call is removed and (b) the call is replaced by:

~~~lisp
(mp:process-wait "Waiting for mailbox to exist."
                 'identity
                 mailbox)
~~~


<a name="interrupts"></a>

## Interrupts

The above sections have dealt with ways of using **data** to communicate between threads. Another important mode of communication is the _interrupt_: a message sent by one thread to another, to tell that thread to stop whatever it was doing and get on with something else instead. This might come in handy if:

* you need certain actions to be performed synchronously - for example you need your web server to flush some cache before it accepts any further connections; or
* you need actions to be performed in the dynamic context of another thread.

To interrupt a thread, use the function `mp:process-interrupt`. This takes a thread as its first argument, followed by a function and optionally arguments to that function. As elsewhere, you can use `mp:find-process-from-name` to locate the thread object corresponding to a given name. For example:

~~~lisp
(defun flush-cache (cache)
  (let ((tcp-server (mp:find-process-from-name "Port 80 server")))
    (mp:process-interrupt tcp-server
                          'flush-data-cache
                          cache)))
~~~

A related utility is `mp:process-interrupt-list`, which takes just three arguments: a thread, a function, a list of arguments to that function.

**Exercise:** Use `mp:*process-initial-bindings*` to create a thread whose `*standard-output*` is the listener you're working in. Set this thread to sleeping for a long time (1000 seconds will do). Now create a second thread without the redirected `*standard-output*`, and have it ask the sleeper to print something for it.


<a name="capi"></a>

## Threads and the CAPI windowing system

By default, every CAPI interface runs in its own thread. This thread will be used by the lisp system for actions such as redisplay and invocation of callbacks. If you need to act programmatically on a CAPI interface, you are **strongly recommended** to work in the appropriate thread.

The utility `capi:execute-with-interface` will help you out here. This takes as its first argument a _CAPI interface_; subsequent arguments are a function followed by optional arguments to that function. The function will be invoked in the thread belonging to that interface. To get from any CAPI element to its interface, use the reader `capi:element-interface`, as in the following example.

In this example, the `only` way to get `*switchable*` to switch is to execute the request in `*switchable*`'s thread.

~~~lisp
;; Create and display a window which can switch between
;; two children. These have different coloured backgrounds.
;; Red was listed first and so by default is visible initially.
(defvar *switchable*
  (let ((red-pane (make-instance 'capi:output-pane
                                 :name 'red
                                 :background :red))
        (green-pane (make-instance 'capi:output-pane
                                   :name 'green
                                   :background :green)))
    (capi:contain
     (make-instance 'capi:switchable-layout
                    :description (list red-pane green-pane)))))

;; Utility to return the green child.
(defun green-pane (switchable)
  (find 'green (capi:switchable-layout-switchable-children
                switchable)
        :key 'capi:capi-object-name))

;; If you try this then (a) you'll get an error and (b) calling
;; (right *switchable*) won't help - that window's state is broken
;; and you'll have to create a new one.
(defun wrong (switchable)
  (setf (capi:switchable-layout-visible-child switchable)
        (green-pane switchable)))

(defun right (switchable)
  (capi:execute-with-interface
   (capi:element-interface switchable)
   (lambda (switchable)
     (setf (capi:switchable-layout-visible-child switchable)
           (green-pane switchable)))
   switchable))
~~~


<a name="locks"></a>

## Locks

Sometimes it's important to control access to some resource, so that only one thread is operating on it at a time. One method of effecting this is to restrict access from your code to that resource to one (or more) specialised threads, and to have other threads write to a mailbox when they want the specialised threads to access the resource on their behalf. However there are two potential drawbacks to this approach.

* Frequently you'll need to wait in the invoking thread until the operation on the resource is complete.
* This is becoming a somewhat heavyweight mechanism for handling what should be a fairly simple operation.

A _lock_ is a simple lisp object, which can be _held_ by no more than one thread at a time. A thread attempting to hold a lock which is already in use will hang (i.e. be forced to wait) until the lock is freed. In the following example, the locking mechanism is reduced to its most minimal form.

~~~lisp
;; Create a lock and remember it.
(defvar *lock* (mp:make-lock))

(defun use-resource-when-free (id stream)
  ;; Callers must wait at this point for the lock to
  ;; be freed, before they can proceed with the body of
  ;; this form.
  (mp:with-lock (*lock*)
    (use-resource-anyway id stream)
    ;; When we exit the form, the lock is freed automatically
    ;; and some other thread will be allowed to claim it.
    ))

(defun use-resource-anyway (id stream)
  (format stream "~&Starting ~a." id)
  (sleep 1)
  (format stream "~&Ending ~a." id))

(defun test (lock-p)
  (let ((run-function (if lock-p
                          'use-resource-when-free
                          'use-resource-anyway)))
    (dotimes (id 3)
      (mp:process-run-function
       (format nil "Competing thread ~a" id) nil
       run-function id *standard-output*))))
~~~


<a name="timers"></a>

## A brief warning about timers

Timers are a handy way of notifying your application that some period of time has elapsed. You should however be aware of the following vital piece of information: **TIMERS RUN IN WHATEVER PROCESS IS RUNNING WHEN THE TIME IS REACHED, WHICH IS LIKELY TO BE THE IDLE PROCESS IN CASES WHERE LISPWORKS IS SLEEPING**...

~~~lisp
CL-USER 62 > (defun foo () (print mp:*current-process*
                                  #.*standard-output*))
FOO

CL-USER 63 > (setf *timer* (mp:make-timer 'foo))
#<Time Event : FOO>

CL-USER 64 > (mp:schedule-timer-relative *timer* 1 1)
#<Time Event : FOO>

#<MP:PROCESS Name "The idle process" Priority -8388608 State "Running">
#<MP:PROCESS Name "The idle process" Priority -8388608 State "Running">
#<MP:PROCESS Name "The idle process" Priority -8388608 State "Running">
#<MP:PROCESS Name "The idle process" Priority -8388608 State "Running">
CL-USER 65 > (mp:unschedule-timer *timer*)
#<Time Event : FOO>

CL-USER 66 >
~~~

A consequence of this is that any errors you get in a timer are errors in the idle process, which is the manager for multiprocessing and generally a Bad Place to Get Errors. This was potentially a more serious problem in LispWorks 4.1; it looks safer (i.e. non fatal) in more recent editions of LispWorks. In any case, you are advised always to switch to another process to run the contents of your timer.

~~~lisp
(defvar *timer*)
(defparameter *housekeeping-interval* 3600)      ; once per hour

;; Create a timer, set it to "expire" in one hour, and subsequently at
;; hourly intervals.
(defun start-timer ()
  (setf *timer* (mp:make-timer 'safe-housekeeping))
  (mp:schedule-timer-relative *timer*
                              *housekeeping-interval*
                              *housekeeping-interval*))

; Switch thread, so we can't possibly get errors in the idle process ;-(
(defun safe-housekeeping ()
  (mp:process-run-function "Housekeeping" nil
                           'housekeeping))
~~~


<a name="initializing"></a>

## Initializing multithreading

As noted previously, multithreading is "always" running in the LispWorks for Windows environment. However, while the image is initializing itself (loading your ".lispworks" file, for example, or running a `:restart-function` supplied to `save-image`), multithreading has not yet started and so "inter-thread" functions such as `mp:process-run-function` and `mp:process-wait` cannot be called.

There are circumstances in which you will want to start multithreading yourself:

* on platforms where you must explicitly call `(env:start-environment)` to launch the CAPI development environment - this function will start multithreading if that hasn't already happened;
* you might want access to multithreading functionality from the non-windowing listener;
* you might want a resaved image to execute code which requires multithreading.

If in doubt, you can tell whether multithreading has started yet by examining either `mp:*current-process*` or `(mp:list-all-processes)`. If either of these is null, multithreading hasn't started.

To start multithreading, call `(mp:initialize-multiprocessing)`. Specify the threads that should run on startup of multithreading via the variable `mp:*initial-processes*`. This is a list whose members are themselves lists to which `mp:process-run-function` can be applied. **Warning:** do not remove any entries which the implementation has placed on `mp:*initial-processes*`.

Note that `(mp:initialize-multiprocessing)` does not return until all threads have run to completion, at which point multithreading itself halts. If you call it from the non-windowing listener without specifying any threads to run, `(mp:initialize-multiprocessing)` will appear to return no values immediately, but appearances can be deceptive - what actually happens is that LispWorks is designed to spot that multithreading has been started without any threads to run, and so to give you a "default listener" (in its own thread) rather than leaving you with a hung lisp.

The above ideas are illustrated by the following example. To run this on Windows or Linux, you will need to resave the image to run in the TTY (without multithreading on startup), by passing the keyword argument `:environment nil` to `save-image`.

~~~lisp
/cygdrive/c/Program Files/Xanalys/LispWorks $ ./console-test.exe
LispWorks(R) (for the Windows(R) operating system)
Copyright (C) 1987-2002 Xanalys Inc.  All rights reserved.
Version 4.2.6
Saved by nick as console-test, at 28 Jun 2002 15:25
User nick on GANNET
; Loading text file c:\Program Files\Xanalys\LispWorks\lib\4-2-0-0\config\siteinit.lisp
;  Loading text file c:\Program Files\Xanalys\LispWorks\lib\4-2-0-0\private-patches\load.lisp
; Loading text file e:\.lispworks

CL-USER 1 > mp:*current-process*

NIL

CL-USER 2 > (push (list "My only thread" ()
                        (lambda ()
                          (print (mp:list-all-processes) *terminal-io*)
                          (sleep 5)))
                  mp:*initial-processes*)

(("My only thread" NIL #'(LAMBDA NIL (PRINT (MP:LIST-ALL-PROCESSES) *TERMINAL-IO*) (SLEEP 5)))
 ("The idle process" (:PRIORITY -8388608 :RESTART-ACTION :CONTINUE) MP::PROCESS-IDLE-FUNCTION))

CL-USER 3 > (mp:initialize-multiprocessing)

(#<MP:PROCESS Name "My only thread" Priority 0 State "Running">
              #<MP:PROCESS Name "The idle process" Priority -8388608 State "Running">)
NIL

CL-USER 4 > (mp:list-all-processes)

NIL

CL-USER 5 > (pop mp:*initial-processes*)

("My only thread" NIL #'(LAMBDA NIL (PRINT (MP:LIST-ALL-PROCESSES) *TERMINAL-IO*) (SLEEP 5)))

CL-USER 6 > (mp:initialize-multiprocessing)

CL-USER 7 > (mp:list-all-processes)

(#<MP:PROCESS Name "default listener process" Priority 600000 State "Running">
              #<MP:PROCESS Name "The idle process" Priority -8388608 State "Running">)

CL-USER 8 > (quit)
/cygdrive/c/Program Files/Xanalys/LispWorks $
~~~

**Exercise:** Resave a lisp image which will restart with a "TTY" listener running in multithread mode.
