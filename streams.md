---
title: Streams
---

Streams are the standard abstraction for input and output in
Common Lisp. Every time you read from a file, write to the
terminal, or communicate over a network socket, you are using
a stream. This chapter covers the stream types, how to create
and use them, and how to extend the stream protocol.

For basic read/write recipes, see also
[Input/Output](io.html).

## Stream basics

A stream is an object that represents a source or sink of
characters or bytes. The standard defines several stream
types:

- **Input streams** support reading (`read-char`,
  `read-byte`, `read-line`, `read`).
- **Output streams** support writing (`write-char`,
  `write-byte`, `write-string`, `format`).
- **Bidirectional streams** support both.

Separately, streams have an element type:

- **Character streams** carry characters, which is what
  `read-char`, `read-line`, `format`, and most examples in
  this chapter use by default.
- **Binary streams** carry bytes, usually declared with an
  element type like `(unsigned-byte 8)`.

You can test what a stream supports:

~~~lisp
(input-stream-p *standard-input*)   ;; => T
(output-stream-p *standard-output*) ;; => T
(stream-element-type *standard-input*)
;; => CHARACTER
~~~

## Standard stream variables

Common Lisp provides several global stream variables that
are bound by default:

| Variable | Purpose |
|---|---|
| `*standard-input*` | Default input (your terminal) |
| `*standard-output*` | Default output (your terminal) |
| `*error-output*` | Error/warning messages |
| `*trace-output*` | Output from `trace` |
| `*debug-io*` | Interactive debugging I/O |
| `*query-io*` | User yes/no questions |
| `*terminal-io*` | The actual terminal stream |

Functions like `read`, `print`, and `format` use these by
default when you don't specify a stream:

~~~lisp
;; these are equivalent:
(print "hello")
(print "hello" *standard-output*)
~~~

You can rebind them with `let` to redirect output:

~~~lisp
(let ((*standard-output* some-other-stream))
  (print "hello"))
  ;; prints to some-other-stream
~~~

## File streams

Use `open` to create a file stream, or the
`with-open-file` macro which ensures the stream is
properly closed:

~~~lisp
;; reading a file:
(with-open-file (stream "/tmp/test.txt")
  (loop for line = (read-line stream nil)
        while line
        do (print line)))
~~~

~~~lisp
;; writing to a file:
(with-open-file (stream "/tmp/out.txt"
                 :direction :output
                 :if-exists :supersede)
  (format stream "Hello, streams!~%"))
~~~

The `:direction` keyword controls the stream type:

- `:input` (default) — read only
- `:output` — write only
- `:io` — read and write
- `:probe` — just check if the file exists, then close

For binary files, specify `:element-type`:

~~~lisp
(with-open-file (stream "/tmp/data.bin"
                 :direction :output
                 :if-exists :supersede
                 :element-type '(unsigned-byte 8))
  (write-byte 72 stream)
  (write-byte 101 stream))
~~~

## String streams

String streams let you treat strings as streams, which is
useful for building output or parsing input without files.

### Writing to a string: `with-output-to-string`

~~~lisp
(with-output-to-string (s)
  (format s "Hello, ~a!" "world"))
;; => "Hello, world!"
~~~

This is the idiomatic way to build strings with
`format`, `write-string`, or other stream operations. It is
often the stream-oriented equivalent of using `format` with a
destination of `nil`:

~~~lisp
(format nil "Hello, ~a!" "world")
;; => "Hello, world!"
~~~

It is especially handy when you already have functions that
write to a stream, such as `print-object` methods:

~~~lisp
(defclass person ()
  ((name :initarg :name :reader person-name)))

(defmethod print-object ((obj person) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (person-name obj))))
~~~

### Reading from a string: `with-input-from-string`

`read` reads the next Lisp object from the stream, so it
parses tokens using the Lisp reader. `read-char` reads a
single character instead. Reading from a string is useful for
small parsers, REPL helpers, or tests where you want input
without touching the filesystem.

~~~lisp
(with-input-from-string (s "123 456")
  (list (read s) (read s)))
;; => (123 456)
~~~

### `make-string-input-stream` and `make-string-output-stream`

For cases where the macro forms are inconvenient, you
can create string streams directly. This is common when you
need to create the stream in one place and consume it later.

~~~lisp
(let ((s (make-string-output-stream)))
  (format s "one ")
  (format s "two ")
  (format s "three")
  (get-output-stream-string s))
;; => "one two three"
~~~

~~~lisp
(let ((s (make-string-input-stream "hello")))
  (read-char s))
;; => #\h
~~~

## Concatenated streams

`make-concatenated-stream` creates a stream that reads
from multiple input streams in sequence. When the first
stream is exhausted, reading continues from the next. This is
useful when several inputs should look like one continuous
source to existing stream-consuming code:

~~~lisp
(let* ((s1 (make-string-input-stream "Hello, "))
       (s2 (make-string-input-stream "world!"))
       (combined (make-concatenated-stream s1 s2)))
  (read-line combined))
;; => "Hello, world!"
~~~

## Broadcast streams

`make-broadcast-stream` creates a stream that sends
output to multiple streams simultaneously:

~~~lisp
(let* ((str (make-string-output-stream))
       (broadcast (make-broadcast-stream
                    *standard-output* str)))
  (format broadcast "to both~%")
  (get-output-stream-string str))
;; prints "to both" to the terminal
;; => "to both
;; "
~~~

This is useful for logging to both the console and a
file at the same time.

Calling `make-broadcast-stream` with no arguments is also the
portable equivalent of writing to `/dev/null`: output sent to
that stream is discarded.

~~~lisp
(let ((sink (make-broadcast-stream)))
  (format sink "this goes nowhere"))
~~~

## Real-world example: one report, many destinations

A common pattern in real programs is to write functions that
accept a stream instead of deciding for themselves whether
output should go to the terminal, a file, or an in-memory
string. That keeps the formatting code in one place and makes
it easy to reuse.

~~~lisp
(defun write-expense-report (expenses stream)
  (format stream "Expense report~%")
  (format stream "==============~%")
  (dolist (entry expenses)
    (destructuring-bind (label amount) entry
      (format stream "~a: ~,2f EUR~%" label amount)))
  (format stream "--------------~%")
  (format stream "Total: ~,2f EUR~%"
          (loop for (_ amount) in expenses
                sum amount)))
~~~

The same function can now target different destinations:

~~~lisp
(let ((expenses '(("Books" 12.50)
                  ("Train" 24.10)
                  ("Lunch" 18.00))))
  ;; 1) print to the REPL / terminal
  (write-expense-report expenses *standard-output*)

  ;; 2) save to a file
  (with-open-file (out "/tmp/expenses.txt"
                       :direction :output
                       :if-exists :supersede)
    (write-expense-report expenses out))

  ;; 3) capture as a string, for a test or an email body
  (with-output-to-string (out)
    (write-expense-report expenses out)))
;; => "Expense report
;; => ==============
;; => Books: 12.50 EUR
;; => Train: 24.10 EUR
;; => Lunch: 18.00 EUR
;; => --------------
;; => Total: 54.60 EUR
;; => "
~~~

If you want tee-style output, you can also combine destinations
with a broadcast stream:

~~~lisp
(let* ((expenses '(("Books" 12.50)
                   ("Train" 24.10)))
       (copy (make-string-output-stream))
       (tee (make-broadcast-stream *standard-output* copy)))
  (write-expense-report expenses tee)
  (get-output-stream-string copy))
~~~

## Two-way and echo streams

A **two-way stream** bundles an input and output stream
into a single bidirectional stream:

~~~lisp
(let* ((in (make-string-input-stream "42"))
       (out (make-string-output-stream))
       (two-way (make-two-way-stream in out)))
  (format two-way "answer: ~a~%"
          (read two-way))
  (get-output-stream-string out))
;; => "answer: 42
;; "
~~~

An **echo stream** is a two-way stream that also echoes
everything read from the input stream onto the output
stream. This is useful for logging or recording
interactive sessions:

~~~lisp
(let* ((in (make-string-input-stream "hello"))
       (out (make-string-output-stream))
       (echo (make-echo-stream in out)))
  (read-char echo)  ;; reads #\h, also writes to out
  (read-char echo)  ;; reads #\e, also writes to out
  (get-output-stream-string out))
;; => "he"
~~~

## Synonym streams

A synonym stream is an indirection — it forwards all
operations to the stream that is the current value of a
symbol. `*terminal-io*` is typically a synonym stream.

~~~lisp
(let ((s (make-synonym-stream '*my-output*)))
  (let ((*my-output* *standard-output*))
    (format s "hi~%")))
;; prints "hi" to standard output
~~~

This lets you redirect where a stream goes by rebinding
the symbol, without changing the stream object itself.

## Gray streams: extending the protocol

The standard stream types are implemented by the
Common Lisp runtime. They let you *use* file, string, socket,
and terminal streams, but they do not standardize how you
define new stream classes that participate in ordinary Common
Lisp I/O operations. If you need custom stream behavior
(for example, a stream that compresses data, counts
bytes, transforms characters, or reads from an application
object instead of a file descriptor), you can use
**Gray streams**.

Gray streams are a de facto standard, proposed before ANSI
Common Lisp was finalized and based on the stream chapter from
CLtL. They did not make it into the ANSI standard, but most
popular implementations support this protocol anyway. In
practice, Gray streams are the usual way to define custom
streams that work with standard functions like `read-char`,
`write-char`, `read-sequence`, or `write-sequence`.

The [`trivial-gray-streams`](https://github.com/trivial-gray-streams/trivial-gray-streams)
library provides a portable interface:

~~~lisp
;; in your .asd:
;; :depends-on ("trivial-gray-streams")

(defclass counting-stream
    (trivial-gray-streams:fundamental-character-output-stream)
  ((inner :initarg :inner :reader inner-stream)
   (count :initform 0 :accessor char-count)))

(defmethod trivial-gray-streams:stream-write-char
    ((stream counting-stream) char)
  (incf (char-count stream))
  (write-char char (inner-stream stream)))

(defmethod trivial-gray-streams:stream-line-column
    ((stream counting-stream))
  nil)
~~~

Using it:

~~~lisp
(let* ((out (make-string-output-stream))
       (counting (make-instance 'counting-stream
                                :inner out)))
  (write-string "hello" counting)
  (values (get-output-stream-string out)
          (char-count counting)))
;; => "hello"
;; => 5
~~~

The key methods to implement depend on the stream type:

**Character input streams:**

- `stream-read-char` — read one character
- `stream-unread-char` — push a character back
- `stream-read-char-no-hang` (optional) — non-blocking character read
- `stream-read-line` (optional, for performance)
- `stream-read-sequence` (optional, for performance)

**Character output streams:**

- `stream-write-char` — write one character
- `stream-line-column` — current column (or `nil`)
- `stream-write-string` (optional, for performance)
- `stream-write-sequence` (optional, for performance)

**Binary streams:**

- `stream-read-byte`
- `stream-write-byte`
- `stream-read-sequence` / `stream-write-sequence`

The sequence methods let your stream move whole slices of data
at once, which is often much faster than reading or writing
one character or byte at a time.

## Further reading

- [CLHS: Streams](http://www.lispworks.com/documentation/HyperSpec/Body/21_.htm)
- [CLtL2: Streams](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node329.html)
- [trivial-gray-streams](https://github.com/trivial-gray-streams/trivial-gray-streams)
- [flexi-streams](https://edicl.github.io/flexi-streams/)
- [nontrivial-gray-streams](https://github.com/yitzchak/nontrivial-gray-streams)
- [Allegro CL simple-streams](https://franz.com/support/documentation/10.1/doc/streams.htm)
