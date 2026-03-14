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
- **Character streams** carry characters (the default).
- **Binary streams** carry unsigned bytes, specified by
  an element type like `(unsigned-byte 8)`.

You can test what a stream supports:

~~~lisp
(input-stream-p *standard-input*)   ;; => T
(output-stream-p *standard-output*) ;; => T
(stream-element-type *standard-input*)
;; => CHARACTER
~~~

## Standard streams

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
`format`, `write-string`, or other stream operations.

### Reading from a string: `with-input-from-string`

~~~lisp
(with-input-from-string (s "123 456")
  (list (read s) (read s)))
;; => (123 456)
~~~

### `make-string-input-stream` and `make-string-output-stream`

For cases where the macro forms are inconvenient, you
can create string streams directly:

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
stream is exhausted, reading continues from the next:

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
Common Lisp runtime. If you need custom stream behavior
(for example, a stream that compresses data, counts
bytes, or transforms characters), you can use
**Gray streams**.

Gray streams are a de facto standard (supported by SBCL,
CCL, ECL, ABCL, LispWorks, Allegro, and others) that
lets you define stream classes with CLOS methods.

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
- `stream-read-line` (optional, for performance)

**Character output streams:**

- `stream-write-char` — write one character
- `stream-line-column` — current column (or `nil`)
- `stream-write-string` (optional, for performance)

**Binary streams:**

- `stream-read-byte`
- `stream-write-byte`

## Further reading

- [CLHS: Streams](http://www.lispworks.com/documentation/HyperSpec/Body/21_.htm)
- [CLtL2: Streams](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node329.html)
- [trivial-gray-streams](https://github.com/trivial-gray-streams/trivial-gray-streams)
