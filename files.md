---
title: Files and Directories
---

Note: In this chapter, we use mainly
[namestrings](http://www.lispworks.com/documentation/HyperSpec/Body/19_aa.htm)
to
[specify filenames](http://www.lispworks.com/documentation/HyperSpec/Body/19_.htm). The
issue of
[pathnames](http://www.lispworks.com/documentation/HyperSpec/Body/19_ab.htm)
will be the topic of separate chapter
[REAL SOON NOW](http://www.tuxedo.org/~esr/jargon/html/entry/Real-Soon-Now.html).

### Testing whether a File Exists

Use the function
[`probe-file`](http://www.lispworks.com/documentation/HyperSpec/Body/f_probe_.htm)
which will return a
[generalized boolean](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#generalized_boolean) -
either `nil` if the file doesn't exists, or its
[truename](http://www.lispworks.com/documentation/HyperSpec/Body/20_ac.htm)
(which might be different from the argument you supplied).

~~~lisp
edi@bird:/tmp> ln -s /etc/passwd foo
edi@bird:/tmp> cmucl
; Loading #p"/home/edi/.cmucl-init".
CMU Common Lisp 18d-pre, level-1 built 2002-01-15 on maftia1, running on bird
Send questions to cmucl-help@cons.org. and bug reports to cmucl-imp@cons.org.
Loaded subsystems:
Python native code compiler, target Intel x86
CLOS based on PCL version:  September 16 92 PCL (f)
Gray Streams Protocol Support
CLX X Library MIT R5.02
* (probe-file "/etc/passwd")

#p"/etc/passwd"
* (probe-file "foo")

#p"/etc/passwd"
* (probe-file "bar")

NIL
~~~

### Creating directories

The function
[ensure-directories-exist](http://www.lispworks.com/documentation/HyperSpec/Body/f_ensu_1.htm)
creates the directories if they do not exist:

~~~lisp
(ensure-directories-exist "foo/bar/baz/")
~~~

This may create `foo`, `bar` and `baz`. Don't forget the trailing slash.


### Opening a File

Common Lisp has
[`open`](http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm) and
[`close`](http://www.lispworks.com/documentation/HyperSpec/Body/f_close.htm)
functions which resemble the functions of the same denominator from other
programming languages you're probably familiar with. However, it is almost
always recommendable to use the macro
[`with-open-file`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_open.htm)
instead. Not only will this macro open the file for you and close it when you're
done, it'll also take care of it if your code leaves the body abnormally (such
as by a use of
[throw](http://www.lispworks.com/documentation/HyperSpec/Body/s_throw.htm)). A
typical use of `with-open-file` looks like this:

~~~lisp
(with-open-file (str <_file-spec_>
:direction <_direction_>
:if-exists <_if-exists_>
:if-does-not-exist <_if-does-not-exist_>)
<_your code here_>)
~~~

*   `str` is a variable which'll be bound to the stream which is created by
    opening the file.
*   `<_file-spec_>` will be a truename or a pathname.
*   `<_direction_>` is usually `:input` (meaning you want to read from the file),
    `:output` (meaning you want to write to the file) or `:io` (which is for
    reading _and_ writing at the same time) - the default is `:input`.
*   `<_if-exists_>` specifies what to do if you want to open a file for writing
    and a file with that name already exists - this option is ignored if you
    just want to read from the file. The default is `:error` which means that an
    error is signalled. Other useful options are `:supersede` (meaning that the
    new file will replace the old one), `nil` (the stream variable will be bound
    to `nil`), and `:rename` (i.e. the old file is renamed).
*   `<_if-does-not-exist_>` specifies what to do if the file you want to open does
    not exist. It is one of `:error` for signalling an error, `:create` for
    creating an empty file, or `nil` for binding the stream variable to
    `nil`. The default is, to be brief, to do the right thing depending on the
    other options you provided. See the CLHS for details.

Note that there are a lot more options to `with-open-file`. See
[the CLHS entry for `open`](http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm)
for all the details. You'll find some examples on how to use `with-open-file`
below. Also note that you usually don't need to provide any keyword arguments if
you just want to open an existing file for reading.<a name="strings">

### Using Strings instead of Files

### Reading a File one Line at a Time

[`read-line`](http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_lin.htm)
will read one line from a stream (which defaults to
[_standard input_](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#standard_input))
the end of which is determined by either a newline character or the end of the
file. It will return this line as a string _without_ the trailing newline
character. (Note that `read-line` has a second return value which is true if there
was no trailing newline, i.e. if the line was terminated by the end of the
file.) `read-line` will by default signal an error if the end of the file is
reached. You can inhibit this by supplying NIL as the second argument. If you do
this, `read-line` will return `nil` if it reaches the end of the file.

~~~lisp
(with-open-file (stream "/etc/passwd")
  (do ((line (read-line stream nil)
       (read-line stream nil)))
       ((null line))
       (print line)))
~~~

You can also supply a third argument which will be used instead of `nil` to signal
the end of the file:

~~~lisp
(with-open-file (stream "/etc/passwd")
  (loop for line = (read-line stream nil 'foo)
   until (eq line 'foo)
   do (print line)))
~~~

### Reading a File one Character at a Time

[`read-char`](http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_cha.htm)
is similar to `read-line`, but it only reads one character as opposed to one
line. Of course, newline characters aren't treated differently from other
characters by this function.

~~~lisp
(with-open-file (stream "/etc/passwd")
  (do ((char (read-char stream nil)
       (read-char stream nil)))
       ((null char))
       (print char)))
~~~

### Reading a File into a String

It's quite common to need to access the contents of a file in string
form.

Alexandria offers this function: [read-file-into-string](https://common-lisp.net/project/alexandria/draft/alexandria.html#IO). It accepts an `:external-format` argument (that can be bound to `:utf-8`). It is included in [cl21](cl21.html).


Without Alexandria, this can be achieved by using `read-line` or `read-char` functions,
that probably won't be the best solution. File might not be divided into
multiple lines or reading one character at a time might bring significant
performance problems. To solve this problems, you can read files using buckets
of specific sizes.

~~~lisp
(with-output-to-string (out)
  (with-open-file (in "/path/to/big/file")
    (loop with buffer = (make-array 8192 :element-type 'character)
          for n-characters = (read-sequence buffer in)
          while (< 0 n-characters)
          do (write-sequence buffer out :start 0 :end n-characters)))))
~~~

Furthermore, you're free to change the format of the read/written data, instead
of using elements of type character everytime. For instance, you can set
`:element-type` type argument of `with-output-to-string`, `with-open-file` and
`make-array` functions to `'(unsigned-byte 8)` to read data in octets.

### Reading with an UTF-8 encoding

To avoid an `ASCII stream decoding error` you might want to specify an UTF-8 encoding:

~~~lisp
(with-open-file (in "/path/to/big/file"
                     :external-format :utf-8)
                 ...
~~~

### Looking one Character ahead

You can 'look at' the next character of a stream without actually removing it
from there - this is what the function
[`peek-char`](http://www.lispworks.com/documentation/HyperSpec/Body/f_peek_c.htm)
is for. It can be used for three different purposes depending on its first
(optional) argument (the second one being the stream it reads from): If the
first argument is `nil`, `peek-char` will just return the next character that's
waiting on the stream:

~~~lisp
CL-USER> (with-input-from-string (stream "I'm not amused")
           (print (read-char stream))
           (print (peek-char nil stream))
           (print (read-char stream))
           (values))

#\I
#\'
#\'
~~~

If the first argument is `T`, `peek-char` will skip
[whitespace](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_w.htm#whitespace)
characters, i.e. it will return the next non-whitespace character that's waiting
on the stream. The whitespace characters will vanish from the stream as if they
had been read by `read-char`:

~~~lisp
CL-USER> (with-input-from-string (stream "I'm not amused")
           (print (read-char stream))
           (print (read-char stream))
           (print (read-char stream))
           (print (peek-char t stream))
           (print (read-char stream))
           (print (read-char stream))
           (values))

#\I
#\'
#\m
#\n
#\n
#\o
~~~

If the first argument to `peek-char` is a character, the function will skip all
characters until that particular character is found:

~~~lisp
CL-USER> (with-input-from-string (stream "I'm not amused")
           (print (read-char stream))
           (print (peek-char #\a stream))
           (print (read-char stream))
           (print (read-char stream))
           (values))

#\I
#\a
#\a
#\m
~~~

Note that `peek-char` has further optional arguments to control its behaviour on
end-of-file similar to those for `read-line` and `read-char` (and it will signal an
error by default):

~~~lisp
CL-USER> (with-input-from-string (stream "I'm not amused")
           (print (read-char stream))
           (print (peek-char #\d stream))
           (print (read-char stream))
           (print (peek-char nil stream nil 'the-end))
           (values))

#\I
#\d
#\d
THE-END
~~~

You can also put one character back onto the stream with the function
[`unread-char`](http://www.lispworks.com/documentation/HyperSpec/Body/f_unrd_c.htm). You
can use it as if, _after_ you have read a character, you decide that you'd
better used `peek-char` instead of `read-char`:

~~~lisp
CL-USER> (with-input-from-string (stream "I'm not amused")
           (let ((c (read-char stream)))
             (print c)
             (unread-char c stream)
             (print (read-char stream))
             (values)))

#\I
#\I
~~~

Note that the front of a stream doesn't behave like a stack: You can only put
back exactly _one_ character onto the stream. Also, you _must_ put back the same
character that has been read previously, and you can't unread a character if
none has been read before.

### Random Access to a File

Use the function
[`file-position`](http://www.lispworks.com/documentation/HyperSpec/Body/f_file_p.htm)
for random access to a file. If this function is used with one argument (a
stream), it will return the current position within the stream. If it's used
with two arguments (see below), it will actually change the
[file position](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#file_position)
in the stream.

~~~lisp
CL-USER> (with-input-from-string (stream "I'm not amused")
           (print (file-position stream))
           (print (read-char stream))
           (print (file-position stream))
           (file-position stream 4)
           (print (file-position stream))
           (print (read-char stream))
           (print (file-position stream))
           (values))

0
#\I
1
4
#\n
5
~~~

### Getting file attributes (size, access time,...), with the Osicat library

[Osicat](https://www.common-lisp.net/project/osicat/) (in Quicklisp)
is a lightweight operating system interface for Common Lisp on
POSIX-like systems, including Windows. With Osicat we can get and set
**environment variables**, manipulate **files and directories**,
**pathnames** and a bit more.

Once it is installed, Osicat also defines the `osicat-posix` system,
which permits us to get file attributes.

~~~lisp
(ql:quickload :osicat)

(let ((stat (osicat-posix:stat #P"./files.md")))
  (osicat-posix:stat-size stat))  ;; => 10629
~~~

We can get the other attributes with the following methods:

~~~
osicat-posix:stat-dev
osicat-posix:stat-gid
osicat-posix:stat-ino
osicat-posix:stat-uid
osicat-posix:stat-mode
osicat-posix:stat-rdev
osicat-posix:stat-size
osicat-posix:stat-atime
osicat-posix:stat-ctime
osicat-posix:stat-mtime
osicat-posix:stat-nlink
osicat-posix:stat-blocks
osicat-posix:stat-blksize
~~~
