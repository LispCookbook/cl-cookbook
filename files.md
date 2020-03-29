---
title: Files and Directories
---

Note: In this chapter, we use mainly
[namestrings](http://www.lispworks.com/documentation/HyperSpec/Body/19_aa.htm)
to
[specify filenames](http://www.lispworks.com/documentation/HyperSpec/Body/19_.htm). The
issue of
[pathnames](http://www.lispworks.com/documentation/HyperSpec/Body/19_ab.htm)
needs to be covered separately.

Many functions will come from UIOP, so we suggest you have a look directly at it:

* [UIOP/filesystem](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fFILESYSTEM)
* [UIOP/pathname](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fPATHNAME)

Of course, do not miss:

* [Files and File I/O in Practical Common Lisp](http://gigamonkeys.com/book/files-and-file-io.html)


### Testing whether a file exists

Use the function
[`probe-file`](http://www.lispworks.com/documentation/HyperSpec/Body/f_probe_.htm)
which will return a
[generalized boolean](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#generalized_boolean) -
either `nil` if the file doesn't exists, or its
[truename](http://www.lispworks.com/documentation/HyperSpec/Body/20_ac.htm)
(which might be different from the argument you supplied).

~~~lisp
$ ln -s /etc/passwd foo

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


### Opening a file

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
    new file will replace the old one), `:append` (content is added to the file),
    `nil` (the stream variable will be bound to `nil`),
    and `:rename` (i.e. the old file is renamed).
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


### Reading files

#### Reading a file into a string or a list of lines

It's quite common to need to access the contents of a file in string
form, or to get a list of lines.

uiop is included in ASDF (there is no extra library to install or
system to load) and has the following functions:


~~~lisp
(uiop:read-file-string "file.txt")
~~~

and

~~~lisp
(uiop:read-file-lines "file.txt")
~~~

*Otherwise*, this can be achieved by using `read-line` or `read-char` functions,
that probably won't be the best solution. The file might not be divided into
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
of using elements of type character every time. For instance, you can set
`:element-type` type argument of `with-output-to-string`, `with-open-file` and
`make-array` functions to `'(unsigned-byte 8)` to read data in octets.

#### Reading with an utf-8 encoding

To avoid an `ASCII stream decoding error` you might want to specify an UTF-8 encoding:

~~~lisp
(with-open-file (in "/path/to/big/file"
                     :external-format :utf-8)
                 ...
~~~

#### Set SBCL's default encoding format to utf-8

Sometimes you don't control the internals of a library, so you'd
better set the default encoding to utf-8.  Add this line to your
`~/.sbclrc`:

    (setf sb-impl::*default-external-format* :utf-8)

and optionally

    (setf sb-alien::*default-c-string-external-format* :utf-8)

#### Reading a file one line at a time

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

#### Reading a file one character at a time

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

#### Looking one character ahead

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

#### Random access to a File

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

### Writing content to a file

With `with-open-file`, specify `:direction :output` and use `write-sequence` inside:

~~~lisp
(with-open-file (f <pathname> :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (write-sequence s f)))
~~~

If the file exists, you can also `:append` content to it.

If it doesn't exist, you can `:error` out. See the standard for more details.


The library [str](https://github.com/vindarel/cl-str) has a shortcut:

~~~lisp
(str:to-file "file.txt" content) ;; with optional options
~~~

### Getting the file extension

The file extension is a pathname type in Lisp parlance:

~~~lisp
(pathname-type "~/foo.org")  ;; => "org"
~~~


### Getting file attributes (size, access time,...)

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

### Listing files and directories

Some functions below return pathnames, so you might need the following:

~~~lisp
(namestring #p"/foo/bar/baz.txt")           ==> "/foo/bar/baz.txt"
(directory-namestring #p"/foo/bar/baz.txt") ==> "/foo/bar/"
(file-namestring #p"/foo/bar/baz.txt")      ==> "baz.txt"
~~~


#### Listing files in a directory

~~~lisp
(uiop:directory-files "./")
~~~

Returns a list of pathnames:

```
(#P"/home/vince/projects/cl-cookbook/.emacs"
 #P"/home/vince/projects/cl-cookbook/.gitignore"
 #P"/home/vince/projects/cl-cookbook/AppendixA.jpg"
 #P"/home/vince/projects/cl-cookbook/AppendixB.jpg"
 #P"/home/vince/projects/cl-cookbook/AppendixC.jpg"
 #P"/home/vince/projects/cl-cookbook/CHANGELOG"
 #P"/home/vince/projects/cl-cookbook/CONTRIBUTING.md"
 […]
```

#### Listing sub-directories

~~~lisp
(uiop:subdirectories "./")
~~~

```
(#P"/home/vince/projects/cl-cookbook/.git/"
 #P"/home/vince/projects/cl-cookbook/.sass-cache/"
 #P"/home/vince/projects/cl-cookbook/_includes/"
 #P"/home/vince/projects/cl-cookbook/_layouts/"
 #P"/home/vince/projects/cl-cookbook/_site/"
 #P"/home/vince/projects/cl-cookbook/assets/")
```

#### Traversing (walking) directories

See `uiop/filesystem:collect-sub*directories`. It takes as arguments:

- a `directory`
- a `recursep` function
- a `collectp` function
- a `collector` function

Given a directory, when `collectp` returns true with the directory,
call the `collector` function on the directory, and recurse
each of its subdirectories on which `recursep` returns true.

This function will thus let you traverse a filesystem hierarchy,
superseding the functionality of `cl-fad:walk-directory`.

The behavior in presence of symlinks is not portable. Use IOlib to handle such situations.

Example:

~~~lisp
(defparameter *dirs* nil "All recursive directories.")

(uiop:collect-sub*directories "~/cl-cookbook"
    (constantly t)
    (constantly t)
    (lambda (it) (push it *dirs*)))
~~~

With `cl-fad:walk-directory`, we can also collect files, not only subdirectories:

~~~lisp
(cl-fad:walk-directory "./"
  (lambda (name)
     (format t "~A~%" name))
   :directories t)
~~~



#### Finding files matching a pattern

Below we simply list files of a directory and check that their name
contains a given string.

~~~lisp
(remove-if-not (lambda (it)
                   (search "App" (namestring it)))
               (uiop:directory-files "./"))
~~~

```
(#P"/home/vince/projects/cl-cookbook/AppendixA.jpg"
 #P"/home/vince/projects/cl-cookbook/AppendixB.jpg"
 #P"/home/vince/projects/cl-cookbook/AppendixC.jpg")
```

We used `namestring` to convert a `pathname` to a string, thus a
sequence that `search` can deal with.


#### Finding files with a wildcard

We can not transpose unix wildcards to portable Common Lisp.

In pathname strings we can use `*` and `**` as wildcards. This works
in absolute and relative pathnames.

~~~lisp
(directory #P"*.jpg")
~~~

~~~lisp
(directory #P"**/*.png")
~~~


#### Change the default pathname

The concept of `.` denoting the current directory does not exist in
portable Common Lisp. This may exist in specific filesystems and
specific implementations.

Also `~` to denote the home directory does not exist. They may be
recognized by some implementations as non-portable extensions.


`*default-pathname-defaults*`provides a default for some pathname
operations.

~~~lisp
(let ((*default-pathname-defaults* (pathname "/bin/")))
          (directory "*sh"))
(#P"/bin/zsh" #P"/bin/tcsh" #P"/bin/sh" #P"/bin/ksh" #P"/bin/csh" #P"/bin/bash")
~~~

See also `(user-homedir-pathname)`.
