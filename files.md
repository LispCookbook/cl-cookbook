---
title: Files and Directories
---

We'll see here a handful of functions and libraries to operate on files and directories.

In this chapter, we use mainly
[namestrings](http://www.lispworks.com/documentation/HyperSpec/Body/19_aa.htm)
to
[specify filenames](http://www.lispworks.com/documentation/HyperSpec/Body/19_.htm).
In a recipe or two we also use
[pathnames](http://www.lispworks.com/documentation/HyperSpec/Body/19_ab.htm).

Many functions will come from UIOP, so we suggest you have a look directly at it:

* [UIOP/filesystem](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fFILESYSTEM)
* [UIOP/pathname](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fPATHNAME)

Of course, do not miss:

* [Files and File I/O in Practical Common Lisp](http://gigamonkeys.com/book/files-and-file-io.html)


### Getting the components of a pathname

#### File name (sans directory)

Use `file-namestring` to get a file name from a pathname:

~~~lisp
(file-namestring #p"/path/to/file.lisp") ;; => "file.lisp"
~~~

#### File extension

The file extension is called "pathname type" in Lisp parlance:

~~~lisp
(pathname-type "~/foo.org")  ;; => "org"
~~~

#### File basename

The basename is called the "pathname name" -

~~~lisp
(pathname-name "~/foo.org")  ;; => "foo"
(pathname-name "~/foo")      ;; => "foo"
~~~

If a directory pathname has a trailing slash, `pathname-name` may return `nil`; use `pathname-directory` instead -

~~~lisp
(pathname-name "~/foo/")     ;; => NIL
(first (last (pathname-directory #P"~/foo/"))) ;; => "foo"
~~~

#### Parent directory

~~~lisp
(uiop:pathname-parent-directory-pathname #P"/foo/bar/quux/")
;; => #P"/foo/bar/"
~~~

### Testing whether a file exists

Use the function
[`probe-file`](http://www.lispworks.com/documentation/HyperSpec/Body/f_probe_.htm)
which will return a
[generalized boolean](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#generalized_boolean) -
either `nil` if the file doesn't exists, or its
[truename](http://www.lispworks.com/documentation/HyperSpec/Body/20_ac.htm)
(which might be different from the argument you supplied).

For more portability, use `uiop:probe-file*` or `uiop:file-exists-p`
which will return the file pathname (if it exists).

~~~lisp
$ ln -s /etc/passwd foo

* (probe-file "/etc/passwd")
#p"/etc/passwd"

* (probe-file "foo")
#p"/etc/passwd"

* (probe-file "bar")
NIL
~~~

### Expanding a file or a directory name with a tilde (`~`)

For portability, use `uiop:native-namestring`:

~~~lisp
(uiop:native-namestring "~/.emacs.d/")
"/home/me/.emacs.d/"
~~~

It also expand the tilde with files and directories that don't exist:

~~~lisp
(uiop:native-namestring "~/foo987.txt")
:: "/home/me/foo987.txt"
~~~

On several implementations (CCL, ABCL, ECL, CLISP, LispWorks),
`namestring` works similarly. On SBCL, if the file or directory
doesn't exist, `namestring` doesn't expand the path but returns the
argument, with the tilde.

With files that exist, you can also use `truename`. But, at least on
SBCL, it returns an error if the path doesn't exist.

### Turning a pathname into a string with Windows' directory separator

Use again `uiop:native-namestring`:

~~~lisp
CL-USER> (uiop:native-namestring #p"~/foo/")
"C:\\Users\\You\\foo\\"
~~~

See also `uiop:parse-native-namestring` for the inverse operation.

### Creating directories

The function
[ensure-directories-exist](http://www.lispworks.com/documentation/HyperSpec/Body/f_ensu_1.htm)
creates the directories if they do not exist:

~~~lisp
(ensure-directories-exist "foo/bar/baz/")
~~~

This may create `foo`, `bar` and `baz`. Don't forget the trailing slash.

### Deleting directories

Use `uiop:delete-directory-tree` with a pathname (`#p`), a trailing slash and the `:validate` key:

~~~lisp
;; mkdir dirtest
(uiop:delete-directory-tree #p"dirtest/" :validate t)
~~~

You can use `pathname` around a string that designates a directory:

~~~lisp
(defun rmdir (path)
  (uiop:delete-directory-tree (pathname path) :validate t))
~~~

UIOP also has `delete-empty-directory`

[cl-fad][cl-fad] has `(fad:delete-directory-and-files "dirtest")`.

### Merging files and directories

Use `merge-pathnames`, with one thing to note: if you want to append
directories, the second argument must have a trailing `/`.

As always, look at UIOP functions. We have a `uiop:merge-pathnames*`
equivalent which fixes corner cases.

So, here's how to append a directory to another one:

~~~lisp
(merge-pathnames "otherpath" "/home/vince/projects/")
;; important:                                     ^^
;; a trailing / denotes a directory.
;; => #P"/home/vince/projects/otherpath"
~~~

Look at the difference: if you don't include a trailing slash to
either paths, `otherpath` and `projects` are seen as files, so `otherpath` is appended to the base directory containing `projects`:

~~~lisp
(merge-pathnames "otherpath" "/home/vince/projects")
;; #P"/home/vince/otherpath"
;;               ^^ no "projects", because it was seen as a file.
~~~

or again, with `otherpath/` (a trailing `/`) but `projects` seen as a file:

~~~lisp
(merge-pathnames "otherpath/" "/home/vince/projects")
;; #P"/home/vince/otherpath/projects"
;;                ^^ inserted here
~~~

### Get the current working directory (CWD)

Use `uiop/os:getcwd`:

~~~lisp
(uiop/os:getcwd)
;; #P"/home/vince/projects/cl-cookbook/"
;;                                    ^ with a trailing slash, useful for merge-pathnames
~~~

### Get the current directory relative to a Lisp project

Use `asdf:system-relative-pathname system path`.

Say you are working inside `mysystem`. It has an ASDF system
declaration, the system is loaded in your Lisp image. This ASDF file
is somewhere on your filesystem and you want the path to `src/web/`. Do this:

~~~lisp
(asdf:system-relative-pathname "mysystem" "src/web/")
;; => #P"/home/vince/projects/mysystem/src/web/"
~~~

This will work on another user's machine, where the system sources are located in another location.


### Setting the current working directory

Use [`uiop:chdir`](https://asdf.common-lisp.dev/uiop.html#Function-uiop_002fos_003achdir) _`path`_:

~~~lisp
(uiop:chdir "/bin/")
0
~~~

The trailing slash in _path_ is optional.

Or, to set for the current directory for the next operation only, use `uiop:with-current-directory`:

~~~lisp
(let ((dir "/path/to/another/directory/"))
  (uiop:with-current-directory (dir)
      (directory-files "./")))
~~~


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
  (your code here))
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
you just want to open an existing file for reading.

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
better set the default encoding to utf-8. Add this line to your
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
    (write-sequence s f))
~~~

If the file exists, you can also `:append` content to it.

If it doesn't exist, you can `:error` out. See [the standard](http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm) for more details.

#### Using libraries

The library [Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html#Conses)
has a function called [write-string-into-file](https://gitlab.common-lisp.net/alexandria/alexandria/-/blob/master/alexandria-1/io.lisp#L73)

~~~lisp
(alexandria:write-string-into-file content "file.txt")
~~~

Alternatively, the library [str](https://github.com/vindarel/cl-str) has the `to-file` function.

~~~lisp
(str:to-file "file.txt" content) ;; with optional options
~~~

Both `alexandria:write-string-into-file` and `str:to-file` take the same keyword arguments as `cl:open` that controls file creation: `:if-exists` and `if-does-not-exists`.

### Getting file attributes (size, access time,...)

[Osicat](https://www.common-lisp.net/project/osicat/)
is a lightweight operating system interface for Common Lisp on
POSIX-like systems, including Windows. With Osicat we can get and set
**environment variables** (now doable with `uiop:getenv`),
manipulate **files and directories**,
**pathnames** and a bit more.

[file-attributes](https://github.com/Shinmera/file-attributes/) is a
newer and lighter OS portability library specifically for getting file attributes,
using system calls (cffi).

SBCL with its `sb-posix` contrib can be used too.

#### File attributes (Osicat)

Once Osicat is installed, it also defines the `osicat-posix` system,
which permits us to get file attributes.

~~~lisp
(ql:quickload "osicat")

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

#### File attributes (file-attributes)

Install the library with

    (ql:quickload "file-attributes")

Its package is `org.shirakumo.file-attributes`. You can use a
package-local nickname for a shorter access to its functions, for example:

~~~lisp
(uiop:add-package-local-nickname :file-attributes :org.shirakumo.file-attributes)
~~~

Then simply use the functions:

- `access-time`, `modification-time`, `creation-time`. You can `setf` them.
- `owner`, `group`, and `attributes`. The values used are OS specific
for these functions. The attributes flag can be decoded and
encoded via a standardised form with `decode-attributes` and
`encode-attributes`.

~~~lisp
CL-USER> (file-attributes:decode-attributes
           (file-attributes:attributes #p"test.txt"))
(:READ-ONLY NIL :HIDDEN NIL :SYSTEM-FILE NIL :DIRECTORY NIL :ARCHIVED T :DEVICE
 NIL :NORMAL NIL :TEMPORARY NIL :SPARSE NIL :LINK NIL :COMPRESSED NIL :OFFLINE
 NIL :NOT-INDEXED NIL :ENCRYPTED NIL :INTEGRITY NIL :VIRTUAL NIL :NO-SCRUB NIL
 :RECALL NIL)
~~~

See [its documentation](https://shinmera.github.io/file-attributes).

#### File attributes (sb-posix)

This contrib is loaded by default on POSIX systems.

First get a stat object for a file, then get the stat you want:

~~~lisp
CL-USER> (sb-posix:stat "test.txt")
#<SB-POSIX:STAT {10053FCBE3}>

CL-USER> (sb-posix:stat-mtime *)
1686671405
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

#### Iterating on files (lazily)

In addition of the above functions, we mention solutions that *lazily*
traverse a directory. They don't load the entire list of files before
returning it.

Osicat has `with-directory-iterator`:

~~~lisp
(with-directory-iterator (next "/")
  (loop for entry = (next)
        while entry
        when (member :group-write (file-permissions entry))
        collect entry))
;; => (#P"tmp/")
~~~

LispWorks has the [fast-directory-files](https://www.lispworks.com/documentation/lw80/lw/lw-hcl-74.htm#LWUGRM) function, and AllegroCL has [map-over-directory](https://franz.com/support/documentation/10.1/doc/operators/excl/map-over-directory.htm).

#### Traversing (walking) directories recursively

See `uiop/filesystem:collect-sub*directories`. It takes as arguments:

- a `directory`
- a `collectp` function
- a `recursep` function
- a `collector` function

Given a directory, when `collectp` returns true with the directory,
call the `collector` function on the directory, and recurse
each of its subdirectories on which `recursep` returns true.

This function will thus let you traverse a filesystem hierarchy,
superseding the functionality of `cl-fad:walk-directory`.

The behavior in presence of symlinks is not portable. Use IOlib to handle such situations.

Examples:

- this collects only subdirectories:

~~~lisp
(defparameter *dirs* nil "All recursive directories.")

(uiop:collect-sub*directories "~/cl-cookbook"
    (constantly t)
    (constantly t)
    (lambda (it) (push it *dirs*)))
~~~

- this collects files and subdirectories:

~~~lisp
(let ((results))
    (uiop:collect-sub*directories
     "./"
     (constantly t)
     (constantly t)
     (lambda (subdir)
       (setf results
             (nconc results
                    ;; A detail: we return strings, not pathnames.
                    (loop for path in (append (uiop:subdirectories subdir)
                                              (uiop:directory-files subdir))
                          collect (namestring path))))))
    results)
~~~

- we can do the same with the `cl-fad` library:

~~~lisp
(cl-fad:walk-directory "./"
  (lambda (name)
     (format t "~A~%" name))
   :directories t)
~~~

- and of course, we can use an external tool: the good ol' unix `find`, or the newer `fd` (`fdfind` on Debian) that has a simpler syntax and filters out a set of common files and directories by default (node_modules, .git…):

~~~lisp
(str:lines (uiop:run-program (list "find" ".") :output :string))
;; or
(str:lines (uiop:run-program (list "fdfind") :output :string))
~~~

Here with the help of the `str` library.


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

[cl-fad]: https://edicl.github.io/cl-fad/
