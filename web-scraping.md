---
title: Web Scraping
---

The set of tools to do web scraping in Common Lisp is pretty complete
and pleasant. In this short tutorial we'll see how to make http
requests, parse html, extract content and do asynchronous requests.

Our simple task will be to extract the list of links on the CL
Cookbook's index page and check if they are reachable.

We'll use the following libraries:

- [Dexador](https://github.com/fukamachi/dexador) - an HTTP client
  (that aims at replacing the venerable Drakma),
- [Plump](https://shinmera.github.io/plump/) - a markup parser, that works on malformed HTML,
- [Lquery](https://shinmera.github.io/lquery/) - a DOM manipulation
  library, to extract content from our Plump result,
- [lparallel](https://lparallel.org/pmap-family/) -  a library for parallel programming (read more in the [process section](process.html)).

Before starting let's install those libraries with Quicklisp:

~~~lisp
(ql:quickload '(:dexador :plump :lquery :lparallel))
~~~

## HTTP Requests

Easy things first. Install Dexador. Then we use the `get` function:

~~~lisp
(defvar *url* "https://lispcookbook.github.io/cl-cookbook/")
(defvar *request* (dex:get *url*))
~~~

This returns a list of values: the whole page content, the return code
(200), the response headers, the uri and the stream.

```
"<!DOCTYPE html>
 <html lang=\"en\">
  <head>
    <title>Home &ndash; the Common Lisp Cookbook</title>
    […]
    "
200
#<HASH-TABLE :TEST EQUAL :COUNT 19 {1008BF3043}>
#<QURI.URI.HTTP:URI-HTTPS https://lispcookbook.github.io/cl-cookbook/>
#<CL+SSL::SSL-STREAM for #<FD-STREAM for "socket 192.168.0.23:34897, peer: 151.101.120.133:443" {100781C133}>>

```

Remember, in Slime we can inspect the objects with a right-click on
them.

## Parsing and extracting content with CSS selectors

We'll use `lquery` to parse the html and extract the
content.

- [https://shinmera.github.io/lquery/](https://shinmera.github.io/lquery/)

We first need to parse the html into an internal data structure. Use
`(lquery:$ (initialize <html>))`:

~~~lisp
(defvar *parsed-content* (lquery:$ (initialize *request*)))
;; => #<PLUMP-DOM:ROOT {1009EE5FE3}>
~~~

lquery uses [plump](https://shinmera.github.io/plump/) internally.

Now we'll extract the links with CSS selectors.

__Note__: to find out what should be the CSS selector of the element
I'm interested in, I right click on an element in the browser and I
choose "Inspect element". This opens up the inspector of my browser's
web dev tool and I can study the page structure.

So the links I want to extract are in a page with an `id` of value
"content", and they are in regular list elements (`li`).

Let's try something:

~~~lisp
(lquery:$ *parsed-content* "#content li")
;; => #(#<PLUMP-DOM:ELEMENT li {100B3263A3}> #<PLUMP-DOM:ELEMENT li {100B3263E3}>
;;  #<PLUMP-DOM:ELEMENT li {100B326423}> #<PLUMP-DOM:ELEMENT li {100B326463}>
;;  #<PLUMP-DOM:ELEMENT li {100B3264A3}> #<PLUMP-DOM:ELEMENT li {100B3264E3}>
;;  #<PLUMP-DOM:ELEMENT li {100B326523}> #<PLUMP-DOM:ELEMENT li {100B326563}>
;;  #<PLUMP-DOM:ELEMENT li {100B3265A3}> #<PLUMP-DOM:ELEMENT li {100B3265E3}>
;;  #<PLUMP-DOM:ELEMENT li {100B326623}> #<PLUMP-DOM:ELEMENT li {100B326663}>
;;  […]
~~~

Wow it works ! We get here a vector of plump elements.

I'd like to easily check what those elements are. To see the entire
html, we can end our lquery line with `(serialize)`:

~~~lisp
(lquery:$  *parsed-content* "#content li" (serialize))
#("<li><a href=\"license.html\">License</a></li>"
  "<li><a href=\"getting-started.html\">Getting started</a></li>"
  "<li><a href=\"editor-support.html\">Editor support</a></li>"
  […]
~~~

And to see their *textual* content (the user-visible text inside the
html), we can use `(text)` instead:

~~~lisp
(lquery:$  *parsed-content* "#content" (text))
#("License" "Editor support" "Strings" "Dates and Times" "Hash Tables"
  "Pattern Matching / Regular Expressions" "Functions" "Loop" "Input/Output"
  "Files and Directories" "Packages" "Macros and Backquote"
  "CLOS (the Common Lisp Object System)" "Sockets" "Interfacing with your OS"
  "Foreign Function Interfaces" "Threads" "Defining Systems"
  […]
  "Pascal Costanza’s Highly Opinionated Guide to Lisp"
  "Loving Lisp - the Savy Programmer’s Secret Weapon by Mark Watson"
  "FranzInc, a company selling Common Lisp and Graph Database solutions.")
~~~

All right, so we see we are manipulating what we want. Now to get their
`href`, a quick look at lquery's doc and we'll use `(attr
"some-name")`:


~~~lisp
(lquery:$  *parsed-content* "#content li a" (attr :href))
;; => #("license.html" "editor-support.html" "strings.html" "dates_and_times.html"
;;  "hashes.html" "pattern_matching.html" "functions.html" "loop.html" "io.html"
;;  "files.html" "packages.html" "macros.html"
;;  "/cl-cookbook/clos-tutorial/index.html" "os.html" "ffi.html"
;;  "process.html" "systems.html" "win32.html" "testing.html" "misc.html"
;;  […]
;;  "http://www.nicklevine.org/declarative/lectures/"
;;  "http://www.p-cos.net/lisp/guide.html" "https://leanpub.com/lovinglisp/"
;;  "https://franz.com/")
~~~

*Note*: using `(serialize)` after `attr` leads to an error.

Nice, we now have the list (well, a vector) of links of the
page. We'll now write an async program to check and validate they are
reachable.

External resources:

- [CSS selectors](https://developer.mozilla.org/en-US/docs/Glossary/CSS_Selector)

## Async requests

In this example we'll take the list of url from above and we'll check
if they are reachable. We want to do this asynchronously, but to see
the benefits we'll first do it synchronously !

We need a bit of filtering first to exclude the email addresses (maybe
that was doable in the CSS selector ?).

We put the vector of urls in a variable:

~~~lisp
(defvar *urls* (lquery:$  *parsed-content* "#content li a" (attr :href)))
~~~

We remove the elements that start with "mailto:": (a quick look at the
[strings](strings.html) page will help)

~~~lisp
(remove-if (lambda (it) (string= it "mailto:" :start1 0 :end1 (length "mailto:"))) *urls*)
;; => #("license.html" "editor-support.html" "strings.html" "dates_and_times.html"
;;  […]
;;  "process.html" "systems.html" "win32.html" "testing.html" "misc.html"
;;  "license.html" "http://lisp-lang.org/"
;;  "https://github.com/CodyReichert/awesome-cl"
;;  "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm"
;;  […]
;;  "https://franz.com/")
~~~

Actually before writing the `remove-if` (which works on any sequence,
including vectors) I tested with a `(map 'vector …)` to see that the
results where indeed `nil` or `t`.

As a side note, there is a handy `starts-with` function in
[cl-strings](https://github.com/diogoalexandrefranco/cl-strings/),
available in Quicklisp. So we could do:

~~~lisp
(map 'vector (lambda (it) (cl-strings:starts-with it "mailto:")) *urls*)
~~~

it also has an option to ignore or respect the case.

While we're at it, we'll only consider links starting with "http", in
order not to write too much stuff irrelevant to web scraping:

~~~lisp
(remove-if-not (lambda (it) (string= it "http" :start1 0 :end1 (length "http"))) *) ;; note the remove-if-NOT
~~~

All right, we put this result in another variable:

~~~lisp
(defvar *filtered-urls* *)
~~~

and now to the real work. For every url, we want to request it and
check that its return code is 200. We have to ignore certain
errors. Indeed, a request can timeout, be redirected (we don't want
that) or return an error code.

To be in real conditions we'll add a link that times out in our list:

~~~lisp
(setf (aref *filtered-urls* 0) "http://lisp.org")  ;; too bad indeed
~~~

We'll take the simple approach to ignore errors and return `nil` in
that case. If all goes well, we return the return code, that should be
200.

As we saw at the beginning, `dex:get` returns many values, including
the return code. We'll catch only this one with `nth-value` (instead
of all of them with `multiple-value-bind`) and we'll use
`ignore-errors`, that returns nil in case of an error. We could also
use `handler-case` and catch specific error types (see examples in
dexador's documentation) or (better yet ?) use `handler-bind` to catch
any `condition`.

(*ignore-errors has the caveat that when there's an error, we can not
return the element it comes from. We'll get to our ends though.*)


~~~lisp
(map 'vector (lambda (it)
  (ignore-errors
    (nth-value 1 (dex:get it))))
  *filtered-urls*)
~~~

we get:

```
#(NIL 200 200 200 200 200 200 200 200 200 200 NIL 200 200 200 200 200 200 200
  200 200 200 200)
```

it works, but *it took a very long time*. How much time precisely ?
with `(time …)`:

```
Evaluation took:
  21.554 seconds of real time
  0.188000 seconds of total run time (0.172000 user, 0.016000 system)
  0.87% CPU
  55,912,081,589 processor cycles
  9,279,664 bytes consed
```

21 seconds ! Obviously this synchronous method isn't efficient. We
wait 10 seconds for links that time out. It's time to write and
measure and async version.

After installing `lparallel` and looking at
[its documentation](https://lparallel.org/), we see that the parallel
map [pmap](https://lparallel.org/pmap-family/) seems to be what we
want. And it's only a one word edit. Let's try:

~~~lisp
(time (lparallel:pmap 'vector
  (lambda (it)
    (ignore-errors (let ((status (nth-value 1 (dex:get it)))) status)))
  *filtered-urls*)
;;  Evaluation took:
;;  11.584 seconds of real time
;;  0.156000 seconds of total run time (0.136000 user, 0.020000 system)
;;  1.35% CPU
;;  30,050,475,879 processor cycles
;;  7,241,616 bytes consed
;;
;;#(NIL 200 200 200 200 200 200 200 200 200 200 NIL 200 200 200 200 200 200 200
;;  200 200 200 200)
~~~

Bingo. It still takes more than 10 seconds because we wait 10 seconds
for one request that times out. But otherwise it proceeds all the http
requests in parallel and so it is much faster.

Shall we get the urls that aren't reachable, remove them from our list
and measure the execution time in the sync and async cases ?

What we do is: instead of returning only the return code, we check it
is valid and we return the url:

~~~lisp
... (if (and status (= 200 status)) it) ...
(defvar *valid-urls* *)
~~~

we get a vector of urls with a couple of `nil`s: indeed, I thought I
would have only one unreachable url but I discovered another
one. Hopefully I have pushed a fix before you try this tutorial.

But what are they ? We saw the status codes but not the urls :S We
have a vector with all the urls and another with the valid ones. We'll
simply treat them as sets and compute their difference. This will show
us the bad ones. We must transform our vectors to lists for that.

~~~lisp
(set-difference (coerce *filtered-urls* 'list)
                (coerce *valid-urls* 'list))
;; => ("http://lisp-lang.org/" "http://www.psg.com/~dlamkins/sl/cover.html")
~~~

Gotcha !

BTW it takes 8.280 seconds of real time to me to check the list of
valid urls synchronously, and 2.857 seconds async.

Have fun doing web scraping in CL !


More helpful libraries:

- we could use [VCR](https://github.com/tsikov/vcr), a store and
  replay utility to set up repeatable tests or to speed up a bit our
  experiments in the REPL.
- [cl-async](https://github.com/orthecreedence/cl-async),
  [carrier](https://github.com/orthecreedence/carrier) and others
  network, parallelism and concurrency libraries to see on the
  [awesome-cl](https://github.com/CodyReichert/awesome-cl) list,
  [Cliki](http://www.cliki.net/) or
  [Quickdocs](http://quickdocs.org/search?q=web).
