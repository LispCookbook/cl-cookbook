---
title: CSV parsing and data frames
---

What are our options to parse and write CSV files?

We could split CSV lines manually (`subseq`, `split-sequence`,
`str:split`, `cl-ppcre`…) but what if a column contains the separator?
A library starts to be useful.

A library will be even more so to help us manipulate columns by name,
to loop over rows, or use tabular data structures, to automatically
parse columns to numbers, and so on.

Editor's tips:

- for simple use, try `cl-csv`
- if you'll do statistics or want a good dataframes library, go for `lisp-stats`
- to work with large datasets, see `cl-csv`'s `:row-fn` argument.
- for the fastest parsing, maybe `fare-csv`, or DuckDB with
  [cl-duckdb](https://github.com/ak-coram/cl-duckdb), which can also
  ingest [much more file
  formats](https://duckdb.org/docs/2.0/data/data_sources) than CSV,
  including Excel.


# Introduction: our test files

Let's work together with this simple CSV file, `persons.csv`:

```txt
Alice,7,1
Bob,77,0
```

The separator is `,` (often the default in libraries).

For simplicity, the file doesn't contain a headers row.

This second file, `persons2.csv`, contains headers:

```txt
name,age,lisper
Alice,7,1
Bob,77,0
```


# Parsing with cl-csv

The [cl-csv](https://github.com/AccelerationNet/cl-csv) library is on Quicklisp.

```lisp
(ql:quicklisp "cl-csv")
```

It allows to parse data from a string, from a file, or from any stream.

It may not be the fastest, but it has a decent set of quality-of-life
features.

Please go read its README, as we don't illustrate some of its features
here. For example, it supports multiline quoted data, we can set the
quote character, it has good error messages (line and column number,
char index), etc.

For reference, this is the function signature of `cl-csv:read-csv` (`C-c-s` on Slime):

~~~lisp
(cl-csv:read-csv stream-or-string :csv-reader csv-reader
                                             :row-fn row-fn
                                             :map-fn map-fn
                                             :data-map-fn data-map-fn
                                             :sample sample
                                             :skip-first-p skip-first-p
                                             :separator *separator*
                                             :quote *quote*
                                             :escape *quote-escape*
                                             :unquoted-empty-string-is-nil *unquoted-empty-string-is-nil*
                                             :quoted-empty-string-is-nil *quoted-empty-string-is-nil*
                                             :trim-outer-whitespace *trim-outer-whitespace*
                                             :newline *read-newline*
                                             :escape-mode *escape-mode*)
~~~

## Parsing from a string

To parse from a string, just use `cl-csv:read-csv`:

```lisp
(cl-csv:read-csv "Alice,7,1
Bob,77,0")
;; =>
(("Alice" "7" "1") ("Bob" "77" "0"))
```

It returns a list of lists, values are all strings.

## Parsing from a file

Also use `cl-csv:read-csv` but give *a pathname* as argument, not only
the filename as a string.

```lisp
(cl-csv:read-csv #P"persons.csv")
;; =>
(("Alice,7,1") ("Bob,77,0"))
```

What happens if you omit the `#P`?

~~~lisp
(cl-csv:read-csv "persons.csv")
;; =>
(("persons.csv"))
~~~

this looks strange and no, there is no bug ;)

To parse from a stream, see the library's documentation.

## Using another separator

Is `;` the columns separator? Use `cl-csv:read-csv`'s `:separator`
argument, or the global `cl-csv:*separator*` parameter.

For a Tab, use `#\Tab`.

~~~lisp
(defparameter *csv-string* "Alice;7;1
Bob;77;0")

(cl-csv:read-csv *csv-string* :separator #\;)

;; OR

(let ((cl-csv:*separator* #\;))
  (cl-csv:read-csv *csv-string*))
~~~

## Skipping the first row of headers

With `cl-csv`, use `:skip-first-p`.

However, we'll use this row of headers as soon as we work with dataframes.

~~~lisp
(cl-csv:read-csv "name,age,lisper
Alice,7,1
Bob,77,0"
 :skip-first-p t)
~~~

## Returning a list of objects

So far, we got a list of *lists* with raw values as strings. We can do
whatever we want with them. `cl-csv` has a `:map-fn` argument that
we can use to create an object while parsing each row.

Let's re-use our class definition from the CLOS chapter:

```lisp
(defclass person ()
  ((name :initarg :name :initform "")
   (age :initarg :age :initform nil)
   (lisper :initarg :lisper :initform nil)))
```

We can parse our CSV and return a list of objects at once:

~~~lisp
(cl-csv:read-csv #p"persons.csv"
  :map-fn (lambda (values)
             (make-instance 'person :name (nth 0 values)
                                    :age (parse-integer (nth 1 values))
                                    :lisper (if (equal "1" (nth 2 values)) t nil))))
~~~

Observe that:

- we had to parse the age from a string to an integer ourselves
- our file doesn't have a headers row.

## Streaming reads (for large files)

`cl-csv` provides the `:row-fn` parameter, that *does something* with
each row and *doesn't return them*. This allows us to work with large
datasets.

In this example, the `:row-fn` function has *side-effects* after each
row was read and passed to it. Our function returns a result, the mean
of ages.


~~~lisp
(let ((sum-ages 0)
      (nb-ages 0))
  (cl-csv:read-csv #p"persons.csv"
                   :row-fn (lambda (values)
                             (incf sum-ages (parse-integer (nth 1 values)))
                             (incf nb-ages)))
  ;; Age mean:
  (/ sum-ages nb-ages))
~~~

## Using cl-csv's reader information for debugging

cl-csv makes efforts to tell us where it is at its parsing. Here's
where to find the bits of information that will help us during
debugging.

In this dummy example, we'll observe cl-csv's hints:

```txt
;; this is uncomplete CSV, parsing will raise an error:
(cl-csv:read-csv "Alice,7,1
Bob,77,0
Oh?")

;; =>

The value
  NIL
is not of type
  STRING
when binding STRING
   [Condition of type TYPE-ERROR]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] Exit debugger, returning to top level.

Backtrace:
  0: (SB-IMPL::PARSE-INTEGER10 NIL 0 NIL NIL) [external]
  1: ((LAMBDA (VALUES)) ("oh?"))
  2: (CL-CSV::READ-CSV-WITH-READER #P"persons.csv" :CSV-READER NIL :ROW-FN #<FUNCTION (LAMBDA (VALUES)) {1203B956DB}> :MAP-FN NIL :DATA-MAP-FN NIL :SKIP-FIRST-P NIL)
      Locals:
        #:.DEFAULTING-TEMP. = NIL
        CSV-READER = #<CL-CSV::CSV-READER LINE-IDX:2 CHARACTER-LINE-IDX:4 CHARACTER-IDX:23 "" {1203B99343}>

        ^^^^^ let's see more about this READER object.

        DATA-MAP-FN-P = NIL
        IN-STREAM = #<SB-SYS:FD-STREAM for "file /home/vindarel/cl-cookbook/persons.csv" {1203B96553}>
        MAP-FN-P = NIL
        #:NEW3 = #<FUNCTION (LAMBDA (VALUES)) {1203B956DB}>
        #:NEW5 = NIL
        #:NEW7 = NIL
        #:NEW9 = NIL
        ROW-FN-P = T
        STREAM-OR-STRING = #P"persons.csv"
  3: (CL-CSV:READ-CSV #P"persons.csv" :ROW-FN #<FUNCTION (LAMBDA (VALUES)) {1203B956DB}>)
```

We don't see everything. Click or press Enter on the `CSV-READER` that is on the 2nd backtrace:

```txt
#<CL-CSV::CSV-READER {1203B99343}>
--------------------
Class: #<STANDARD-CLASS CL-CSV::CSV-READER>
--------------------
 Group slots by inheritance [ ]
 Sort slots alphabetically  [X]

All Slots:
[ ]  AFTER-QUOTED?      = NIL
[ ]  BUFFER             = ""
[ ]  CHARACTER-IDX      = 23         <------------ useful: character index in the file
[ ]  CHARACTER-LINE-IDX = 4
[ ]  DATA-MAP-FN        = CL-CSV::MAP-EMPTY-STRING-TO-NIL
[ ]  ENTRIES            = #(#<CL-CSV::READ-DISPATCH-TABLE-ENTRY {1203B98F93}> #<CL-CSV::READ-DISPATCH-TABLE-ENTRY {1203B99033}> #<CL-CSV::READ-DISPATCH-TABLE-ENTRY {1203B990D3}> #<CL-CSV::READ-DISPATCH-TABLE-ENTRY {1203B99173}> #<CL-CSV::READ-DISPATCH-TABLE-ENTRY {1203B99213}> #<CL-CSV::READ-DISPATCH-TABLE-ENTRY {1203B992B3}>)
[ ]  LINE-DATA          = #()
[ ]  LINE-IDX           = 2           <------------- useful: line index
[ ]  MAP-FN             = NIL
[ ]  PARSE-STREAM       = #<SB-SYS:FD-STREAM for "file /home/vindarel/cl-cookbook/persons.csv" {1203B96553}>
[ ]  READING-QUOTED?    = NIL
[ ]  ROW-FN             = #<FUNCTION (LAMBDA (VALUES)) {1203B956DB}>
[ ]  ROWS               = #()
[ ]  SKIP-ROW?          = NIL

[set value]  [make unbound]
```

Knowing the line causing an error and the file position is useful.

# cl-csv and data-table: column names, column types, columns manipulation

[data-table](https://github.com/AccelerationNet/data-table) is a
library by the same authors that brings a data structure that
represents our CSV data.

A data table has rows of data, associates names to the columns, and guesses columns types.

Then, we can access values by row and column names, and manipulate columns (add data, take a substract etc).

We link cl-csv and data-tables together with the system `cl-csv-data-table` (defined in `cl-csv`).

~~~lisp
(ql:quickload '("cl-csv" "data-table" "cl-csv-data-table"))
~~~

You don't need to go read its README, as it is poorly documented (only in tests).

We'll use the second CSV file, `persons2.csv`.

Let's parse it:

```lisp
(cl-csv:get-data-table-from-csv #p"persons2.csv")
;; =>
#<DATA-TABLE:DATA-TABLE {1205D151C3}>
```

This returns a `DATA-TABLE` object. What is it? We can inspect it:

```txt
(inspect *)

The object is a STANDARD-OBJECT of type DATA-TABLE:DATA-TABLE.
0. COLUMN-NAMES: ("name" "age" "lisper")
1. COLUMN-TYPES: (STRING INTEGER INTEGER)
2. ROWS: (("Alice" 7 1) ("Bob" 77 0))
```

We observe:

- the data-table used our headers row
- the columns' types were inferred: "name" is a string, "age" and
  "lisper" are integers.
- in each row, the "age" and "lisper" fields were parsed to an integer.

Let's save our result to a parameter:

```lisp
(defparameter *dt* **)
```

Get all rows with:

```lisp
(data-table:rows *dt*)
```

Instead of accessing a column by its index, you can access it by its name.

Use `data-table:data-table-value dt &key row row-idx col-name col-idx`.

For example, what is the "name" of the first row?

```lisp
(csv:data-table-value *dt* :row-idx 0 :col-name "age")
;; => 7
```

The library has more functions to manipulate columns and
data-tables. We give them as a reference, and then we'll see how lisp-stat does it.

`data-table:number-of-rows` and `data-table:number-of-columns`

`data-table-to-plists`, `plists-to-data-table`

Add columns together with `(csv:add-column dt column-name [default] [column-type] [index])`.

Select columns with `(csv:select-columns table column-names)` to create a new data-table.

In this example, we create a new data-table object with only 2
columns. We also inverted their order.

```lisp
(csv:select-columns *dt* '("age" "name"))
;; =>
#<DATA-TABLE:DATA-TABLE {1203F88213}>
CL-USER> (inspect *)

The object is a STANDARD-OBJECT of type DATA-TABLE:DATA-TABLE.
0. COLUMN-NAMES: ("age" "name")
1. COLUMN-TYPES: (INTEGER STRING)
2. ROWS: ((7 "Alice") (77 "Bob"))
> q
```

`overlay-region  new dt :col-idx col-idx :row-idx row-idx)`: "puts all values from new-dt into dt starting at row-idx col-idx".

`make-sub-table parent :uri uri :lri lri :uci uci :lci lci`:

```txt
Make a new data table from a subset of another data-table
   lci - low column index
   uci - upper column index (as per subseq 1 beyond the col you want)
   lri - low row index
   uri - upper row index (as per subseq 1 beyond the row you want)
```

Write a data-table to a file (or any stream) with `(data-table-to-csv dt &optional stream)`

All this is good, but today lisp-stat does this, and more.


# lisp-stat's Data Frame

[lisp-stat](https://lisp-stat.dev/) is a powerful library.

> Lisp-Stat provides a statistical computing engine comparable to R, Python, Julia and Matlab. Its major differentiators are a true REPL based development environment, high performance and a single binary image that can be moved through the devops pipeline in a secure, safe and manageable way.

It integrates with Jupyter notebooks, to Quarto, it can load R datasets…

It's pretty good and actively used and developed.

It interests us for its [Data
Frame](https://lisp-stat.dev/docs/manuals/data-frame/). You should go
read its documentation ;)

Let's load the library:

~~~lisp
(ql:quickload :lisp-stat)
~~~

We'll give a brief showcase. For this, we'll go to the `ls-user` package.
It is "the package for you to do statistics work".

~~~lisp
(in-package :ls-user)
~~~

We can read a CSV file with `read-csv`. This function is actually not
the one we used so far with cl-csv, it is a wrapper around `fare-csv`.

So, using another separator is different, use `fare-csv:*separator*`.

~~~lisp
(defparameter *persons-df* (read-csv #p"persons2.csv"))
;; => *PERSONS-DF*
~~~

It is important we used our file `persons2.csv` which contains the
first row with column names.

We can start working with our data.

How do we get all names of our dataset? Simply like this:

~~~lisp
LS-USER> (column *persons-df* 'name)
#("Alice" "Bob")
~~~

That's already interesting, isn't it?

Observe that:
- the result is not a list, it is an array.

lisp-stat has a notation trick to access data in an even shorter
way. For this, we need to define an "environment" with `defdf`:

~~~lisp
LS-USER> (defdf persons (read-csv #p"persons2.csv"))
;; => #<DATA-FRAME (2 observations of 3 variables)>
~~~

Observe that:
- we deliberately didn't use "\*earmuffs\*" around "persons"
- the environment already gives us information: it saw 2 rows and 3 columns.

Now, we can get all names simply with:

~~~lisp
LS-USER> persons:name   <----- you can use auto-completion, of course.
;; => #("Alice" "Bob")
~~~

all ages:

~~~lisp
LS-USER> persons:age
;; => #(7 77)
~~~

We can inspect the shape of our data:

~~~lisp
LS-USER> (head persons)

;;   NAME  AGE LISPER
;; 0 Alice   7      1
;; 1 Bob    77      0
~~~

Using another function, `select`, we can get all ages with:

~~~lisp
LS-USER> (select persons t 'age)
;;                      ^^^ take the age of all rows
~~~

But with `select` we can get a subset of values:

~~~lisp
LS-USER> (select persons '(0 1) 'age)
;;                         ^^^ our subset (it's all our data lol)
;; => #(7 77)
~~~

We can get values of some columns:

~~~lisp
LS-USER> (columns persons '(age name))
;; => #(#(7 77) #("Alice" "Bob"))
~~~

We can get a high-level description of our dataset:

```lisp
LS-USER> (describe persons)
PERSONS
  A data-frame with 2 observations of 3 variables

Variable | Type | Unit | Label
-------- | ---- | ---- | -----------
NAME     | NIL  | NIL  | NIL
AGE      | NIL  | NIL  | NIL
LISPER   | NIL  | NIL  | NIL
```

This shows that the columns' types were not inferred. Can we fix this?

```lisp
LS-USER> (heuristicate-types persons)
NIL

LS-USER> (describe persons)
PERSONS
  A data-frame with 2 observations of 3 variables

Variable | Type    | Unit | Label
-------- | ----    | ---- | -----------
NAME     | STRING  | NIL  | NIL
AGE      | INTEGER | NIL  | NIL
LISPER   | BIT     | NIL  | NIL
```

Yes, now they are.

We can now manipulate columns: add, remove, rename, replace, transform… as well as rows: add, count, loop over (do-rows), map into, mask, filter…

Everything is in lisp-stat's documentation!


# Utilities

## Print data in an ascii table

We saw how to do that with lisp-stat's `head` function.

We have another solution with the [cl-ansi-term](https://github.com/vindarel/cl-ansi-term) library.

~~~lisp
(ql:quickload '("cl-ansi-term"))
~~~

Its global nickname is `term` (warn: global nicknames are now legacy,
prefer package-local nicknames).

We use `term:table` to print our tabular data.

```lisp
(let ((csv (cl-csv:read-csv #p"persons2.csv")))
  (term:table csv))

;; =>
+-----+---+------+
|name |age|lisper|
+-----+---+------+
|Alice|7  |1     |
+-----+---+------+
|Bob  |77 |0     |
+-----+---+------+
```

It has a few more options. With many columns, you could display only a
chosen set of columns. You can style cells programmatically, etc.

# See also

- acknowldegment: [read CSV files in CL on dev.to](https://dev.to/vindarel/read-csv-files-in-common-lisp-cl-csv-data-table-3c9n)
- [auto-text](https://github.com/defunkydrummer/auto-text), automatic detection for text files (encoding, end of line, column width, csv delimiter etc). [inquisitor](https://github.com/t-sin/inquisitor) for detection of asian and far eastern languages.
- [CLAWK](https://github.com/sharplispers/clawk), an AWK implementation embedded into Common Lisp, to parse files line-by-line.
- [redditors' feedback](https://www.reddit.com/r/Common_Lisp/comments/1kht5ht/read_csv_files_in_common_lisp_clcsv_datatable/) about duckdb, SBCL maintainer's fast parsing snippet.
