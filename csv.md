---
title: CSV Files
---

The [cl-csv](https://github.com/AccelerationNet/cl-csv) library handles
CSV reading and writing, including quoted fields, custom separators, and
streaming.  Together with
[data-table](https://github.com/AccelerationNet/data-table) it gives
you named-column access with automatic type coercion.

Install both with Quicklisp:

~~~lisp
(ql:quickload '("cl-csv" "data-table"))
~~~


## Reading CSV files

Read a CSV file into a list of lists (each inner list is one row):

~~~lisp
(cl-csv:read-csv #P"data.csv")
;; => (("name" "age" "city")
;;     ("Alice" "30" "Paris")
;;     ("Bob" "25" "London"))
~~~

All values come back as strings.  See [Transforming rows with
`:map-fn`](#transforming-rows-with-map-fn) below for converting types as
you read.

### Parsing from a string

~~~lisp
(cl-csv:read-csv "1,2,3
4,5,6")
;; => (("1" "2" "3") ("4" "5" "6"))
~~~

### Custom separators

Read a tab-delimited file:

~~~lisp
(cl-csv:read-csv #P"data.tsv" :separator #\Tab)
~~~

Use `:separator` with any character—semicolons are common in
European locales:

~~~lisp
(cl-csv:read-csv #P"data.csv" :separator #\;)
~~~


## Iterating over rows

### `do-csv` — side-effect loop

`do-csv` binds each row to a variable without building a result list,
so it is useful when you only need side effects:

~~~lisp
(let ((sum 0))
  (cl-csv:do-csv (row #P"numbers.csv")
    (incf sum (parse-integer (nth 0 row))))
  sum)
~~~

### Transforming rows with `:map-fn`

`:map-fn` receives each row (a list of strings) and returns whatever
you like.  The return values are collected into the result list.

Convert selected columns to Lisp types:

~~~lisp
(cl-csv:read-csv #P"people.csv"
  :map-fn (lambda (row)
            (list :name (nth 0 row)
                  :age (parse-integer (nth 1 row)))))
;; => ((:NAME "Alice" :AGE 30)
;;     (:NAME "Bob" :AGE 25))
~~~

Build CLOS instances directly:

~~~lisp
(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age  :initarg :age  :accessor person-age)))

(cl-csv:read-csv #P"people.csv"
  :map-fn (lambda (row)
            (make-instance 'person
                           :name (nth 0 row)
                           :age  (parse-integer (nth 1 row)))))
~~~

Note that `:map-fn` receives *every* row, including the header.  If
your file has a header row you want to skip, use `:skip-first-p t`:

~~~lisp
(cl-csv:read-csv #P"people.csv"
  :skip-first-p t
  :map-fn (lambda (row)
            (list :name (nth 0 row)
                  :age (parse-integer (nth 1 row)))))
~~~


## Named columns with data-table

When your CSV has a header row, `data-table` gives you a table object
with named columns and automatic type guessing so you can stop
counting column indices.

~~~lisp
(ql:quickload "cl-csv")  ;; data-table comes as a dependency

(cl-csv:get-data-table-from-csv #P"sells.csv")
;; => #<DATA-TABLE:DATA-TABLE {10018A9F63}>
~~~

Describe the result to see what was inferred:

~~~lisp
(describe *)
;; COLUMN-NAMES = ("Date" "Type" "Quantity" "Total")
;; COLUMN-TYPES = (STRING STRING INTEGER DOUBLE-FLOAT)
;; ROWS = (("9 jan. 1975" "Sell" 1 9.90d0) ...)
~~~

`get-data-table-from-csv` assumes by default that the first row
contains column names (`:has-column-names t`) and guesses column types
(`:munge-types t`), converting strings to integers or floats where
possible.

### Accessing data

Access all rows:

~~~lisp
(data-table:rows dt)
~~~

Access a single cell by row object and column name:

~~~lisp
(data-table:data-table-value dt :row row :col-name "Total")
~~~

Or by row and column index:

~~~lisp
(data-table:data-table-value dt :row-idx 0 :col-idx 2)
~~~

### Round-tripping back to CSV

~~~lisp
(data-table:data-table-to-csv dt)          ;; to *standard-output*
(data-table:data-table-to-csv dt stream)   ;; to a stream
~~~


## Writing CSV files

Write a list of rows to a stream:

~~~lisp
(cl-csv:write-csv
  '(("name" "age") ("Alice" "30"))
  :stream *standard-output*)
;; prints:
;; "name","age"
;; "Alice","30"
~~~

Write to a file:

~~~lisp
(with-open-file (out #P"output.csv"
                 :direction :output
                 :if-exists :supersede)
  (cl-csv:write-csv
    '(("name" "age") ("Alice" "30"))
    :stream out))
~~~


## See also

- [cl-csv documentation](https://github.com/AccelerationNet/cl-csv)
- [data-table source](https://github.com/AccelerationNet/data-table)
- [Read CSV files in Common Lisp](https://dev.to/vindarel/read-csv-files-in-common-lisp-cl-csv-data-table-3c9n) — a tutorial covering cl-csv and data-table in depth
- [Files and Directories](files.html) — reading and writing files in general
