---
title: Dates and Times
---

Common Lisp provides two different ways of looking at time: universal time,
meaning time in the "real world", and run time, meaning time as seen by your
computer's CPU. We will deal with both of them separately.

<a name="univ"></a>


## Built-in time functions

### Universal Time

Universal time is represented as the number of seconds that have elapsed since
00:00 of January 1, 1900 in the GMT time zone. The function
[`get-universal-time`](http://www.lispworks.com/documentation/HyperSpec/Body/f_get_un.htm)
returns the current universal time:

~~~lisp
CL-USER> (get-universal-time)
3220993326
~~~

Of course this value is not very readable, so you can use the function
[`decode-universal-time`](http://www.lispworks.com/documentation/HyperSpec/Body/f_dec_un.htm)
to turn it into a "calendar time" representation:

~~~lisp
CL-USER> (decode-universal-time 3220993326)
6
22
19
25
1
2002
4
NIL
5
~~~

**NB**: in the next section we'll use the `local-time` library to get more user-friendy functions, such as `(local-time:universal-to-timestamp (get-universal-time))` which returns `@2021-06-25T09:16:29.000000+02:00`.

This call to `decode-universal-time` returns nine values: `seconds, minutes, hours, day, month, year, day of
the week, daylight savings time flag and time zone`. Note that the day of the
week is represented as an integer in the range 0..6 with 0 being Monday and 6
being Sunday. Also, the **time zone** is represented as the number of hours you need
to add to the current time in order to get GMT time.

So in this example the
decoded time would be `19:22:06 of Friday, January 25, 2002`, in the EST time
zone, with no daylight savings in effect. This, of course, relies on the
computer's own clock, so make sure that it is set correctly (including the time
zone you are in and the DST flag). As a shortcut, you can use
[`get-decoded-time`](http://www.lispworks.com/documentation/HyperSpec/Body/f_get_un.htm)
to get the calendar time representation of the current time directly:

~~~lisp
CL-USER> (get-decoded-time)
~~~

is equivalent to

~~~lisp
CL-USER> (decode-universal-time (get-universal-time))
~~~

Here is an example of how to use these functions in a program (but frankly, use the `local-time` library instead):

~~~lisp
CL-USER> (defconstant *day-names*
           '("Monday" "Tuesday" "Wednesday"
	         "Thursday" "Friday" "Saturday"
	         "Sunday"))
*DAY-NAMES*

CL-USER> (multiple-value-bind
           (second minute hour day month year day-of-week dst-p tz)
    	   (get-decoded-time)
           (format t "It is now ~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
	    	 hour
	    	 minute
	    	 second
	    	 (nth day-of-week *day-names*)
	    	 month
	    	 day
	    	 year
	    	 (- tz)))
It is now 17:07:17 of Saturday, 1/26/2002 (GMT-5)
~~~

Of course the call to `get-decoded-time` above could be replaced by
`(decode-universal-time n)`, where n is any integer number, to print an
arbitrary date. You can also go the other way around: the function
[`encode-universal-time`](http://www.lispworks.com/documentation/HyperSpec/Body/f_encode.htm)
lets you encode a calendar time into the corresponding universal time. This
function takes six mandatory arguments (seconds, minutes, hours, day, month and
year) and one optional argument (the time zone) and it returns a universal time:

~~~lisp
CL-USER> (encode-universal-time 6 22 19 25 1 2002)
3220993326
~~~

Note that the result is automatically adjusted for daylight savings time if the time zone is not supplied. If it is supplied, than Lisp assumes that the specified time zone already accounts for daylight savings time, and no adjustment is performed.

Since universal times are simply numbers, they are easier and safer to manipulate than calendar times. Dates and times should always be stored as universal times if possible, and only converted to string representations for output purposes. For example, it is straightforward to know which of two dates came before the other, by simply comparing the two corresponding universal times with `<`.

### Internal Time

Internal time is the time as measured by your Lisp environment, using your computer's clock. It differs from universal time in three important respects. First, internal time is not measured starting from a specified point in time: it could be measured from the instant you started your Lisp, from the instant you booted your machine, or from any other arbitrary time point in the past. As we will see shortly, the absolute value of an internal time is almost always meaningless; only differences between internal times are useful. The second difference is that internal time is not measured in seconds, but in a (usually smaller) unit whose value can be deduced from [`internal-time-units-per-second`](http://www.lispworks.com/documentation/HyperSpec/Body/v_intern.htm):

~~~lisp
CL-USER> internal-time-units-per-second
1000
~~~

This means that in the Lisp environment used in this example, internal time is measured in milliseconds.

Finally, what is being measured by the "internal time" clock? There are actually two different internal time clocks in your Lisp:

- one of them measures the passage of "real" time (the same time that universal time measures, but in different units), and
- the other one measures the passage of CPU time, that is, the time your CPU spends doing actual computation for the current Lisp process.

On most modern computers these two times will be different, since your CPU will never be entirely dedicated to your program (even on single-user machines, the CPU has to devote part of its time to processing interrupts, performing I/O, etc). The two functions used to retrieve internal times are called [`get-internal-real-time`](http://www.lispworks.com/documentation/HyperSpec/Body/f_get_in.htm) and [`get-internal-run-time`](http://www.lispworks.com/documentation/HyperSpec/Body/f_get__1.htm) respectively. Using them, we can solve the above problem about measuring a function's run time, which is what the `time` built-in macro does.

~~~lisp
CL-USER> (time (sleep 1))
Evaluation took:
  1.000 seconds of real time
  0.000049 seconds of total run time (0.000044 user, 0.000005 system)
  0.00% CPU
  2,594,553,447 processor cycles
  0 bytes consed
~~~


## The `local-time` library

The [local-time](https://common-lisp.net/project/local-time/) library ([GitHub](https://github.com/dlowe-net/local-time/)) is a very handy extension to
the somewhat limited functionalities as defined by the standard.

In particular, it can

- print timestamps in various standard or custom formats (e.g. RFC1123 or RFC3339)
- parse timestrings,
- perform time arithmetic,
- convert Unix times, timestamps, and universal times to and from.

We present below what we find the most useful functions. See its [manual](https://common-lisp.net/project/local-time/manual.html) for the full details.

It is available in Quicklisp:

~~~lisp
CL-USER> (ql:quickload "local-time")
~~~

### Create timestamps (encode-timestamp, universal-to-timestamp)

Create a timestamp with `encode-timestamp`, giving it its number of nanoseconds, seconds, minutes, days, months and years:

~~~lisp
(local-time:encode-timestamp 0 0 0 0 1 1 1984)
@1984-01-01T00:00:00.000000+01:00
~~~

The complete signature is:

    **encode-timestamp** nsec sec minute hour day month year &key timezone offset into

    The offset is the number of seconds offset from UTC of the locale. If offset is not specified, the offset will be guessed from the timezone. If a timestamp is passed as the into argument, its value will be set and that timestamp will be returned. Otherwise, a new timestamp is created.

Create a timestamp from a universal time with `universal-to-timestamp`:

~~~lisp
(get-universal-time)
3833588757
(local-time:universal-to-timestamp (get-universal-time))
@2021-06-25T07:45:59.000000+02:00
~~~

You can also parse a human-readable time string:

~~~lisp
(local-time:parse-timestring "1984-01-01")
@1984-01-01T01:00:00.000000+01:00
~~~

But see the section on parsing timestrings for more.

### Get today's date (now, today)

Use `now` or `today`:

~~~lisp
(local-time:now)
@2019-11-13T20:02:13.529541+01:00

(local-time:today)
@2019-11-13T01:00:00.000000+01:00
~~~

"today" is the midnight of the current day in the UTC zone.

To compute "yesterday" and "tomorrow", see below.

### Add or substract times (timestamp+, timestamp-)

Use `timestamp+` and `timestamp-`. Each takes 3 arguments: a date, a number and a unit (and optionally a timezone and an offset):

~~~lisp
(local-time:now)
@2021-06-25T07:19:39.836973+02:00

(local-time:timestamp+ (local-time:now) 1 :day)
@2021-06-26T07:16:58.086226+02:00

(local-time:timestamp- (local-time:now) 1 :day)
@2021-06-24T07:17:02.861763+02:00
~~~

The available units are `:sec :minute :hour :day :year`.

This operation is also possible with `adjust-timestamp`, which can do a bit more as we'll see right in the next section (it can do many operations at once).

~~~lisp
(local-time:timestamp+ (today) 3 :day)
@2021-06-28T02:00:00.000000+02:00

(local-time:adjust-timestamp (today) (offset :day 3))
@2021-06-28T02:00:00.000000+02:00
~~~

Here's `yesterday` and `tomorrow` defined from `today`:

~~~lisp
(defun yesterday ()
  "Returns a timestamp representing the day before today."
  (timestamp- (today) 1 :day))

(defun tomorrow ()
  "Returns a timestamp representing the day after today."
  (timestamp+ (today) 1 :day))
~~~

### Modify timestamps with any offset (adjust-timestamp)

`adjust-timestamp`'s first argument is the timestamp we operate on, and then it accepts a full `&body changes` where a "change" is in the form `(offset :part value)`:

Please point to the previous Monday:

~~~lisp
(local-time:adjust-timestamp (today) (offset :day-of-week :monday))
@2021-06-21T02:00:00.000000+02:00
~~~

We can apply many changes at once. Travel in time:

~~~lisp
(local-time:adjust-timestamp (today)
  (offset :day 3)
  (offset :year 110)
  (offset :month -1))
@2131-05-28T02:00:00.000000+01:00
~~~

There is a destructive version, `adjust-timestamp!`.


### Compare timestamps (timestamp<, timestamp<, timestamp= …)

These should be self-explanatory.

~~~lisp
timestamp< time-a time-b
timestamp<= time-a time-b
timestamp> time-a time-b
timestamp>= time-a time-b
timestamp= time-a time-b
timestamp/= time-a time-b
~~~

### Find the minimum or maximum timestamp

Use `timestamp-minimum` and `timestamp-maximum`. They accept any number of arguments.

~~~lisp
(local-time:timestamp-minimum (local-time:today)
                              (local-time:timestamp- (local-time:today) 100 :year))
@1921-06-25T02:00:00.000000+01:00
~~~

If you have a list of timestamps, use `(apply #'timestamp-minimum <your list of timestamps>)`.

### Maximize or minimize a timestamp according to a time unit (timestamp-maximize-part, timestamp-minimize-part)

We can answer quite a number of questions with this handy function.

Here's an example: please give me the last day of this month:

~~~lisp
(let ((in-february (local-time:parse-timestring "1984-02-01")))
  (local-time:timestamp-maximize-part in-february :day))

@1984-02-29T23:59:59.999999+01:00
~~~


### Querying timestamp objects (get the day, the day of week, the days in month…)

Use:

~~~lisp
timestamp-[year, month, day, hour, minute, second, millisecond, microsecond,
           day-of-week (starts at 0 for sunday),
           millenium, century, decade]
~~~

Get all the values at once with `decode-timestamp`.

Bind a variable to a value of your choice with this convenient macro:

~~~lisp
(local-time:with-decoded-timestamp (:hour h)
     (now)
   (print h))

8
8
~~~

You can of course bind each time unit (`:sec :minute :day`) to its variable, in any order.

See also `(days-in-month <month> <year>)`.


### Formatting time strings (format, format-timestring, +iso-8601-format+)

local-time's date representation starts with `@`. We can `format` them as usual, with the aesthetic directive for instance, to get a usual date representation.

~~~lisp
(local-time:now)
@2019-11-13T18:07:57.425654+01:00
~~~

~~~lisp
(format nil "~a" (local-time:now))
"2019-11-13T18:08:23.312664+01:00"
~~~

We can use `format-timestring`, which can be used like `format` (thus it takes a stream as first argument):

~~~lisp
(local-time:format-timestring nil (local-time:now))
"2019-11-13T18:09:06.313650+01:00"
~~~

Here `nil` returns a new string. `t` would print to `*standard-output*`.

But `format-timestring` also accepts a `:format` argument. We can use predefined date formats as well as give our own in s-expression friendly way (see next section).

Its default value is
`+iso-8601-format+`, with the output shown above. The `+rfc3339-format+` format defaults to it.

With `+rfc-1123-format+`:

~~~lisp
(local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+)
"Wed, 13 Nov 2019 18:11:38 +0100"
~~~

With `+asctime-format+`:

~~~lisp
(local-time:format-timestring nil (local-time:now) :format local-time:+asctime-format+)
"Wed Nov 13 18:13:15 2019"
~~~

With `+iso-week-date-format+`:

~~~lisp
(local-time:format-timestring nil (local-time:now) :format local-time:+iso-week-date-format+)
"2019-W46-3"
~~~


Putting all this together, here is a function that returns Unix times as a human readable string:

~~~lisp
(defun unix-time-to-human-string (unix-time)
  (local-time:format-timestring
   nil
   (local-time:unix-to-timestamp unix-time)
   :format local-time:+asctime-format+))

(unix-time-to-human-string (get-universal-time))
"Mon Jun 25 06:46:49 2091"
~~~


### Defining format strings (format-timestring (:year "-" :month "-" :day))

We can pass a custom `:format` argument to `format-timestring`.

The syntax consists of a list made of symbols with special meanings
(`:year`, `:day`…), strings and characters:

~~~lisp
(local-time:format-timestring nil (local-time:now) :format '(:year "-" :month "-" :day))
"2019-11-13"
~~~

The list of symbols is available in the documentation: [https://common-lisp.net/project/local-time/manual.html#Parsing-and-Formatting](https://common-lisp.net/project/local-time/manual.html#Parsing-and-Formatting)

There are `:year :month :day :weekday :hour :min :sec :msec`, long and
short notations ("Monday", "Mo."), gmt offset, timezone markers and
more.

The `+rfc-1123-format+` itself is defined like this:

~~~lisp
(defparameter +rfc-1123-format+
  ;; Sun, 06 Nov 1994 08:49:37 GMT
  '(:short-weekday ", " (:day 2) #\space :short-month #\space (:year 4) #\space
    (:hour 2) #\: (:min 2) #\: (:sec 2) #\space :gmt-offset-hhmm)
  "See the RFC 1123 for the details about the possible values of the timezone field.")
~~~

We see the form `(:day 2)`: the 2 is for **padding**, to ensure that the
day is printed with two digits (not only `1`, but `01`). There could be
an optional third argument, the character with which to fill the
padding (by default, `#\0`).

### Parsing time strings

Use `parse-timestring` to parse timestrings, in the form
`2019-11-13T18:09:06.313650+01:00`. It works in a variety of formats
by default, and we can change parameters to adapt it to our needs.

To parse more formats such as "Thu Jul 23 19:42:23 2013" (asctime),
we'll use the [cl-date-time-parser](https://github.com/tkych/cl-date-time-parser) library.

The `parse-timestring` docstring is:

>  Parses a timestring and returns the corresponding timestamp. Parsing begins at start and stops at the end position. If there are invalid characters within timestring and fail-on-error is T, then an invalid-timestring error is signaled, otherwise NIL is returned.
>
> If there is no timezone specified in timestring then offset is used as the default timezone offset (in seconds).

Examples:

~~~lisp
(local-time:parse-timestring "2019-11-13T18:09:06.313650+01:00")
;; @2019-11-13T18:09:06.313650+01:00
~~~

~~~lisp
(local-time:parse-timestring "2019-11-13")
;; @2019-11-13T01:00:00.000000+01:00
~~~

This custom format fails by default: "2019/11/13", but we can set the
`:date-separator` to "/":

~~~lisp
(local-time:parse-timestring "2019/11/13" :date-separator #\/)
;; @2019-11-13T19:42:32.394092+01:00
~~~

There is also a `:time-separator` (defaulting to `#\:`) and
`:date-time-separator` (`#\T`).

Other options include:

- the start and end positions
- `fail-on-error` (defaults to `t`)
- `(allow-missing-elements t)`
- `(allow-missing-date-part allow-missing-elements)`
- `(allow-missing-time-part allow-missing-elements)`
- `(allow-missing-timezone-part allow-missing-elements)`
- `(offset 0)`

Now a format like ""Wed Nov 13 18:13:15 2019" will fail. We'll use the
`cl-date-time-parser` library:

~~~lisp
(cl-date-time-parser:parse-date-time "Wed Nov 13 18:13:15 2019")
;; 3782657595
;; 0
~~~

It returns the universal time which, in turn, we can ingest with the
local-time library:

~~~lisp
(local-time:universal-to-timestamp *)
;; @2019-11-13T19:13:15.000000+01:00
~~~

### Misc

To find out if it's Alice anniversary, use `timestamp-whole-year-difference time-a time-b`.
