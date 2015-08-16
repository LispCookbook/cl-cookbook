---
title: Sockets
---

ANSI Common Lisp does not provide standard functions to operate on sockets, but
all major CL implementations have added socket support to their
environments. Although all socket packages offer roughly the same
functionalities, they use slightly different names for functions, making it hard
to provide a standardized description. In this chapter, we will use the socket
library contained in [PORT](http://clocc.sourceforge.net/dist/port.html), that
in turn is part of the
[Common Lisp Open Code Collection](http://clocc.sourceforge.net/) (CLOCC). The
socket functions provided by PORT currently work without modifications on a wide
range of Common Lisp implementations (Allegro, Lispworks, CLISP, CMU CL, SBCL,
GCL). Therefore, this section does not contain pointers to the Common Lisp
HyperSpec. We refer you to the documentation of the PORT package for more
information. Even if you do not use port, the concepts described here will most
likely apply to your Lisp implementation, and it should not be hard to find out
what the equivalent functions are in it.

<a name="intro"></a>

### Introduction

First of all, some theory. If you are interested in using sockets you probably
already know this stuff, but it is useful to go through it anyway to establish
our terminology. A **socket** is a communication channel between two processes,
usually (but not necessarily) running on different computers. Several types of
sockets exist, but in the following we are only going to describe **TCP**
sockets, that implement a reliable, two-way communication stream. A socket
connection originates from a **port** on a **client** machine, and reaches
another port on a **server** machine. Therefore, a connected socket is
identified by four data elements: the address of the client computer, the port
on the client computer, the address of the server computer and the port on the
server computer. This particular way of using sockets is referred to as the
"Internet" address family. Under Unix, there is also an alternative address
family ("Unix") that uses local files as sockets. In the following we will
describe how to implement both TCP clients and servers in Common Lisp. Of
course, nothing prevents you from using a CL client to connect to a server
written in a different language, and vice-versa.

Addresses can be represented in three different ways. The one you are probably
most familiar with is the human-readable form, such as <tt>www.lisp.org</tt>. We
will call this a **hostname**. Each hostname may be associated with one or more
**IP addresses**, each represented by four bytes. These are usually written in
**dotted** notation: <tt>128.18.65.4</tt>. The translation between a hostname
and a dotted IP address is called a **DNS lookup**, and normally occurs behind
the scenes in modern systems. Finally, an address can be easily converted from
dotted notation to an **ipaddr**, which is simply the long integer formed by the
four bytes in the dotted address. The ipaddr corresponding to the above example
is 2148679940\. To summarize: humans use hostnames that are easier to memorize
and type; software uses IP addresses that are stored as ipaddrs but are usually
printed in dotted notation.

Ports are represented as integer numbers in the range 1-65535\. In general,
ports can be chosen arbitrarily (except that some ports might only be available
to privileged users on Unix and similar operating systems), but two processes
that need to communicate through sockets should have a way of knowing which port
to use. Let's look at how sockets are normally used, in order to make this point
clear.

1.  A process on computer A (the server) **opens** TCP port X and prepares to
    **accept** connections.
2.  A process on computer B (the client) **creates** a socket connecting port Y
    on B to port X on A. (Note that B needs to know that A opened port X, but A
    does not need to know that the socket originates from port Y on B. Y is
    usually chosen randomly by the TCP implementation.)
3.  The server process accepts the socket connection, and spawns a thread or a
    sub-process to **handle** it.
4.  A and B can freely exchange data over the socket, until either the client or
    the server closes it.

<a name="adr"></a>

### Addresses

A prerequisite for working with sockets is the ability to convert hostnames to
IP addresses and vice versa. The function RESOLVE-HOST-IPADDR takes a hostname
as its argument and returns a HOSTENT structure. For example:

~~~lisp
> (resolve-host-ipaddr "www.lisp.org")
#S(HOSTENT :NAME "bibop.alu.org" :ALIASES NIL :ADDR-LIST ("128.18.65.4") :ADDR-TYPE 2)
~~~

This structure contains four fields, but the only ones we are interested in are
NAME and ADDR-LIST. NAME contains the _canonical_ hostname of the machine (that
might or might not be the same as the argument), while ADDR-LIST contains the IP
address corresponding to the given hostname (in some cases a hostname is mapped
to more than one IP address for load-balancing purposes, but not all
implementations return the additional addresses). You can also use this function
to perform the reverse mapping:

~~~lisp
> (resolve-host-ipaddr "128.18.65.4")
#S(HOSTENT :NAME "bibop.alu.org" :ALIASES NIL :ADDR-LIST ("128.18.65.4") :ADDR-TYPE 2)
~~~

The functions DOTTED-TO-IPADDR and IPADDR-TO-DOTTED are used to convert from
dotted notation to ipaddrs:

~~~lisp
> (dotted-to-ipaddr "128.18.65.4")
2148679940
> (ipaddr-to-dotted 2148679940)
"128.18.65.4"
~~~

<a name="server"></a>

### Server sockets

As we saw earlier, the first step in setting up a socket consists in the server
process _opening a port_. This is accomplished with the OPEN-SOCKET-SERVER
function, that takes the port number (an integer) as its argument and returns an
object representing the open socket port. There are two ways that things could
go wrong at this stage: either your process does not have sufficient privileges
to open the desired port (for example, you are trying to open a port below 1024
without being root under unix), or the port you have chosen has already been
opened by another process. In both cases, Lisp will signal an error.

Assuming you have successfully opened the server port, your process should now
start waiting for incoming connections. The function SOCKET-ACCEPT takes the
"open port" structure returned by OPEN-SOCKET-SERVER as input, and waits until a
connection attempt is received on the port. The keyword argument :WAIT controls
how the waiting happens exactly: if it is specified and is a positive number,
the function will wait for a connection for the specified number of seconds at
most. If a connection is not received before timeout, the function returns
NIL. If :WAIT is not specified, the function waits forever. When a connection is
received, the function returns a "socket-stream" object, and the server process
turns to handling the connection.

If the Lisp environment you are using supports multiple processes or threads,
you can take advantage of this feature to avoid tying up the server when
handling a connection. The problem is that while it is executing code to handle
a connection, the server cannot call SOCKET-ACCEPT, and therefore other clients
trying to connect will not be served. Using multiprocessing, you can prevent
this from happening by spawning a new process to handle each request, so that
the main process is always available to accept new connections. For example:

~~~lisp
;; Open the server socket
(let ((server (open-socket-server 4141)))
  (loop

    ;; Listen for incoming connections
    (let ((socket (socket-accept server)))

      ;; Spawn a process to handle the connection
      (make-process "Connection handler"
                    #'handle-connection
                    socket))

    ;; The main process is now free to accept a new connection
    ))
~~~

We assumed the existence of a function called MAKE-PROCESS that creates a new
Lisp thread to run a function call. In this case the thread runs a user-defined
function called HANDLE-CONNECTION that takes the socket stream as its only
argument and operates on it. After handing the connection to the subprocess, the
main process immediately returns to the SOCKET-ACCEPT call to wait for another
incoming connection. This basic structure could be extended to limit the number
of concurrent processes, to perform load balancing, request logging, etc.<a
name="client"></a>

### Client sockets

Let's now look at the other side of the story. On the client machine, the Lisp
process should call OPEN-SOCKET, specifying both the host to connect to and the
port number. The host can be specified in any of the formats we saw above, while
the port should be an integer. Several scenarios are possible now:

*   The destination port is open and the server process is listening. In this
    case the connection is established, and OPEN-SOCKET returns a
    "socket-stream" structure.
*   The destination port was opened but the server process did not call
    SOCKET-ACCEPT. In this case the connection will be established and put in a
    backlog queue by the operating system (provided there is still room in the
    queue), until the server process gets around to calling
    SOCKET-ACCEPT. Things then proceed as in the previous case.
*   The specified port was not opened on the remote machine. In this case
    OPEN-SOCKET returns immediately with a "connection refused" error.

Note that the socket-stream structures returned to the server and client
processes when a connection is successfully established are simmetrical: each of
them contains two pairs (IP address, port number) for both the local and the
remote end of the socket. The roles of the two pairs will be reversed in the two
structures, that is:

~~~lisp
#<SOCKET-STREAM [A:X] to [B:Y]> (on A)
#<SOCKET-STREAM [B:Y] to [A:X]> (on B)
~~~

The function SOCKET-HOST/PORT returns the four pieces of information in a
socket-stream structure as multiple values.

<a name="comm"></a>

### Communication

Once both the client and the server are in possession of an open socket stream,
they can start communicating. Since socket streams are a type of streams, the
two agents can communicate by writing to them (e.g., using FORMAT) and reading
from them (e.g. using READ-LINE or READ). This is, for example, how the client
would send a string to the server:

~~~lisp
> (format client-stream "Username: user1~%")
> (force-output client-stream)
~~~

And the server would do the following to read the message:

~~~lisp
> (read-line server-stream)
"Username: user1"
~~~

Note that since socket streams are usually buffered, the data is actually sent
over the network only when the buffer is full, when the stream is closed or when
you call FORCE-OUTPUT on the stream. If you need to be sure that the data is
sent out (for example, because you are then expecting an answer), it is a good
idea to call FORCE-OUTPUT after all output operations. Note also that
communication can take place in both directions at once. Finally, what we said
so far applies to text data. In some implementations you can send binary data
over a socket using the same techniques, in other implementations you have to
specify whether the socket is going to be used for text or binary data.<a
name="close"></a>

### Closing

The socket can be closed by either party, by calling the regular CL function
CLOSE on the sockets. The server can close the server socket with the function
SOCKET-SERVER-CLOSE.

<a name="example"></a>

### A complete example

In this section we will implement a very simple HTTP/0.9 server and
client. HTTP/0.9 is a primitive version of the HTTP protocol currently used by
all Web servers and clients. It is a very simple query-response protocol whose
only purpose is to allow a client to retrieve a document from a server. The
client sends a request line with the following format:

~~~lisp
GET /pathname/to/file HTTP/0.9
~~~

followed by a blank line, and the server replies with the contents of the
specified file. Here is the code:

~~~lisp
--------------------------cut here------------------------------------
(in-package :port)

;;; Utilities

(defun http-send-line (stream line)
  "Send a line of text to the HTTP stream, terminating it with CR+LF."
  (princ line stream)
  (princ (code-char 13) stream)  ;; carriage-return
  (princ (code-char 10) stream)) ;; linefeed
;;; Server

(defun http-0.9-server (port root)
  "Run an HTTP/0.9 server on `port'. `root' is the pathname to the
directory where the HTML pages are stored."
  (let ((server (open-socket-server port)))
    (format t "> Started server on port ~d~%" port)
    (unwind-protect
        (loop
          (let ((socket (socket-accept server)))
            (unwind-protect
                (process-request socket
                                 (read-request socket)
                                 root)
              ;; Close connection when done
              (close socket))))

      ;; Close server before exiting
      (socket-server-close server))))

(defun read-request (socket)
  "Read an HTTP/0.9 request from `socket' and determine the
corresponding filename. An HTTP/0.9 request has the form:

GET /filename HTTP/0.9

Returns the filename, or NIL if the request is incorrect."

  (let ((request (read-line socket nil nil)))
    (when request
      (let ((p1 (position #\Space request))
            (p2 (position #\Space request :from-end t)))
        (when (and p1 p2)
          (subseq request (1+ p1) p2))))))

(defun process-request (socket filename root)
  (format t "> Received request from host ~a~%"
          (socket-host/port socket))
  ;; discard empty line
  (read-line socket)
  (if filename
      ;; Correct request, serve file
      (serve-file socket
                  (concatenate 'string root filename))
    ;; Incorrect request, return error
    (http-send-line socket "HTTP/0.9 400 Invalid HTTP Request."))

  ;; Make sure the client sees the output - not really
  ;; necessary since we close the socket right after this.
  (force-output socket))

(defun serve-file (socket pathname)
  "Write the contents of the file `pathname' to `socket'."
  ;; Does file exist?
  (if (probe-file pathname)

      ;; Yes, write it out to the socket
      (with-open-file (in pathname)
        (format t "> Serving file ~a...~%" pathname)
        (loop
          (let ((line (read-line in nil nil)))
            (unless line (return))
            (http-send-line socket line))))

    ;; No, return error
    (format socket "HTTP/0.9 401 Not found.~%")))

;;; Client

(defun http-get (server port path)
  "Send a request for file `path' to an HTTP/0.9 server on host
`server', port number `port'. Print the contents of the returned file
to standard output."
  ;; Open connection
  (let ((socket (open-socket server port)))
    (unwind-protect
        (progn
          (format t "> Sending request to ~a:~a...~%" server port)
          ;; Send request
          (http-send-line socket (format nil "GET ~a HTTP/0.9~%~%" path))
          (force-output socket)

          ;; Read response and output it
          (format t "> Received response:~%")
          (loop
            (let ((line (read-line socket nil nil)))
              (unless line (return))
              (format t "~a~%" line))))

      ;; Close socket before exiting.
      (close socket))))
--------------------------cut here------------------------------------
~~~

To run this example you should open two Lisp listeners, and load the PORT
package followed by the above code into both of them. On one of them (the
server) call:

~~~lisp
(http-0.9-server 8080 "/etc/")
~~~

(You can select any directory as the root, and exposing the contents of /etc is
usually not a good idea, but of course this is just an example). You will not
see anything happening, meaning that the server is idle waiting for
connection. Then in the second listener (the client) call:

~~~lisp
(http-get "localhost" 8080 "/hosts")
~~~

and you should see the contents of your /etc/hosts file printed to standard
output. In this case we used "localhost" as the server's hostname because we are
running both the client and the server on the same machine for simplicity, but
of course they could be on two different machines. Note that the server runs
forever, you will have to interrupt it manually after you have finished trying
it.
