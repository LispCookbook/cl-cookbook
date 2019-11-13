---
title: TCP/UDP programming with sockets
---

This is a short guide to TCP/IP and UDP/IP client/server programming in Common
Lisp using [usockets](https://github.com/usocket/usocket).


# TCP/IP

As usual, we will use quicklisp to load usocket.

    (ql:quickload "usocket")

Now we need to create a server. There are 2 primary functions that we need
to call. `usocket:socket-listen` and `usocket:socket-accept`.

`usocket:socket-listen` binds to a port and listens on it. It returns a socket
object. We need to wait with this object until we get a connection that we
accept. That's where `usocket:socket-accept` comes in. It's a blocking call
that returns only when a connection is made. This returns a new socket object
that is specific to that connection. We can then use that connection to
communicate with our client.

So, what were the problems I faced due to my mistakes?
Mistake 1 - My initial understanding was that `socket-accept` would return
a stream object. NO.... It returns a socket object. In hindsight, its correct
and my own mistake cost me time. So, if you want to write to the socket, you
need to actually get the corresponding stream from this new socket. The socket
object has a stream slot and we need to explicitly use that. And how does one
know that? `(describe connection)` is your friend!

Mistake 2 - You need to close both the new socket and the server socket.
Again this is pretty obvious but since my initial code was only closing
the connection, I kept running into a socket in use problem. Of course
one more option is to reuse the socket when we listen.

Once you get past these mistakes, it's pretty easy to do the rest. Close
the connections and the server socket and boom you are done!


~~~lisp
(defun create-server (port)
  (let* ((socket (usocket:socket-listen "127.0.0.1" port))
	 (connection (usocket:socket-accept socket :element-type 'character)))
    (unwind-protect
        (progn
	      (format (usocket:socket-stream connection) "Hello World~%")
	      (force-output (usocket:socket-stream connection)))
      (progn
	    (format t "Closing sockets~%")
	    (usocket:socket-close connection)
        (usocket:socket-close socket)))))
~~~

Now for the client. This part is easy. Just connect to the server port
and you should be able to read from the server. The only silly mistake I
made here was to use read and not read-line. So, I ended up seeing only a
"Hello" from the server. I went for a walk and came back to find the issue
and fix it.


~~~lisp
(defun create-client (port)
  (usocket:with-client-socket (socket stream "127.0.0.1" port :element-type 'character)
    (unwind-protect
         (progn
           (usocket:wait-for-input socket)
           (format t "Input is: ~a~%" (read-line stream)))
      (usocket:socket-close socket))))
~~~

So, how do you run this? You need two REPLs, one for the server
and one for the client. Load this file in both REPLs. Create the
server in the first REPL.

    (create-server 12321)

Now you are ready to run the client on the second REPL

    (create-client 12321)

Voila! You should see "Hello World" on the second REPL.


# UDP/IP

As a protocol, UDP is connection-less, and therefore there is no
concept of binding and accepting a connection. Instead we only do a
`socket-connect` but pass a specific set of parameters to make sure that
we create an UDP socket that's waiting for data on a particular port.

So, what were the problems I faced due to my mistakes?
Mistake 1 - Unlike TCP, you don't pass host and port to `socket-connect`.
If you do that, then you are indicating that you want to send a packet.
Instead, you pass `nil` but you set `:local-host` and `:local-port` to the address
and port that you want to receive data on. This part took some time to
figure out, because the documentation didn't cover it. Instead reading
a bit of code from
https://code.google.com/p/blackthorn-engine-3d/source/browse/src/examples/usocket/usocket.lisp helped a lot.

Also, since UDP is connectionless, anyone can send data to it at any
time. So, we need to know which host/port did we get data from so
that we can respond on it. So we bind multiple values to `socket-receive`
and use those values to send back data to our peer "client".

~~~lisp
(defun create-server (port buffer)
  (let* ((socket (usocket:socket-connect nil nil
					:protocol :datagram
					:element-type '(unsigned-byte 8)
					:local-host "127.0.0.1"
					:local-port port)))
    (unwind-protect
	 (multiple-value-bind (buffer size client receive-port)
	     (usocket:socket-receive socket buffer 8)
	   (format t "~A~%" buffer)
	   (usocket:socket-send socket (reverse buffer) size
				:port receive-port
				:host client))
      (usocket:socket-close socket))))
~~~


Now for the sender/receiver. This part is pretty easy. Create a socket,
send data on it and receive data back.

~~~lisp
(defun create-client (port buffer)
  (let ((socket (usocket:socket-connect "127.0.0.1" port
					 :protocol :datagram
					 :element-type '(unsigned-byte 8))))
    (unwind-protect
	 (progn
	   (format t "Sending data~%")
	   (replace buffer #(1 2 3 4 5 6 7 8))
	   (format t "Receiving data~%")
	   (usocket:socket-send socket buffer 8)
	   (usocket:socket-receive socket buffer 8)
	   (format t "~A~%" buffer))
      (usocket:socket-close socket))))
~~~


So, how do you run this? You need again two REPLs, one for the server
and one for the client. Load this file in both REPLs. Create the
server in the first REPL.

    (create-server 12321 (make-array 8 :element-type '(unsigned-byte 8)))

Now you are ready to run the client on the second REPL

    (create-client 12321 (make-array 8 :element-type '(unsigned-byte 8)))

Voila! You should see a vector `#(1 2 3 4 5 6 7 8)` on the first REPL
and `#(8 7 6 5 4 3 2 1)` on the second one.


# Credit

This guide originally comes from https://gist.github.com/shortsightedsid/71cf34282dfae0dd2528
