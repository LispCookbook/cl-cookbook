---
title: TCP/UDP programming with sockets
---

This is a short guide to TCP/IP and UDP/IP client/server programming in Common
Lisp using [usockets](https://github.com/usocket/usocket).


## How do I create a TCP/IP server and let a client send data to it?

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

If you want to write to the socket, you need to get the corresponding 
stream from this new socket. The socket object has a stream slot and we need 
to explicitly use that with `usocket:socket-stream`.

After using the connection and the socket both need to be closed with `usocket:socket-close`. 
This will be done automatically if we use the macro `usocket:with-socket-listener` 
instead of `usocket:socket-listen`.

~~~lisp
(defun create-server (port)
  (usocket:with-socket-listener (socket "127.0.0.1" port)
    (let ((connection (usocket:socket-accept socket :element-type 'character)))
      (format (usocket:socket-stream connection) "Hello World!~%")
      (force-output (usocket:socket-stream connection)))))
~~~


Now for the client. Just connect to the server port with 
`usocket:with-client-socket` and you should be able to read from the server. 
This can be done with `read-line`.

~~~lisp
(defun create-client (port)
  (usocket:with-client-socket (socket stream "127.0.0.1" port :element-type 'character)
    (usocket:wait-for-input socket)
    (format t "Input is: ~a~%" (read-line stream))))
~~~


So, how do you run this? You need two REPLs, one for the server
and one for the client. Evaluate the function definition for `create-server` and create 
the server in the first REPL.

    (create-server 12321)

Now you are ready to evaluate the function definition for `create-client` and to run 
the client on the second REPL

    (create-client 12321)

Voilà! You should see `"Input is: Hello World!"` on the second REPL.


## How do I create a UDP/IP server and let a client send data to it?

As a protocol, UDP is connection-less, and therefore there is no
concept of binding and accepting a connection. So we would only use
`usocket:socket-connect` to make sure that we create an UDP socket specified with
`:protocol :datagram` that's waiting for data on a address specified with 
`:local-host` and port specified with `:local-port`.

Since we don't want to bother closing the socket we use the macro 
`usocket:with-client-socket` which takes the same arguments instead.
We don't need the stream variable here, so we bind it to NIL.

Also, since UDP is connectionless, anyone can send data to it at any
time. So, we need to know which host/port did we get data from so
that we can respond on it. So we bind multiple values to `usocket:socket-receive`
and use those values to send back data with `usocket:socket-send` to our peer "client".

~~~lisp
(defun create-server (port buffer)
  (usocket:with-client-socket (socket nil nil nil 
                                      :protocol :datagram
                                      :element-type '(unsigned-byte 8)
					                  :local-host "127.0.0.1"
					                  :local-port port)
    (multiple-value-bind (buffer size client receive-port)
	    (usocket:socket-receive socket buffer 8)
	  (format t "~A~%" buffer)
	  (usocket:socket-send socket (reverse buffer) size
				           :port receive-port
				           :host client))))
~~~


Now for the sender/receiver. This part is pretty easy. Create a socket,
send data on it and receive data back.

~~~lisp
(defun create-client (port buffer)
  (usocket:with-client-socket (socket nil "127.0.0.1" port 
                                      :protocol :datagram
                                      :element-type '(unsigned-byte 8))
    (format t "Sending data~%")
    (replace buffer #(1 2 3 4 5 6 7 8))
    (format t "Receiving data~%")
    (usocket:socket-send socket buffer 8)
    (usocket:socket-receive socket buffer 8)
    (format t "~A~%" buffer)))
~~~


So, how do you run this? You need again two REPLs, one for the server
and one for the client. Load this file in both REPLs. Create the
server in the first REPL.

    (create-server 12321 (make-array 8 :element-type '(unsigned-byte 8)))

Now you are ready to run the client on the second REPL

    (create-client 12321 (make-array 8 :element-type '(unsigned-byte 8)))

Voilà! You should see a vector `#(1 2 3 4 5 6 7 8)` on the first REPL
and the output
 
  Sending data
  Receiving data
  #(8 7 6 5 4 3 2 1)
  
on the second one.


## Credit

This guide originally comes from [shortsightedsid](https://gist.github.com/shortsightedsid/71cf34282dfae0dd2528) and was edited.
