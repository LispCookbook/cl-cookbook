---
title: WebSockets
---

The Common Lisp ecosystem boasts a few approaches to building WebSocket servers.
First, there is the excellent
[Hunchensocket](https://github.com/joaotavora/hunchensocket) that is written as
an extension to [Hunchentoot](https://edicl.github.io/hunchentoot/), the classic
web server for Common Lisp.  I have used both and I find them to be wonderful.

Today, however, you will be using the equally excellent
[websocket-driver](https://github.com/fukamachi/websocket-driver) to build a WebSocket server with
[Clack](https://github.com/fukamachi/clack). The Common Lisp web development community has expressed a
slight preference for the Clack ecosystem because Clack provides a uniform interface to
a variety of backends, including Hunchentoot. That is, with Clack, you can pick and choose the
backend you prefer.

In what follows, you will build a simple chat server and connect to it from a
web browser. The tutorial is written so that you can enter the code into your
REPL as you go, but in case you miss something, the full code listing can be found at the end.

As a first step, you should load the needed libraries via quicklisp:

~~~lisp

(ql:quickload '(clack websocket-driver alexandria))

~~~


## The websocket-driver Concept

In websocket-driver, a WebSocket connection is an instance of the `ws` class,
which exposes an event-driven API. You register event handlers by passing your
WebSocket instance as the second argument to a method called `on`. For example,
calling `(on :message my-websocket #'some-message-handler)` would invoke
`some-message-handler` whenever a new message arrives.

The `websocket-driver` API provides handlers for the following events:

- `:open`: When a connection is opened. Expects a handler with zero arguments.
- `:message` When a message arrives. Expects a handler with one argument, the message received.
- `:close` When a connection closes. Expects a handler with two keyword args, a
  "code" and a "reason" for the dropped connection.
- `:error` When some kind of protocol level error occurs. Expects a handler with
  one argument, the error message.

For the purposes of your chat server, you will want to handle three cases: when
a new user arrives to the channel, when a user sends a message to the channel,
and when a user leaves.

## Defining Handlers for Chat Server Logic

In this section you will define the functions that your event handlers will
eventually call. These are helper functions that manage the chat server logic.
You will define the WebSocket server in the next section.

First, when a user connects to the server, you need to give that user a nickname
so that other users know whose chats belong to whom. You will also need a data
structure to map individual WebSocket connections to nicknames:

~~~lisp

;; make a hash table to map connections to nicknames
(defvar *connections* (make-hash-table))

;; and assign a random nickname to a user upon connection
(defun handle-new-connection (con)
  (setf (gethash con *connections*)
        (format nil "user-~a" (random 100000))))

~~~

Next, when a user sends a chat to the room, the rest of the room should be
notified. The message that the server receives is prepended with the nickname of
the user who sent it.

~~~lisp

(defun broadcast-to-room (connection message)
  (let ((message (format nil "~a: ~a"
                         (gethash connection *connections*)
                         message)))
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))
~~~

Finally, when a user leaves the channel, by closing the browser tab or
navigating away, the room should be notified of that change, and the user's
connection should be dropped from the `*connections*` table.

~~~lisp
(defun handle-close-connection (connection)
  (let ((message (format nil " .... ~a has left."
                         (gethash connection *connections*))))
    (remhash connection *connections*)
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))
~~~

## Defining A Server

Using Clack, a server is started by passing a function to `clack:clackup`. You
will define a function called `chat-server` that you will start by
calling `(clack:clackup #'chat-server :port 12345)`.

A Clack server function accepts a single plist as its argument. That plist
contains environment information about a request and is provided by the system.
Your chat server will not make use of that environment, but if you want to learn
more you can check out Clack's documentation.

When a browser connects to your server, a websocket will be instantiated and
handlers will be defined on it for each of the the events you want to support.
A WebSocket "handshake" will then be sent back to the browser, indicating
that the connection has been made. Here's how it works:

~~~lisp
(defun chat-server (env)
  (let ((ws (websocket-driver:make-server env)))

    (websocket-driver:on :open ws
                         (lambda () (handle-new-connection ws)))

    (websocket-driver:on :message ws
                         (lambda (msg) (broadcast-to-room ws msg)))

    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (handle-close-connection ws)))

    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws)))) ; send the handshake

~~~

You may now start your server, running on port `12345`:

~~~lisp
;; keep the handler around so that you can stop your server later on

(defvar *chat-handler* (clack:clackup #'chat-server :port 12345))
~~~


## A Quick HTML Chat Client

So now you need a way to talk to your server. Using Clack, define a simple
application that serves a web page to display and send chats.  First the web page:

~~~lisp

(defvar *html*
  "<!doctype html>

<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <title>LISP-CHAT</title>
</head>

<body>
    <ul id=\"chat-echo-area\">
    </ul>
    <div style=\"position:fixed; bottom:0;\">
        <input id=\"chat-input\" placeholder=\"say something\" >
    </div>
    <script>
     window.onload = function () {
         const inputField = document.getElementById(\"chat-input\");

         function receivedMessage(msg) {
             let li = document.createElement(\"li\");
             li.textContent = msg.data;
             document.getElementById(\"chat-echo-area\").appendChild(li);
         }

         const ws = new WebSocket(\"ws://localhost:12345/chat\");
         ws.addEventListener('message', receivedMessage);

         inputField.addEventListener(\"keyup\", (evt) => {
             if (evt.key === \"Enter\") {
                 ws.send(evt.target.value);
                 evt.target.value = \"\";
             }
         });
     };

    </script>
</body>
</html>
")


(defun client-server (env)
    (declare (ignore env))
    `(200 (:content-type "text/html")
          (,*html*)))

~~~

You might prefer to put the HTML into a file, as escaping quotes is kind of annoying.
Keeping the page data in a `defvar` was simpler for the purposes of this
tutorial.

You can see that the `client-server` function just serves the HTML content. Go
ahead and start it, this time on port `8080`:

~~~lisp
(defvar *client-handler* (clack:clackup #'client-server :port 8080))
~~~

## Check it out!

Now open up two browser tabs and point them to `http://localhost:8080` and you
should see your chat app!

<img src="https://raw.githubusercontent.com/thegoofist/resource-dump/master/lisp-chat.gif"
     width="470" height="247"/>

## All The Code

~~~lisp
(ql:quickload '(clack websocket-driver alexandria))

(defvar *connections* (make-hash-table))

(defun handle-new-connection (con)
  (setf (gethash con *connections*)
        (format nil "user-~a" (random 100000))))

(defun broadcast-to-room (connection message)
  (let ((message (format nil "~a: ~a"
                         (gethash connection *connections*)
                         message)))
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defun handle-close-connection (connection)
  (let ((message (format nil " .... ~a has left."
                         (gethash connection *connections*))))
    (remhash connection *connections*)
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defun chat-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda () (handle-new-connection ws)))

    (websocket-driver:on :message ws
                         (lambda (msg) (broadcast-to-room ws msg)))

    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws))))

(defvar *html*
  "<!doctype html>

<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <title>LISP-CHAT</title>
</head>

<body>
    <ul id=\"chat-echo-area\">
    </ul>
    <div style=\"position:fixed; bottom:0;\">
        <input id=\"chat-input\" placeholder=\"say something\" >
    </div>
    <script>
     window.onload = function () {
         const inputField = document.getElementById(\"chat-input\");

         function receivedMessage(msg) {
             let li = document.createElement(\"li\");
             li.textContent = msg.data;
             document.getElementById(\"chat-echo-area\").appendChild(li);
         }

         const ws = new WebSocket(\"ws://localhost:12345/\");
         ws.addEventListener('message', receivedMessage);

         inputField.addEventListener(\"keyup\", (evt) => {
             if (evt.key === \"Enter\") {
                 ws.send(evt.target.value);
                 evt.target.value = \"\";
             }
         });
     };

    </script>
</body>
</html>
")

(defun client-server (env)
  (declare (ignore env))
  `(200 (:content-type "text/html")
     (,*html*)))

(defvar *chat-handler* (clack:clackup #'chat-server :port 12345))
(defvar *client-handler* (clack:clackup #'client-server :port 8080))
~~~
