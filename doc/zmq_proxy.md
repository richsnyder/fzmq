% zmq_proxy

Name
----

zmq_proxy - start built-in ØMQ proxy


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_proxy(frontend, backend, capture) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: frontend
  TYPE(C_PTR), INTENT(IN) :: backend
  TYPE(C_PTR), INTENT(IN) :: capture
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_proxy()* function starts the built-in ØMQ proxy in the current
application thread.

The proxy connects a frontend socket to a backend socket. Conceptually, data
flows from frontend to backend. Depending on the socket types, replies may flow
in the opposite direction. The direction is conceptual only; the proxy is fully
symmetric and there is no technical difference between frontend and backend.

Before calling *zmq_proxy()* you must set any socket options, and connect or
bind both frontend and backend sockets.

*zmq_proxy()* runs in the current thread and returns only if/when the current
context is closed.

If the capture socket is not _C_NULL_PTR_, the proxy shall send all messages,
received on both frontend and backend, to the capture socket. The capture
socket should be a _ZMQ_PUB_, _ZMQ_DEALER_, _ZMQ_PUSH_, or _ZMQ_PAIR_ socket.

Refer to [zmq_socket][] for a description of the available socket types.


Example Usage
-------------

### Shared Queue

When the frontend is a _ZMQ_ROUTER_ socket, and the backend is a _ZMQ_DEALER_
socket, the proxy shall act as a shared queue that collects requests from a
set of clients, and distributes these fairly among a set of services.
Requests shall be fair-queued from frontend connections and distributed evenly
across backend connections. Replies shall automatically return to the client
that made the original request.

### Forwarder

When the frontend is a _ZMQ_XSUB_ socket, and the backend is a _ZMQ_XPUB_
socket, the proxy shall act as a message forwarder that collects messages from
a set of publishers and forwards these to a set of subscribers. This may be
used to bridge networks transports, e.g. read on tcp:// and forward on pgm://.

### Streamer

When the frontend is a _ZMQ_PULL_ socket, and the backend is a _ZMQ_PUSH_
socket, the proxy shall collect tasks from a set of clients and forwards these
to a set of workers using the pipeline pattern.


Return value
------------

The *zmq_proxy()* function always returns `-1` and _errno_ set to ETERM (the
ØMQ _context_ associated with either of the specified sockets was terminated).


Example
-------

### Creating a shared queue proxy

~~~{.example}
frontend = zmq_socket(context, ZMQ_ROUTER)
backend = zmq_socket(context, ZMQ_DEALER)
rc = zmq_bind(frontend, 'tcp://*:5555')
rc = zmq_bind(backend, 'tcp://*:5556')
rc = zmq_proxy(frontent, backend, C_NULL_PTR)
~~~


See also
--------

[zmq_bind][]
[zmq_connect][]
[zmq_socket][]
