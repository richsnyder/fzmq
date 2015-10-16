% zmq_proxy

Name
----

zmq_proxy - start a built-in ØMQ proxy


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

The proxy connects a frontend socket to a backend socket.  Conceptually, data
flows from frontend to backend.  Depending on the socket types, replies may
flow in the opposite direction.  The direction is conceptual only; the proxy is
fully symmetric and there is no technical difference between frontend and
backend.

Before calling *zmq_proxy()* you must set any socket options, and connect or
bind both frontend and backend sockets.

*zmq_proxy()* runs in the current thread and returns only if/when the current
context is closed.

If the capture socket is not *C_NULL_PTR*, the proxy shall send all messages
received on both frontend and backend to the capture socket.  The capture
socket should be a *ZMQ_PUB*, *ZMQ_DEALER*, *ZMQ_PUSH*, or *ZMQ_PAIR* socket.

Refer to [zmq_socket][] for a description of the available socket types.


Example Usage
-------------

### Shared Queue

When the frontend is a *ZMQ_ROUTER* socket and the backend is a *ZMQ_DEALER*
socket, the proxy shall act as a shared queue that collects requests from a
set of clients, and distributes these fairly among a set of services.
Requests shall be fair-queued from frontend connections and distributed evenly
across backend connections.  Replies shall automatically return to the client
that made the original request.

### Forwarder

When the frontend is a *ZMQ_XSUB* socket and the backend is a *ZMQ_XPUB*
socket, the proxy shall act as a message forwarder that collects messages from
a set of publishers and forwards these to a set of subscribers.  This may be
used to bridge networks transports, e.g. read on tcp:// and forward on pgm://.

### Streamer

When the frontend is a *ZMQ_PULL* socket, and the backend is a *ZMQ_PUSH*
socket, the proxy shall collect tasks from a set of clients and forwards these
to a set of workers using the pipeline pattern.


Return value
------------

The *zmq_proxy()* function always returns `-1` and sets _errno_ to *ETERM*,
indicating that the ØMQ _context_ associated with one of the specified sockets
was terminated.


Example
-------

### Creating a shared queue proxy

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: frontend
TYPE(C_PTR) :: backend
INTEGER(KIND = C_INT) :: rc

context = zmq_ctx_new()
frontend = zmq_socket(context, ZMQ_ROUTER)
backend = zmq_socket(context, ZMQ_DEALER)
rc = zmq_bind(frontend, 'tcp://*:5555')
rc = zmq_bind(backend, 'tcp://*:5556')

rc = zmq_proxy(frontent, backend, C_NULL_PTR)

rc = zmq_close(frontend)
rc = zmq_close(backend)
rc = zmq_ctx_term(context)
~~~


See also
--------

[zmq_bind][]
[zmq_connect][]
[zmq_socket][]
[fzmq][]
