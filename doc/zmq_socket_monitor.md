% zmq_socket_monitor


Name
----

zmq_socket_monitor - monitor socket events


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_socket_monitor(socket, endpoint, events) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: socket
  CHARACTER(LEN = *), INTENT(IN) :: endpoint
  INTEGER(KIND = C_INT), INTENT(IN) :: events
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_socket_monitor()* method lets an application thread track socket
events (like connects) on a ØMQ socket.  Each call to this method creates a
*ZMQ_PAIR* socket and binds that to the specified `inproc://` _endpoint_.  To
collect the socket events, you must create your own *ZMQ_PAIR* socket and
connect that to the endpoint.

The _events_ argument is a bitmask of the socket events you wish to monitor,
see _Supported events_ below.  To monitor all events, use the event value
*ZMQ_EVENT_ALL*.

Each event is sent as two frames.  The first frame contains an event number
(16 bits), and an event value (32 bits) that provides additional data according
to the event number.  The second frame contains a string that specifies the
affected TCP or IPC endpoint.

The *zmq_socket_monitor()* method supports only connection-oriented transports,
that is, TCP, IPC, and TIPC.


Supported events
----------------

### ZMQ_EVENT_CONNECTED

The socket has successfully connected to a remote peer. The event value is the
file descriptor (FD) of the underlying network socket.  Warning: there is no
guarantee that the FD is still valid by the time your code receives this event.

### ZMQ_EVENT_CONNECT_DELAYED

A connect request on the socket is pending.  The event value is unspecified.

### ZMQ_EVENT_CONNECT_RETRIED

A connect request failed, and is now being retried.  The event value is the
reconnect interval in milliseconds.  Note that the reconnect interval is
recalculated at each retry.

### ZMQ_EVENT_LISTENING

The socket was successfully bound to a network interface.  The event value is
the FD of the underlying network socket. Warning: there is no guarantee that
the FD is still valid by the time your code receives this event.

### ZMQ_EVENT_BIND_FAILED

The socket could not bind to a given interface.  The event value is the
_errno_ generated by the system bind call.

### ZMQ_EVENT_ACCEPTED

The socket has accepted a connection from a remote peer.  The event value is
the FD of the underlying network socket.  Warning: there is no guarantee that
the FD is still valid by the time your code receives this event.

### ZMQ_EVENT_ACCEPT_FAILED

The socket has rejected a connection from a remote peer.  The event value is
the errno generated by the accept call.

### ZMQ_EVENT_CLOSED

The socket was closed.  The event value is the FD of the (now closed) network
socket.

### ZMQ_EVENT_CLOSE_FAILED

The socket close failed.  The event value is the _errno_ returned by the system
call.  Note that this event occurs only on IPC transports.

### ZMQ_EVENT_DISCONNECTED

The socket was disconnected unexpectedly.  The event value is the FD of the
underlying network socket.  Warning: this socket will be closed.

### ZMQ_EVENT_MONITOR_STOPPED

Monitoring on this socket ended.


Return value
------------

The *zmq_socket_monitor()* function returns a value of 0 or greater if
successful.  Otherwise it returns `-1` and sets _errno_ to one of the values
defined below.


Errors
------

*ETERM*
  ~ The ØMQ _context_ associated with the specified _socket_ was terminated.

*EPROTONOSUPPORT*
  ~ The requested _transport_ protocol is not supported. Monitor sockets are
    required to use the `inproc://` transport.

*EINVAL*
  ~ The endpoint supplied is invalid.


Example
-------

### Monitoring a server socket for connections

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
TYPE(C_PTR) :: monitor
TYPE(ZMQ_POLLITEM_T), DIMENSION(2) :: items
INTEGER(KIND = C_INT) :: rc
INTEGER(KIND = C_INT) :: nbytes
CHARACTER(KIND = C_CHAR, LEN = 256) :: buffer

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_REP)
monitor = zmq_socket(context, ZMQ_PAIR)

rc = zmq_bind(socket, 'tcp://*:5555')
rc = zmq_connect(monitor, 'inproc://monitor')
rc = zmq_socket_monitor(socket, 'inproc://monitor', ZMQ_EVENT_ACCEPTED)

items(1)%socket = socket
items(2)%socket = monitor
items(1)%events = ZMQ_POLLIN
items(2)%events = ZMQ_POLLIN

DO WHILE (zmq_poll(items, 2, -1_C_LONG) > 0)
  IF (IAND(items(1)%revents, ZMQ_POLLIN) /= 0) THEN
    ! Echo messages back to the client
    nbytes = zmq_recv(socket, buffer, 0)
    nbytes = zmq_send(socket, buffer(1:nbytes), 0)
  ELSE IF (IAND(items(2)%revents, ZMQ_POLLIN) /= 0) THEN
    ! Got a connection
    nbytes = zmq_recv(monitor, buffer, 0)
    nbytes = zmq_recv(monitor, buffer, 0)
    PRINT*, 'Connection from client'
  END IF
END DO

rc = zmq_close(socket)
rc = zmq_close(monitor)
rc = zmq_ctx_term(context)
~~~


See also
--------

[fzmq][]
