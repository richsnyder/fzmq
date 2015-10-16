% zmq_close


Name
----

zmq_close - close a ØMQ socket


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_close(socket) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: socket
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_close()* function shall destroy the socket referenced by the _socket_
argument.  Any outstanding messages physically received from the network but
not yet received by the application with *zmq_recv()* or *zmq_msg_recv()* shall
be discarded.  The behavior for discarding messages sent by the application
with *zmq_send()* and *zmq_msg_send()* but not yet physically transferred to
the network depends on the value of the *ZMQ_LINGER* socket option for the
specified _socket_.

The default setting of *ZMQ_LINGER* does not discard unsent messages; this
behaviour may cause the application to block when calling *zmq_ctx_term()*.  
For details refer to [zmq_setsockopt][] and [zmq_ctx_term][].


Return value
------------

The *zmq_close()* function shall return zero if successful.  Otherwise it shall
return `-1` and set _errno_ to one of the values defined below.


Errors
------

*ENOTSOCK*
  ~ The provided _socket_ was invalid.


Example
-------

### Close a request socket

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
INTEGER(KIND = C_INT) :: rc

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_REQ)
rc = zmq_close(socket)
~~~


See also
--------

[zmq_recv][]
[zmq_send][]
[zmq_msg_recv][]
[zmq_msg_send][]
[zmq_socket][]
[zmq_setsockopt][]
[zmq_ctx_term][]
[fzmq][]
