% zmq_disconnect


Name
----

zmq_disconnect - Disconnect a socket


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_disconnect(socket, endpoint) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: socket
  CHARACTER(LEN = *), INTENT(IN) :: endpoint
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_disconnect()* function shall disconnect a socket specified by the
_socket_ argument from the endpoint specified by the _endpoint_ argument. Any
outstanding messages physically received from the network but not yet received
by the application with *zmq_recv()* or *zmq_msg_recv()* shall be discarded.
The behaviour for discarding messages sent by the application with *zmq_send()*
and *zmq_msg_send()* but not yet physically transferred to the network depends
on the value of the *ZMQ_LINGER* socket option for the specified _socket_.

The _endpoint_ argument is as described in [zmq_connect][].

The default setting of *ZMQ_LINGER* does not discard unsent messages; this
behaviour may cause the application to block when calling *zmq_ctx_term()*.
For details refer to [zmq_setsockopt][] and [zmq_ctx_term][].


Return value
------------

The *zmq_disconnect()* function shall return zero if successful.  Otherwise it
shall return `-1` and set _errno_ to one of the values defined below.


Errors
------

*EINVAL*
  ~ The endpoint supplied is invalid.

*ETERM*
  ~ The ØMQ _context_ associated with the specified _socket_ was terminated.

*ENOTSOCK*
  ~ The provided _socket_ was invalid.

*ENOENT*
  ~ The provided endpoint is not connected.


Example
-------

### Connecting and disconnecting a subscriber socket

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
INTEGER(KIND = C_INT) :: rc

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_SUB)
rc = zmq_connect(socket, 'tcp://server:5555')
rc = zmq_disconnect(socket, 'tcp://server:5555')
~~~


See also
--------

[zmq_connect][]
[zmq_socket][]
[fzmq][]
