% zmq_unbind


Name
----

zmq_unbind - Stop accepting connections on a socket


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_unbind(socket, endpoint) RESULT(code)
  TYPE(C_PTR), INTENT(IN) :: socket
  CHARACTER(LEN = *), INTENT(IN) :: endpoint
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_unbind()* function shall unbind a socket specified by the _socket_
argument from the endpoint specified by the _endpoint_ argument.

The _endpoint_ argument is as described in [zmq_bind][].

### Unbinding wild-card address from a socket

When the wild-card `*` _endpoint_ (described in [zmq_tcp][] and [zmq_ipc][])
is used in *zmq_bind()*, the caller should use the real _endpoint_ obtained
from the _ZMQ_LAST_ENDPOINT_ socket option to unbind this _endpoint_ from a
socket.


Return value
------------

The *zmq_unbind()* function shall return zero if successful. Otherwise it
shall return `-1` and set _errno_ to one of the values defined below.


Errors
------

EINVAL
  ~ The endpoint supplied is invalid.

ETERM
  ~ The ØMQ _context_ associated with the specified _socket_ was terminated.

ENOTSOCK
  ~ The provided _socket_ was invalid.


Examples
--------

### Unbind a subscriber socket from a TCP transport

~~~{.example}
socket = zmq_socket(context, ZMQ_SUB)
rc = zmq_bind(socket, 'tcp://127.0.0.1:5555')
rc = zmq_unbind(socket, 'tcp://127.0.0.1:5555')
~~~

### Unbind wild-card `*` bound socket

~~~{.example}
CHARACTER(LEN = 32) :: endpoint
socket = zmq_socket(context, ZMQ_SUB)
rc = zmq_bind(socket, 'tcp://127.0.0.1:*')
rc = zmq_getsockopt(socket, ZMQ_LAST_ENDPOINT, endpoint)
rc = zmq_unbind(socket, endpoint)
~~~


See also
--------

[zmq_bind][]
[zmq_socket][]
