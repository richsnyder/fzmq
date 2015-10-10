% zmq_recv


Name
----

zmq_recv - receive a message part from a socket


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_recv(socket, buf, len, flags) RESULT(nbytes)
FUNCTION zmq_recv(socket, str, flags) RESULT(nbytes)

  TYPE(C_PTR), INTENT(IN) :: socket
  TYPE(C_PTR), INTENT(IN) :: buf
  CHARACTER(LEN = *), INTENT(INOUT) :: str
  INTEGER(KIND = C_SIZE_T), INTENT(IN) :: len
  INTEGER(KIND = C_INT), INTENT(IN) :: flags
  INTEGER(KIND = C_INT) :: nbytes
~~~


Description
-----------
The *zmq_recv()* function shall receive a message from the socket referenced
by the _socket_ argument and store it in the buffer referenced by the _buf_
argument, or in the character string _str_. Any bytes exceeding the length
of the buffer as specified by the _len_ argument, or the length of _str_, shall
be truncated. If there are no messages available on the specified _socket_
the *zmq_recv()* function shall block until the request can be satisfied.
The _flags_ argument is a combination of the flags defined below:

ZMQ_DONTWAIT
  ~ Specifies that the operation should be performed in non-blocking mode. If
    there are no messages available on the specified _socket_, the *zmq_recv()*
    function shall fail with _errno_ set to EAGAIN.

### Multi-part messages

A ØMQ message is composed of 1 or more message parts. ØMQ ensures atomic
delivery of messages: peers shall receive either all _message parts_ of a
message or none at all. The total number of message parts is unlimited except
by available memory.

An application that processes multi-part messages must use the _ZMQ_RCVMORE_
[zmq_getsockopt][] option after calling *zmq_recv()* to determine if there are
further parts to receive.

Return value
------------

The *zmq_recv()* function shall return the number of bytes in the message
if successful. Note that the value can exceed the value of the _len_ parameter,
or of the character string _str_, in the case where the message was truncated.
If not successful the function shall return `-1` and set _errno_ to one of the
values defined below.


Errors
------

EAGAIN
  ~ Non-blocking mode was requested and no messages are available at the
    moment.

ENOTSUP
  ~ The *zmq_recv()* operation is not supported by this socket type.

EFS
  ~ The *zmq_recv()* operation cannot be performed on this socket at the moment
    due to the socket not being in the appropriate state.  This error may occur
    with socket types that switch between several states, such as _ZMQ_REP_.
    See the _messaging patterns_ section of [zmq_socket][] for more
    information.

ETERM
  ~ The ØMQ _context_ associated with the specified _socket_ was terminated.

ENOTSOCK
  ~ The provided _socket_ was invalid.

EINTR
  ~ The operation was interrupted by delivery of a signal before a message was
    available.


Example
-------

### Receiving a message from a socket

~~~{.example}
CHARACTER(KIND = C_CHAR), DIMENSION(256) :: str
nbytes = zmq_recv(socket, str, 0);
~~~


See also
--------

[zmq_send][]
[zmq_getsockopt][]
[zmq_socket][]
