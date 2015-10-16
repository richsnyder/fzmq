% zmq_send


Name
----

zmq_send - send a message part on a socket


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_send(socket, buf, len, flags) RESULT(nbytes)
FUNCTION zmq_send(socket, str, flags) RESULT(nbytes)

  TYPE(C_PTR), INTENT(IN) :: socket
  TYPE(C_PTR), INTENT(IN) :: buf
  CHARACTER(LEN = *), INTENT(IN) :: str
  INTEGER(KIND = C_SIZE_T), INTENT(IN) :: len
  INTEGER(KIND = C_INT), INTENT(IN) :: flags
  INTEGER(KIND = C_INT) :: nbytes
~~~


Description
-----------

The *zmq_send()* function shall queue a message created from the buffer
referenced by the _buf_ and _len_ arguments, or by the _str_ argument.  The
_flags_ argument is a combination of the flags defined below:

*ZMQ_DONTWAIT*
  ~ For socket types such as DEALER and PUSH that block when there are no
    available peers (or all peers have full high-water mark), this flag
    specifies that the operation should be performed in non-blocking mode. If
    the message cannot be queued on the _socket_, the *zmq_send()* function
    shall fail with _errno_ set to *EAGAIN*.

*ZMQ_SNDMORE*
  ~ Specifies that the message being sent is a multi-part message, and that
    further message parts are to follow.  Refer to the section regarding
    multi-part messages below for a detailed description.

A successful invocation of *zmq_send()* does not indicate that the message has
been transmitted to the network, only that it has been queued on the _socket_
and that ØMQ has assumed responsibility for the message.

### Multi-part messages

A ØMQ message is composed of one or more message parts.  ØMQ ensures atomic
delivery of messages: peers shall receive either all _message parts_ of a
message or none at all.  The total number of message parts is unlimited except
by available memory.

An application that sends multi-part messages must use the *ZMQ_SNDMORE* flag
when sending each message part except the final one.


Return value
------------

The *zmq_send()* function shall return the number of bytes in the message if
successful.  Otherwise it shall return `-1` and set _errno_ to one of the
values defined below.


Errors
------

*EAGAIN*
  ~ Non-blocking mode was requested and the message cannot be sent at the
    moment.

*ENOTSUP*
  ~ The *zmq_send()* operation is not supported by this socket type.

*EFSM*
  ~ The *zmq_send()* operation cannot be performed on this socket at the moment
    due to the socket not being in the appropriate state.  This error may occur
    with socket types that switch between several states, such as *ZMQ_REP*.
    See the _messaging patterns_ section of [zmq_socket][] for more
    information.

*ETERM*
  ~ The ØMQ _context_ associated with the specified _socket_ was terminated.

*ENOTSOCK*
  ~ The provided _socket_ was invalid.

*EINTR*
  ~ The operation was interrupted by delivery of a signal before the message
    was sent.

*EHOSTUNREACH*
  ~ The message cannot be routed.


Example
-------

### Sending a message

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
INTEGER(KIND = C_INT) :: rc
INTEGER(KIND = C_INT) :: nbytes
CHARACTER(KIND = C_CHAR), DIMENSION(13), TARGET :: greeting

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_REQ)
rc = zmq_connect(socket, 'tcp://server1:5555')

greeting = 'Hello, world!'
nbytes = zmq_send(socket, C_LOC(greeting), 13_C_SIZE_T, 0)

rc = zmq_close(socket)
rc = zmq_ctx_term(context)
~~~

### Sending a multi-part message

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
INTEGER(KIND = C_INT) :: rc
INTEGER(KIND = C_INT) :: nbytes

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_REQ)
rc = zmq_connect(socket, 'tcp://server1:5555')

nbytes = zmq_send(socket, 'Hello,', ZMQ_SNDMORE)
nbytes = zmq_send(socket, 'world!', 0)

rc = zmq_close(socket)
rc = zmq_ctx_term(context)
~~~


See also
--------

[zmq_send_const][]
[zmq_recv][]
[zmq_socket][]
[fzmq][]
