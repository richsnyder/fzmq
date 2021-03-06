% zmq_send_const


Name
----

zmq_send_const - send a constant-memory message part on a socket


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_send_const(socket, bin_value, len, flags) RESULT(nbytes)
FUNCTION zmq_send_const(socket, i32_value, flags) RESULT(nbytes)
FUNCTION zmq_send_const(socket, i64_value, flags) RESULT(nbytes)
FUNCTION zmq_send_const(socket, str_value, flags) RESULT(nbytes)

  TYPE(C_PTR), INTENT(IN) :: socket
  TYPE(C_PTR), INTENT(IN) :: bin_value
  INTEGER(KIND = C_SIZE_T), INTENT(IN) :: len
  INTEGER(KIND = C_INT), INTENT(IN), TARGET :: i32_value
  INTEGER(KIND = C_INT64_T), INTENT(IN), TARGET :: i32_value
  CHARACTER(LEN = *), INTENT(IN), TARGET :: str_value
  INTEGER(KIND = C_INT), INTENT(IN) :: flags
  INTEGER(KIND = C_INT) :: nbytes
~~~


Description
-----------

The *zmq_send_const()* function shall queue a message created from the
_*_value_ argument.  For the first form, the _len_ argument is the size of the
_bin_value_ buffer in bytes.  The message buffer is assumed to be
constant-memory and will therefore not be copied or deallocated in any way.
The _flags_ argument is a combination of the flags defined below:

*ZMQ_DONTWAIT*
  ~ For socket types (DEALER, PUSH) that block when there are no available
    peers (or all peers have full high-water mark), specifies that the
    operation should be performed in non-blocking mode.  If the message cannot
    be queued on the _socket_, the *zmq_send_const()* function shall fail with
    _errno_ set to *EAGAIN*.

*ZMQ_SNDMORE*
  ~ Specifies that the message being sent is a multi-part message, and that
    further message parts are to follow.  Refer to the section regarding
    multi-part messages below for a detailed description.

A successful invocation of *zmq_send_const()* does not indicate that the
message has been transmitted to the network, only that it has been queued on
the _socket_ and ØMQ has assumed responsibility for the message.

### Multi-part messages

A ØMQ message is composed of 1 or more message parts.  ØMQ ensures atomic
delivery of messages: peers shall receive either all _message parts_ of a
message or none at all.  The total number of message parts is unlimited except
by available memory.

An application that sends multi-part messages must use the *ZMQ_SNDMORE* flag
when sending each message part except the final one.


Return value
------------

The *zmq_send_const()* function shall return the number of bytes in the message
if successful.  Otherwise it shall return `-1` and set _errno_ to one of the
values defined below.


Errors
------

*EAGAIN*
  ~ Non-blocking mode was requested and the message cannot be sent at the
    moment.

*ENOTSUP*
  ~ The *zmq_send_const()* operation is not supported by this socket type.

*EFSM*
  ~ The *zmq_send_const()* operation cannot be performed on this socket at the
    moment due to the socket not being in the appropriate state.  This error
    may occur with socket types that switch between several states, such as
    *ZMQ_REP*.  See the _messaging patterns_ section of [zmq_socket][] for more
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

### Sending a multi-part message

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
INTEGER(KIND = C_INT) :: rc

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_PAIR)
rc = zmq_connect(socket, 'inproc://fzmq')

rc = zmq_send_const(socket, 123_4, ZMQ_SNDMORE)
rc = zmq_send_const(socket, 456_8, ZMQ_SNDMORE)
rc = zmq_send_const(socket, "ABC" // C_NULL_CHAR, 0)

rc = zmq_close(socket)
rc = zmq_ctx_term(context)
~~~


See also
--------
[zmq_send][]
[zmq_recv][]
[zmq_socket][]
[fzmq][]
