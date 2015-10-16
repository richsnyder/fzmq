% zmq_recvmsg


Name
----

zmq_recvmsg - receive a message part from a socket


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_recvmsg(socket, message, flags) RESULT(nbytes)

  TYPE(C_PTR), INTENT(IN) :: socket
  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: message
  INTEGER(KIND = C_INT), INTENT(IN) :: flags
  INTEGER(KIND = C_INT) :: nbytes
~~~


Description
-----------

This API method is deprecated in favor of [zmq_msg_recv][].

The *zmq_recvmsg()* function shall receive a message part from the socket
referenced by the _socket_ argument and store it in the message referenced by
the _message_ argument.  Any content previously stored in _message_ shall be
properly deallocated.  If there are no message parts available on the specified _socket_ the *zmq_recvmsg()* function shall block until the request can be satisfied.  The _flags_ argument is a combination of the flags defined below:

*ZMQ_DONTWAIT*
  ~ Specifies that the operation should be performed in non-blocking mode.  If
    there are no messages available on the specified _socket_, the
    *zmq_recvmsg()* function shall fail with _errno_ set to *EAGAIN*.

### Multi-part messages

A ØMQ message is composed of one or more message parts. Each message part is an
independent *zmq_msg_t* in its own right.  ØMQ ensures atomic delivery of
messages: peers shall receive either all _message parts_ of a message or none
at all.  The total number of message parts is unlimited except by available
memory.

An application that processes multi-part messages must use either
*zmq_msg_more()* or the *ZMQ_RCVMORE* [zmq_getsockopt][] option after calling
*zmq_recvmsg()* to determine if there are further parts to receive.


Return value
------------

The *zmq_recvmsg()* function shall return the number of bytes in the message
if successful.  Otherwise it shall return `-1` and set _errno_ to one of the
values defined below.


Errors
------

*EAGAIN*
  ~ Non-blocking mode was requested and no messages are available at the
    moment.

*ENOTSUP*
  ~ The *zmq_recvmsg()* operation is not supported by this socket type.

*EFSM*
  ~ The *zmq_recvmsg()* operation cannot be performed on this socket at the
    moment due to the socket not being in the appropriate state.  This error
    may occur with socket types that switch between several states, such as
    *ZMQ_REP*.  See the _messaging patterns_ section of [zmq_socket][] for more
    information.

*ETERM*
  ~ The ØMQ _context_ associated with the specified _socket_ was terminated.

*ENOTSOCK*
  ~ The provided _socket_ was invalid.

*EINTR*
  ~ The operation was interrupted by delivery of a signal before a message was
    available.

*EFAULT*
  ~ The message passed to the function was invalid.


See also
--------

[zmq_msg_send][]
[zmq_msg_recv][]
[zmq_send][]
[zmq_recv][]
[zmq_socket][]
[fzmq][]
