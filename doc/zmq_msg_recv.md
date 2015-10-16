% zmq_msg_recv


Name
----

zmq_msg_recv - receive a message part from a socket


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_msg_recv(message, socket, flags) RESULT(nbytes)

  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: message
  TYPE(C_PTR), INTENT(IN) :: socket
  INTEGER(KIND = C_INT), INTENT(IN) :: flags
  INTEGER(KIND = C_INT) :: nbytes
~~~


Description
-----------

The *zmq_msg_recv()* function shall receive a message part from the socket
referenced by the _socket_ argument and store it in the message referenced by
the _message_ argument.  Any content previously stored in _message_ shall be
properly deallocated.  If there are no message parts available on the specified
_socket_, the *zmq_msg_recv()* function shall block until the request can be
satisfied.  The _flags_ argument is a combination of the flags defined below:

*ZMQ_DONTWAIT*
  ~ Specifies that the operation should be performed in non-blocking mode.  If
    there are no messages available on the specified _socket_, the
    *zmq_msg_recv()* function shall fail with _errno_ set to *EAGAIN*.


### Multi-part messages

A ØMQ message is composed of one or more message parts.  Each message part is
an independent *zmq_msg_t* in its own right.  ØMQ ensures atomic delivery of
messages: peers shall receive either all _message parts_ of a message or none
at all.  The total number of message parts is unlimited except by available
memory.

An application that processes multi-part messages must use either
*zmq_msg_more()* or the *ZMQ_RCVMORE* [zmq_getsockopt][] option after calling
*zmq_msg_recv()* to determine if there are further parts to receive.

This function deprecates [zmq_recvmsg][], which is considered less consistent
with other message manipulation functions.


Return value
------------

The *zmq_msg_recv()* function shall return the number of bytes in the message
if successful.  Otherwise it shall return `-1` and set _errno_ to one of the
values defined below.


Errors
------

*EAGAIN*
  ~ Non-blocking mode was requested and no messages are available at the
    moment.

*ENOTSUP*
  ~ The *zmq_msg_recv()* operation is not supported by this socket type.

*EFSM*
  ~ The *zmq_msg_recv()* operation cannot be performed on this socket at the
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


Example
-------

### Receiving a message from a socket

~~~{.example}
TYPE(C_PTR) :: socket
TYPE(ZMQ_MSG_T) :: message
INTEGER(KIND = C_INT) :: rc

rc = zmq_msg_init(message)
rc = zmq_msg_recv(message, socket, 0)
rc = zmq_msg_close(message)
~~~

### Receiving a multi-part message

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
TYPE(ZMQ_MSG_T) :: message
INTEGER(KIND = C_INT) :: rc

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_REP)
rc = zmq_bind(socket, 'tcp://*:5555')

DO WHILE (.TRUE.)
  rc = zmq_msg_init(message)
  rc = zmq_msg_recv(message, socket, 0)
  IF (zmq_msg_more(message)) THEN
    PRINT*, 'more'
  ELSE
    PRINT*, 'end'
    EXIT
  END IF
  rc = zmq_msg_close(message)
END DO

rc = zmq_close(socket)
rc = zmq_ctx_term(context)
~~~


See also
--------

[zmq_recv][]
[zmq_send][]
[zmq_msg_send][]
[zmq_getsockopt][]
[zmq_socket][]
[fzmq][]
