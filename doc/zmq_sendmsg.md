% zmq_sendmsg


Name
----

zmq_sendmsg - send a message part on a socket


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_sendmsg(socket, message, flags) RESULT(nbytes)

  TYPE(C_PTR), INTENT(IN) :: socket
  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: message
  INTEGER(KIND = C_INT), INTENT(IN) :: flags
  INTEGER(KIND = C_INT) :: nbytes
~~~


Description
-----------

The *zmq_sendmsg()* function shall queue the message referenced by the
_message_ argument to be sent to the socket referenced by the _socket_
argument.  The _flags_ argument is a combination of the flags defined below:

ZMQ_DONTWAIT
  ~ For socket types (DEALER, PUSH) that block when there are no available
    peers (or all peers have full high-water mark), specifies that the
    operation should be performed in non-blocking mode. If the message cannot
    be queued on the _socket_, the *zmq_sendmsg()* function shall fail with
    _errno_ set to EAGAIN.

ZMQ_SNDMORE
  ~ Specifies that the message being sent is a multi-part message, and that
    further message parts are to follow. Refer to the section regarding
    multi-part messages below for a detailed description.

The _zmq_msg_t_ structure passed to *zmq_sendmsg()* is nullified during the
call. If you want to send the same message to multiple sockets you have to copy
it using (e.g. using *zmq_msg_copy()*).

> A successful invocation of *zmq_sendmsg()* does not indicate that the message
> has been transmitted to the network, only that it has been queued on the
> _socket_ and ØMQ has assumed responsibility for the message.

> This API method is deprecated in favor of [zmq_msg_send][].

### Multi-part messages

A ØMQ message is composed of 1 or more message parts. Each message part is an
independent _zmq_msg_t_ in its own right. ØMQ ensures atomic delivery of
messages: peers shall receive either all _message parts_ of a message or none
at all. The total number of message parts is unlimited except by available
memory.

An application that sends multi-part messages must use the _ZMQ_SNDMORE_ flag
when sending each message part except the final one.


Return value
------------

The *zmq_sendmsg()* function shall return the number of bytes in the message
if successful. Otherwise it shall return `-1` and set _errno_ to one of the
values defined below.


Errors
------

EAGAIN
  ~ Non-blocking mode was requested and the message cannot be sent at the
    moment.

ENOTSUP
  ~ The *zmq_sendmsg()* operation is not supported by this socket type.

EFSM
  ~ The *zmq_sendmsg()* operation cannot be performed on this socket at the
    moment due to the socket not being in the appropriate state.  This error
    may occur with socket types that switch between several states, such as
    _ZMQ_REP_.  See the _messaging patterns_ section of [zmq_socket][] for more
    information.

ETERM
  ~ The ØMQ _context_ associated with the specified _socket_ was terminated.

ENOTSOCK
  ~ The provided _socket_ was invalid.

EINTR
  ~ The operation was interrupted by delivery of a signal before the message
    was sent.

EFAULT
  ~ Invalid message.

EHOSTUNREACH
  ~ The message cannot be routed.


Example
-------

### Filling in a message and sending it to a socket

~~~{.example}
TYPE(C_PTR) :: data
TYPE(ZMQ_MSG_T) :: message
INTEGER(KIND = C_INT) :: value
INTEGER(KIND = C_INT) :: nbytes
INTEGER(KIND = C_SIZE_T) :: size
CHARACTER(KIND = C_CHAR, LEN = :), POINTER :: buffer
CHARACTER(KIND = C_CHAR, LEN = :), POINTER :: range

value = 123
size = C_SIZEOF(value)
rc = zmq_msg_init_size(message, size)
data = zmq_msg_data(message)
CALL C_F_POINTER(data, buffer)
range => buffer(1:size)
range = TRANSFER(value, range)
nbytes = zmq_sendmsg(socket, message, 0)
~~~

### Sending a multi-part message

~~~{.example}
rc = zmq_sendmsg(socket, frame1, ZMQ_SNDMORE)
rc = zmq_sendmsg(socket, frame2, ZMQ_SNDMORE)
rc = zmq_sendmsg(socket, frame3, 0)
~~~


See also
--------

[zmq_recv][]
[zmq_socket][]
