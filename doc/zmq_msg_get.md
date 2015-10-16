% zmq_msg_get


Name
----

zmq_msg_get - get a message property


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_msg_get(message, property) RESULT(value)

  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: message
  INTEGER(KIND = C_INT), INTENT(IN) :: property
  INTEGER(KIND = C_INT) :: value
~~~


Description
-----------

The *zmq_msg_get()* function shall return the value for the property
specified by the _property_ argument for the message pointed to by the
_message_ argument.

The following properties can be retrieved with the *zmq_msg_get()* function:

*ZMQ_MORE*
  ~ Indicates that there are more message frames to follow after the _message_.

*ZMQ_SRCFD*
  ~ Returns the file descriptor of the socket the _message_ was read from.
    This allows application to retrieve the remote endpoint via
    *getpeername(2)*.  Be aware that the respective socket might be closed
    already, reused even.  Currently only implemented for TCP sockets.

*ZMQ_SHARED*
  ~ Indicates that a message MAY share underlying storage with another copy of
    this message.


Return value
------------

The *zmq_msg_get()* function shall return the value for the property if
successful.  Otherwise it shall return `-1` and set _errno_ to one of the
values defined below.


Errors
------

*EINVAL*
  ~ The requested _property_ is unknown.


Example
-------

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
  IF (zmq_msg_get(message, ZMQ_MORE) == 1) THEN
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

[zmq_msg_set][]
[zmq_msg_init][]
[zmq_msg_close][]
[fzmq][]
