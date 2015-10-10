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

ZMQ_MORE
  ~ Indicates that there are more message frames to follow after the _message_.

ZMQ_SRCFD
  ~ Returns the file descriptor of the socket the _message_ was read from. This
    allows application to retrieve the remote endpoint via _getpeername(2)_. Be
    aware that the respective socket might be closed already, reused even.
    Currently only implemented for TCP sockets.

ZMQ_SHARED
  ~ Indicates that a message MAY share underlying storage with another copy of
    this message.


Return value
------------

The *zmq_msg_get()* function shall return the value for the property if
successful. Otherwise it shall return `-1` and set _errno_ to one of the
values defined below.


Errors
------

EINVAL
  ~ The requested _property_ is unknown.


Example
-------

### Receiving a multi-frame message

~~~{.example}
TYPE(ZMQ_MSG_T) :: frame
DO WHILE (.TRUE.)
  rc = zmq_msg_init(frame)
  rc = zmq_msg_recv(socket, frame, 0)
  IF (zmq_msg_get(frame, ZMQ_MORE) == 1) THEN
    PRINT*, 'more'
  ELSE
    PRINT*, 'end'
    EXIT
  END IF
  rc = zmq_msg_close(frame)
END DO
~~~


See also
--------

[zmq_msg_set][]
[zmq_msg_init][]
[zmq_msg_close][]
