% zmq_msg_more


Name
----

zmq_msg_more - indicate if there are more message parts to receive


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_msg_more(message) RESULT(more)

  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: message
  LOGICAL :: more
~~~


Description
-----------

The *zmq_msg_more()* function indicates whether this is part of a multi-part
message, and there are further parts to receive. This method can safely be
called after *zmq_msg_close()*. This method is identical to *zmq_msg_get()*
with an argument of _ZMQ_MORE_.


Return value
------------

The *zmq_msg_more()* function shall return zero if this is the final part of
a multi-part message, or the only part of a single-part message. It shall
return `1` if there are further parts to receive.


Example
-------

### Receiving a multi-part message

~~~{.example}
TYPE(ZMQ_MSG_T) :: frame
DO WHILE (.TRUE.)
  rc = zmq_msg_init(frame)
  rc = zmq_msg_recv(socket, frame, 0)
  IF (zmq_msg_more(frame)) THEN
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

[zmq_msg_get][]
[zmq_msg_set][]
[zmq_msg_init][]
[zmq_msg_close][]
