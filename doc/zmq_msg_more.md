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
message, and there are further parts to receive.  This method can safely be
called after *zmq_msg_close()*.  This method is identical to *zmq_msg_get()*
with an argument of *ZMQ_MORE*.


Return value
------------

The *zmq_msg_more()* function shall return `.FALSE.` if this is the final part
of a multi-part message, or the only part of a single-part message.  It shall
return `.TRUE.` if there are further parts to receive.


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

[zmq_msg_get][]
[zmq_msg_set][]
[zmq_msg_init][]
[zmq_msg_close][]
[fzmq][]
