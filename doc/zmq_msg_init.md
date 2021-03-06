% zmq_msg_init


Name
----

zmq_msg_init - initialize an empty ØMQ message


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_msg_init(message) RESULT(code)

  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: message
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_msg_init()* function shall initialize the message object referenced by
_message_ to represent an empty message.  This function is most useful when
called before receiving a message with *zmq_recv()* or *zmq_msg_recv()*.

Never access *zmq_msg_t* members directly, instead always use the *zmq_msg*
family of functions.

The functions *zmq_msg_init()*, *zmq_msg_init_data()*, and
*zmq_msg_init_size()* are mutually exclusive.  Never initialize the same
message twice.


Return value
------------

The *zmq_msg_init()* function always returns zero.


Errors
------

No errors are defined.


Example
-------

### Receiving a message from a socket

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
TYPE(ZMQ_MSG_T) :: message
INTEGER(KIND = C_INT) :: rc
INTEGER(KIND = C_INT) :: nbytes

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_REP)
rc = zmq_bind(socket, 'tcp://*:5555')

rc = zmq_msg_init(message)
nbytes = zmq_msg_recv(message, socket, 0)
rc = zmq_msg_close(message)

rc = zmq_close(socket)
rc = zmq_ctx_term(context)
~~~


See also
--------

[zmq_msg_init_size][]
[zmq_msg_init_data][]
[zmq_msg_close][]
[zmq_msg_data][]
[zmq_msg_size][]
[fzmq][]
