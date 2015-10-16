% zmq_msg_data


Name
----

zmq_msg_data - retrieve a pointer to message content


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_msg_data(message) RESULT(data)

  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: message
  TYPE(C_PTR) :: data
~~~


Description
-----------

The *zmq_msg_data()* function shall return a C pointer to the message content
of the message object referenced by _message_.  Use the *c_f_pointer()*
function to assign the target of the C pointer to a Fortran pointer.

Never access *zmq_msg_t* members directly, instead always use the *zmq_msg*
family of functions.


Return value
------------

Upon successful completion, *zmq_msg_data()* shall return a pointer to the
message content.


Errors
------

No errors are defined.


Example
-------

### Receive a string over a reply socket

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
TYPE(ZMQ_MSG_T) :: message
INTEGER(KIND = C_INT) :: rc
INTEGER(KIND = C_INT) :: nbytes
INTEGER(KIND = C_SIZE_T) :: length
CHARACTER(KIND = C_CHAR, LEN = 8) :: name
CHARACTER(KIND = C_CHAR, LEN = :), POINTER :: message_data

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_REP)
rc = zmq_bind(socket, 'tcp://*:5555')

rc = zmq_msg_init(message)
nbytes = zmq_msg_recv(message, socket, 0)
CALL c_f_pointer(zmq_msg_data(message), message_data)
length = min(8, nbytes)
name = message_data(1:length)
rc = zmq_msg_close(message)
~~~


See also
--------

[zmq_msg_size][]
[zmq_msg_init][]
[zmq_msg_init_size][]
[zmq_msg_init_data][]
[zmq_msg_close][]
[fzmq][]
