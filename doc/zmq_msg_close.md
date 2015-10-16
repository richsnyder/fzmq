% zmq_msg_close


Name
----

zmq_msg_close - release a ØMQ message


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_msg_close(message) RESULT(code)

  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: message
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_msg_close()* function shall inform the ØMQ infrastructure that any
resources associated with the message object referenced by _message_ are no
longer required and may be released.  Actual release of resources associated
with the message object shall be postponed by ØMQ until all users of the
message or underlying data buffer have indicated it is no longer required.

Applications should ensure that *zmq_msg_close()* is called once a message is
no longer required, otherwise memory leaks may occur.  Note that this is NOT
necessary after a successful *zmq_msg_send()*.

Never access *zmq_msg_t* members directly, instead always use the *zmq_msg*
family of functions.


Return value
------------

The *zmq_msg_close()* function shall return zero if successful.  Otherwise
it shall return `-1` and set _errno_ to one of the values defined below.


Errors
------

*EFAULT*
  ~ Invalid message.


Example
-------

### Send a greeting over a reply socket

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

length = length + 8_C_SIZE_T
rc = zmq_msg_init_size(message, length)
CALL c_f_pointer(zmq_msg_data(message), message_data)
message_data(1:7) = 'Hello, '
message_data(8:length-1) = name
message_data(length:length) = '!'
nbytes = zmq_msg_send(message, socket, 0)

rc = zmq_close(socket)
rc = zmq_ctx_term(context)
~~~


See also
--------

[zmq_msg_init][]
[zmq_msg_init_size][]
[zmq_msg_init_data][]
[zmq_msg_data][]
[zmq_msg_size][]
[fzmq][]
