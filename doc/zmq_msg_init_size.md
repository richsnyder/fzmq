% zmq_msg_init_size


Name
----

zmq_msg_init_size - initialise a ØMQ message of a specified size


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_msg_init_size(message, size) RESULT(code)

  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: message
  INTEGER(KIND = C_SIZE_T), INTENT(IN) :: size
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_msg_init_size()* function shall allocate any resources required to
store a message _size_ bytes long and initialise the message object referenced
by _message_ to represent the newly allocated message.

The implementation shall choose whether to store message content on the stack
(small messages) or on the heap (large messages). For performance reasons
*zmq_msg_init_size()* shall not clear the message data.

Never access _zmq_msg_t_ members directly, instead always use the *zmq_msg*
family of functions.

The functions *zmq_msg_init()*, *zmq_msg_init_data()* and
*zmq_msg_init_size()* are mutually exclusive. Never initialize the same
message twice.


Return value
------------

The *zmq_msg_init_size()* function shall return zero if successful. Otherwise
it shall return `-1` and set _errno_ to one of the values defined below.


Errors
------

ENOMEM
  ~ Insufficient storage space is available.


See also
--------

[zmq_msg_init_data][]
[zmq_msg_init][]
[zmq_msg_close][]
[zmq_msg_data][]
[zmq_msg_size][]
