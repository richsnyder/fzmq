% zmq_msg_size


Name
----

zmq_msg_size - retrieve message content size in bytes


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_msg_size(message) RESULT(nbytes)

  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: message
  INTEGER(KIND = C_SIZE_T) :: nbytes
~~~


Description
-----------

The *zmq_msg_size()* function shall return the size in bytes of the content of
the message object referenced by _message_.

Never access _zmq_msg_t_ members directly, instead always use the *zmq_msg*
family of functions.


Return value
------------

Upon successful completion, *zmq_msg_size()* shall return the size of the
message content in bytes.


Errors
------

No errors are defined.


See also
--------

[zmq_msg_data][]
[zmq_msg_init][]
[zmq_msg_init_size][]
[zmq_msg_init_data][]
[zmq_msg_close][]
