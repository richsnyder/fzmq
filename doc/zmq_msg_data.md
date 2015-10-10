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

The *zmq_msg_data()* function shall return a pointer to the message content of
the message object referenced by _message_.

Never access _zmq_msg_t_ members directly, instead always use the *zmq_msg*
family of functions.


Return value
------------

Upon successful completion, *zmq_msg_data()* shall return a pointer to the
message content.


Errors
------

No errors are defined.


See also
--------

[zmq_msg_size][]
[zmq_msg_init][]
[zmq_msg_init_size][]
[zmq_msg_init_data][]
[zmq_msg_close][]
