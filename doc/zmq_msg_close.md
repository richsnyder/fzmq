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
longer required and may be released. Actual release of resources associated
with the message object shall be postponed by ØMQ until all users of the
message or underlying data buffer have indicated it is no longer required.

Applications should ensure that *zmq_msg_close()* is called once a message is
no longer required, otherwise memory leaks may occur. Note that this is NOT
necessary after a successful *zmq_msg_send()*.

Never access _zmq_msg_t_ members directly, instead always use the *zmq_msg*
family of functions.


Return value
------------

The *zmq_msg_close()* function shall return zero if successful. Otherwise
it shall return `-1` and set _errno_ to one of the values defined below.


Errors
------

EFAULT
  ~ Invalid message.


See also
--------

[zmq_msg_init][]
[zmq_msg_init_size][]
[zmq_msg_init_data][]
[zmq_msg_data][]
[zmq_msg_size][]
