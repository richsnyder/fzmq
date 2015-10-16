% zmq_msg_set


Name
----

zmq_msg_set - set a message property


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_msg_set(message, property, value) RESULT(code)

  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: message
  INTEGER(KIND = C_INT), INTENT(IN) :: property
  INTEGER(KIND = C_INT), INTENT(IN) :: value
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_msg_set()* function shall set the property specified by the _property_
argument to the value of the _value_ argument for the ØMQ message fragment
pointed to by the _message_ argument.

Currently the *zmq_msg_set()* function does not support any property names.


Return value
------------

The *zmq_msg_set()* function shall return zero if successful.  Otherwise it
shall return `-1` and set _errno_ to one of the values defined below.


Errors
------

*EINVAL*
  ~ The requested property _property_ is unknown.


See also
--------

[zmq_msg_get][]
[fzmq][]
