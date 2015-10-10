% zmq_ctx_destroy


Name
----

zmq_ctx_destroy - terminate a ØMQ context


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_ctx_destroy(context) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: context
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_ctx_destroy()* function shall destroy the ØMQ context.

Context termination is performed in the following steps:

* Any blocking operations currently in progress on sockets open within the
  context shall return immediately with an error code of ETERM. With the
  exception of *zmq_close()*, any further operations on sockets open within the
  context shall fail with an error code of ETERM.
* After interrupting all blocking calls, *zmq_ctx_destroy()* shall block until
  the following conditions are satisfied:
    - All sockets open within the context have been closed with *zmq_close()*.
    - For each socket within the context, all messages sent by the application
      with *zmq_send()* and *zmq_msg_send()* have either been physically
      transferred to a network peer or the socket's linger period set with the
      _ZMQ_LINGER_ socket option has expired.

For further details regarding socket linger behavior refer to the _ZMQ_LINGER_
option in [zmq_setsockopt][].

This function is deprecated by [zmq_ctx_term][].


Return value
------------

The *zmq_ctx_destroy()* function shall return zero if successful. Otherwise it
shall return -1 and set _errno_ to one of the values defined below.


Errors
------

EFAULT
  ~ The provided context was invalid.

EINTR
  ~ Termination was interrupted by a signal. It can be restarted if needed.


See also
--------

[zmq_init][]
[zmq_close][]
[zmq_setsockopt][]
