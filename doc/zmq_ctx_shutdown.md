% zmq_ctx_shutdown


Name
----

zmq_ctx_shutdown - shutdown a ØMQ context


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_ctx_shutdown(context) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: context
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_ctx_shutdown()* function shall shutdown the ØMQ context _context_.

Context shutdown will cause any blocking operations currently in progress on
sockets open within the context to return immediately with an error code of
ETERM.  With the exception of *zmq_close()*, any further operations on
sockets open within the context shall fail with an error code of ETERM.

This function is optional; client code is still required to call the
[zmq_ctx_term][] function to free all resources allocated by ØMQ.


Return value
------------

The *zmq_ctx_shutdown()* function shall return zero if successful. Otherwise
it shall return `-1` and set _errno_ to one of the values defined below.


Errors
------

EFAULT
  ~ The provided _context_ was invalid.


See also
--------

[zmq_init][]
[zmq_ctx_term][]
[zmq_close][]
[zmq_setsockopt][]
