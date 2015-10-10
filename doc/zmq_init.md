% zmq_init


Name
----

zmq_init - initialise a ØMQ context


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_init(io_threads) RESULT(context)

  INTEGER(KIND = C_INT), INTENT(IN) :: io_threads
  TYPE(C_PTR) :: context
~~~


Description
-----------

The *zmq_init()* function initialises a ØMQ context.

The _io_threads_ argument specifies the size of the ØMQ thread pool to handle
I/O operations. If your application is using only the _inproc_ transport for
messaging you may set this to zero, otherwise set it to at least one.

### Thread safety

A ØMQ context is thread safe and may be shared among as many application
threads as necessary, without any additional locking required on the part of
the caller.

This function is deprecated by [zmq_ctx_new][].


Return value
------------

The *zmq_init()* function shall return an opaque handle to the initialised
context if successful. Otherwise it shall return _C_NULL_PTR_ and set _errno_
to one of the values defined below.


Errors
------

EINVAL
  ~ An invalid number of _io_threads_ was requested.


See also
--------

[zmq_term][]
