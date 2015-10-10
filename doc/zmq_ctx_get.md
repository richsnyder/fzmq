% zmq_ctx_get


Name
----

zmq_ctx_get - get context options


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_ctx_get(context, option_name) RESULT(option_value)

  TYPE(C_PTR), INTENT(IN) :: context
  INTEGER(KIND = C_INT), INTENT(IN) :: option_name
  INTEGER(KIND = C_INT) :: option_value
~~~


Description
-----------

The *zmq_ctx_get()* function shall return the option specified by the
_option_name_ argument.

The *zmq_ctx_get()* function accepts the following option names:


ZMQ_IO_THREADS:
  ~ *Get number of I/O threads*.  The _ZMQ_IO_THREADS_ argument returns the
    size of the ØMQ thread pool for this context.

ZMQ_MAX_SOCKETS
  ~ *Get maximum number of sockets*.  The _ZMQ_MAX_SOCKETS_ argument returns
    the maximum number of sockets allowed for this context.

ZMQ_SOCKET_LIMIT
  ~ *Get largest configurable number of sockets.* The _ZMQ_SOCKET_LIMIT_
    argument returns the largest number of sockets that [zmq_ctx_set][] will
    accept.

ZMQ_IPV6
  ~ *Get IPv6 option*. The _ZMQ_IPV6_ argument returns the IPv6 option for
    the context.


Return value
------------

The *zmq_ctx_get()* function returns a value of zero or greater if successful.
Otherwise it returns `-1` and sets _errno_ to one of the values defined
below.


Errors
------

EINVAL
  ~ The requested option _option_name_ is unknown.


Example
-------

### Setting a limit on the number of sockets

~~~{.example}
context = zmq_ctx_new()
code = zmq_ctx_set(context, ZMQ_MAX_SOCKETS, 256)
max_sockets = zmq_ctx_get(context, ZMQ_MAX_SOCKETS)
~~~


See also
--------

[zmq_ctx_set][]
