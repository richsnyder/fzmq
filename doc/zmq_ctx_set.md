% zmq_ctx_set


Name
----

zmq_ctx_set - set context options


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_ctx_set(context, option_name, option_value) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: context
  INTEGER(KIND = C_INT), INTENT(IN) :: option_name
  INTEGER(KIND = C_INT), INTENT(IN) :: option_value
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_ctx_set()* function shall set the option specified by the
_option_name_ argument to the value of the _option_value_ argument.

The *zmq_ctx_set()* function accepts the following options:

*ZMQ_IO_THREADS*
  ~ *Set number of I/O threads*.  The *ZMQ_IO_THREADS* argument specifies the
    size of the ØMQ thread pool to handle I/O operations. If your application
    is using only the _inproc_ transport for messaging you may set this to
    zero, otherwise set it to at least one. This option only applies before
    creating any sockets on the _context_.

    ------------- -
    Default value 1
    ------------- -

*ZMQ_THREAD_SCHED_POLICY*
  ~ *Set scheduling policy for I/O threads*.  The *ZMQ_THREAD_SCHED_POLICY*
    argument sets the scheduling policy for the context's internal thread pool.
    This option is not available on Windows.  Supported values for this option
    can be found in the `sched.h` file, or at
    <http://man7.org/linux/man-pages/man2/sched_setscheduler.2.html>.
    This option only applies before creating any sockets on the _context_.

    ------------- --
    Default value -1
    ------------- --

*ZMQ_THREAD_PRIORITY*
  ~ *Set scheduling priority for I/O threads*.  The *ZMQ_THREAD_PRIORITY*
    argument sets scheduling priority for internal context's thread pool. This
    option is not available on Windows.  Supported values for this option
    depend on chosen scheduling policy.  Details can be found in the `sched.h`
    file, or at
    <http://man7.org/linux/man-pages/man2/sched_setscheduler.2.html>.
    This option only applies before creating any sockets on the _context_.

    ------------- --
    Default value -1
    ------------- --

*ZMQ_MAX_SOCKETS*
  ~ *Set maximum number of sockets*.  The *ZMQ_MAX_SOCKETS* argument sets the
    maximum number of sockets allowed on the _context_. You can query the
    maximal allowed value with [zmq_ctx_get][] using the *ZMQ_SOCKET_LIMIT*
    option.

    ------------- ----
    Default value 1024
    ------------- ----

*ZMQ_IPV6*
  ~ *Set IPv6 option*.  The *ZMQ_IPV6* argument sets the IPv6 value for all
    sockets created in the _context_ from this point onwards. A value of `1`
    means IPv6 is enabled, while `0` means the socket will use only IPv4. When
    IPv6 is enabled, a socket will connect to, or accept connections from, both
    IPv4 and IPv6 hosts.

    ------------- -
    Default value 0
    ------------- -


Return value
------------

The *zmq_ctx_set()* function returns zero if successful.  Otherwise it returns
`-1` and sets _errno_ to one of the values defined below.


Errors
------

*EINVAL*
  ~ The requested option _option_name_ is unknown.


Example
-------

### Setting a limit on the number of sockets

~~~{.example}
TYPE(C_PTR) :: context
INTEGER(KIND = C_INT) :: rc
INTEGER(KIND = C_INT) :: max_sockets

context = zmq_ctx_new()
rc = zmq_ctx_set(context, ZMQ_MAX_SOCKETS, 256)
max_sockets = zmq_ctx_get(context, ZMQ_MAX_SOCKETS)
~~~


See also
--------

[zmq_ctx_get][]
[fzmq][]
