% zmq_ctx_term


Name
----

zmq_ctx_term - destroy a ØMQ context


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_ctx_term(context) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: context
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_ctx_term()* function shall destroy the ØMQ _context_.

Context termination is performed in the following steps:

1. Any blocking operations currently in progress on sockets open within the
   context shall return immediately with an error code of *ETERM*.  With the
   exception of *zmq_close()*, any further operations on sockets open within
   _context_ shall fail with an error code of *ETERM*.
2. After interrupting all blocking calls, *zmq_ctx_term()* shall _block_ until
   the following conditions are satisfied:
     * All sockets open within the context have been closed with *zmq_close()*.
     * For each socket within the context, all messages sent by the application
       with *zmq_send()* and *zmq_msg_send()* have either been physically
       transferred to a network peer, or the socket's linger period set with
       the *ZMQ_LINGER* socket option has expired.

For further details regarding socket linger behavior refer to the *ZMQ_LINGER*
option in [zmq_setsockopt][].

This function replaces the deprecated function [zmq_term][].


Warning
-------

As *ZMQ_LINGER* defaults to "infinite", by default this function will block
indefinitely if there are any pending connects or sends.  We strongly
recommend setting *ZMQ_LINGER* to zero on all sockets and closing all sockets
before calling this function.


Return value
------------

The *zmq_ctx_term()* function shall return zero if successful.  Otherwise
it shall return `-1` and set _errno_ to one of the values defined below.


Errors
------

*EFAULT*
  ~ The provided _context_ was invalid.

*EINTR*
  ~ Termination was interrupted by a signal.  It can be restarted if needed.


Example
-------

### Create and immediately terminate a context

~~~{.example}
TYPE(C_PTR) :: context
INTEGER(KIND = C_INT) :: rc

context = zmq_ctx_new()
rc = zmq_ctx_term()
~~~


See also
--------

[zmq_init][]
[zmq_close][]
[zmq_setsockopt][]
[fzmq][]
