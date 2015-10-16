% zmq_errno


Name
----

zmq_errno - retrieve the value of errno for the calling thread


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_errno() RESULT(errno)

  INTEGER(KIND = C_INT) :: errno
~~~


Description
-----------

The *zmq_errno()* function shall retrieve the value of the _errno_ variable for
the calling thread.


Return value
------------

The *zmq_errno()* function shall return the value of the _errno_ variable for
the calling thread.


Errors
------

No errors are defined.


Example
-------

### Check the return code for a bind operation

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
INTEGER(KIND = C_INT) :: rc

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_PUB)
rc = zmq_bind(socket, 'inproc://my_publisher')
IF (rc == -1) THEN
  PRINT*, 'Error: ', zmq_strerror(zmq_errno())
END IF
~~~

See also
--------

[zmq_strerror][]
[fzmq][]
