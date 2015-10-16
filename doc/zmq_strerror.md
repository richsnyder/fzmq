% zmq_strerror


Name
----

zmq_strerror - get a ØMQ error message string


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_strerror(errnum) RESULT(message)

  INTEGER(KIND = C_INT), INTENT(IN) :: errnum
  CHARACTER(LEN = :), ALLOCATABLE :: message
~~~


Description
-----------

The *zmq_strerror()* function shall return a pointer to an error message string
corresponding to the error number specified by the _errnum_ argument.


Return value
------------

The *zmq_strerror()* function shall return a pointer to an error message
string.


Errors
------

No errors are defined.


Example
-------

### Displaying an error message when a ØMQ socket cannot be connected

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
INTEGER(KIND = C_INT) :: rc

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_REQ)
rc = zmq_connect(socket, 'tcp://127.0.0.1:5555')
IF (rc < 0) THEN
  PRINT*, 'ERROR: ', zmq_strerror(zmq_errno())
END IF

rc = zmq_close(socket)
rc = zmq_ctx_term(context)
~~~
