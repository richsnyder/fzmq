% zmq_errno


Name
----

zmq_errno - retrieve value of errno for the calling thread


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


See also
--------

[zmq_strerror][]
