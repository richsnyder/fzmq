% zmq_version


Name
----

zmq_version - report the ØMQ library version


Synopsis
--------

~~~{.synopsis}
SUBROUTINE zmq_version(major, minor, patch)

  INTEGER(KIND = C_INT), INTENT(OUT) :: major
  INTEGER(KIND = C_INT), INTENT(OUT) :: minor
  INTEGER(KIND = C_INT), INTENT(OUT) :: patch
~~~


Description
-----------

The *zmq_version()* function shall fill in the integer variables pointed to by
the _major_, _minor_ and _patch_ arguments with the major, minor and patch
level components of the ØMQ library version.

This functionality is intended for applications or language bindings
dynamically linking to the ØMQ library that wish to determine the actual
version of the ØMQ library they are using.


Return value
------------

There is no return value.


Errors
------

No errors are defined.


Example
-------

### Getting the version of the ØMQ library

~~~{.example}
INTEGER(KIND = C_INT) :: major, minor, patch

CALL zmq_version(major, minor, patch)
~~~
