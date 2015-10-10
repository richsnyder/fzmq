% zmq_has


Name
----

zmq_has - check a ØMQ capability


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_has(capability) RESULT(code)

  CHARACTER(LEN = *), INTENT(IN) :: capability
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_has()* function shall report whether a specified capability is
available in the library. This allows bindings and applications to probe
a library directly, for transport and security options.

Capabilities shall be lowercase strings. The following capabilities are
defined:

* ipc - the library supports the ipc:// protocol
* pgm - the library supports the pgm:// protocol
* tipc - the library supports the tipc:// protocol
* norm - the library supports the norm:// protocol
* curve - the library supports the CURVE security mechanism
* gssapi - the library supports the GSSAPI security mechanism


Return value
------------

The *zmq_has()* function shall return `1` if the specified capability is
provided. Otherwise it shall return `0`.
