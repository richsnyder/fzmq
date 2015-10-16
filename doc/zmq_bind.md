% zmq_bind


Name
----

zmq_bind - accept incoming connections on a socket


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_bind(socket, endpoint) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: socket
  CHARACTER(LEN = *), INTENT(IN) :: endpoint
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_bind()* function binds the _socket_ to a local _endpoint_ and then
accepts incoming connections on that endpoint.

The _endpoint_ is a string consisting of a _`transport`_`://` followed by an
_address_.  The _transport_ specifies the underlying protocol to use.  The
_address_ specifies the transport-specific address to bind to.

ØMQ provides the the following transports:

_tcp_
  ~ unicast transport using TCP, see [zmq_tcp][]

_ipc_
  ~ local inter-process communication transport, see [zmq_ipc][]

_inproc_
  ~ local in-process (inter-thread) communication transport, see [zmq_inproc][]

_pgm_, _epgm_
  ~ reliable multicast transport using PGM, see [zmq_pgm][]

_tipc_
  ~ unicast transport for use on clusters, see [zmq_tipc][]

Every ØMQ socket type except *ZMQ_PAIR* supports one-to-many and many-to-one
semantics.  The precise semantics depend on the socket type and are defined in
[zmq_socket][].

The _ipc_ and _tcp_ transports accept wildcard addresses: see [zmq_ipc][]
and [zmq_tcp][] for details.

The address syntax may be different for *zmq_bind()* and *zmq_connect()*
especially for the _tcp_, _pgm_ and _epgm_ transports.

Following a *zmq_bind()*, the socket enters a _mute_ state unless or until at
least one incoming or outgoing connection is made, at which point the socket
enters a _ready_ state. In the mute state, the socket blocks or drops messages
according to the socket type, as defined in [zmq_socket][].  By contrast,
following a [zmq_connect][], the socket enters the _ready_ state.


Return value
------------

The *zmq_bind()* function returns zero if successful.  Otherwise it returns
`-1` and sets _errno_ to one of the values defined below.


Errors
------

*EINVAL*
  ~ The endpoint supplied is invalid.

*EPROTONOSUPPORT*
  ~ The requested _transport_ protocol is not supported.

*ENOCOMPATPROTO*
  ~ The requested _transport_ protocol is not compatible with the socket type.

*EADDRINUSE*
  ~ The requested _address_ is already in use.

*EADDRNOTAVAIL*
  ~ The requested _address_ was not local.

*ENODEV*
  ~ The requested _address_ specifies a nonexistent interface.

*ETERM*
  ~ The ØMQ _context_ associated with the specified _socket_ was terminated.

*ENOTSOCK*
  ~ The provided _socket_ was invalid.

*EMTHREAD*
  ~ No I/O thread is available to accomplish the task.


Example
-------

### Binding a publisher socket to an in-process and a TCP transport

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
INTEGER(KIND = C_INT) :: rc

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_PUB)
rc = zmq_bind(socket, 'inproc://my_publisher')
rc = zmq_bind(socket, 'tcp://*:5555')
~~~


See also
--------

[zmq_connect][]
[zmq_socket][]
[zmq_unbind][]
[fzmq][]
