% zmq_connect


Name
----

zmq_connect - create an outgoing connection from a socket


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_connect(socket, endpoint) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: socket
  CHARACTER(LEN = *), INTENT(IN) :: endpoint
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_connect()* function connects the _socket_ to an _endpoint_ and then
accepts incoming connections on that endpoint.

The _endpoint_ is a string consisting of a _`transport`_`://` followed by an
_address_.  The _transport_ specifies the underlying protocol to use. The
_address_ specifies the transport-specific address to connect to.

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

For most transports and socket types the connection is not performed
immediately but as needed by ØMQ.  Thus a successful call to *zmq_connect()*
does not mean that the connection was or could actually be established.
Because of this, for most transports and socket types the order in which a
_server_ socket is bound and a _client_ socket is connected to it does not
matter.  The first exception is when using the inproc:// transport: you must
call *zmq_bind()* before calling *zmq_connect()*.  The second exception are
*ZMQ_PAIR* sockets, which do not automatically reconnect to endpoints.

Following a *zmq_connect()*, for socket types except for *ZMQ_ROUTER*, the
socket enters its normal _ready_ state.  By contrast, following a *zmq_bind()*
alone, the socket enters a _mute_ state in which the socket blocks or drops
messages according to the socket type, as defined in [zmq_socket][].  A
*ZMQ_ROUTER* socket enters its normal _ready_ state for a specific peer only
when handshaking is complete for that peer, which may take an arbitrary time.


Return value
------------

The *zmq_connect()* function returns zero if successful.  Otherwise it returns
`-1` and sets _errno_ to one of the values defined below.


Errors
------

*EINVAL*
  ~ The endpoint supplied is invalid.

*EPROTONOSUPPORT*
  ~ The requested _transport_ protocol is not supported.

*ENOCOMPATPROTO*
  ~ The requested _transport_ protocol is not compatible with the socket type.

*ETERM*
  ~ The ØMQ _context_ associated with the specified _socket_ was terminated.

*ENOTSOCK*
  ~ The provided _socket_ was invalid.

*EMTHREAD*
  ~ No I/O thread is available to accomplish the task.


Example
-------

### Connecting a subscriber socket to an in-process and a TCP transport

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
INTEGER(KIND = C_INT) :: rc

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_SUB)
rc = zmq_connect(socket, 'inproc://my_publisher')
rc = zmq_connect(socket, 'tcp://server:5555')
~~~


See also
--------

[zmq_bind][]
[zmq_socket][]
[fzmq][]
