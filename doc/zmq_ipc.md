% zmq_ipc


Name
----

zmq_ipc - ØMQ local inter-process communication transport


Synopsis
--------

The inter-process transport passes messages between local processes using a
system-dependent IPC mechanism.

> The inter-process transport is currently only implemented on operating
> systems that provide UNIX domain sockets.


Addressing
----------

A ØMQ endpoint is a string consisting of a _transport_`://` followed by an
_address_. The _transport_ specifies the underlying protocol to use. The
_address_ specifies the transport-specific address to connect to.

For the inter-process transport, the transport is `ipc`, and the meaning of
the _address_ part is defined below.

### Binding a socket

When binding a _socket_ to a local address using *zmq_bind()* with the _ipc_
transport, the _endpoint_ shall be interpreted as an arbitrary string
identifying the _pathname_ to create. The _pathname_ must be unique within the
operating system namespace used by the _ipc_ implementation, and must fulfill
any restrictions placed by the operating system on the format and length of a
_pathname_.

When the address is `*`, *zmq_bind()* shall generate a unique temporary
pathname. The caller should retrieve this pathname using the
_ZMQ_LAST_ENDPOINT_ socket option. See [zmq_getsockopt][] for details.

> Any existing binding to the same endpoint shall be overridden. That is,
> if a second process binds to an endpoint already bound by a process, this
> will succeed and the first process will lose its binding. In this behavior,
> the _ipc_ transport is not consistent with the _tcp_ or _inproc_ transports.

> The endpoint pathname must be writable by the process. When the endpoint
> starts with `/`, e.g., `ipc:///pathname`, this will be an _absolute_
> pathname.  If the endpoint specifies a directory that does not exist, the
> bind shall fail.

> On Linux only, when the endpoint pathname starts with `@`, the abstract
> namespace shall be used.  The abstract namespace is independent of the
> filesystem and if a process attempts to bind an endpoint already bound by a
> process, it will fail.  See _unix(7)_ for details.

> IPC pathnames have a maximum size that depends on the operating system.
> On Linux, the maximum is 113 characters including the "ipc://" prefix (107
> characters for the real path name).

### Unbinding wild-card address from a socket

When wild-card `*` _endpoint_ was used in *zmq_bind()*, the caller should use
real _endpoint_ obtained from the _ZMQ_LAST_ENDPOINT_ socket option to unbind
this _endpoint_ from a socket using *zmq_unbind()*.

### Connecting a socket

When connecting a _socket_ to a peer address using _zmq_connect()_ with the
_ipc_ transport, the _endpoint_ shall be interpreted as an arbitrary string
identifying the _pathname_ to connect to.  The _pathname_ must have been
previously created within the operating system namespace by assigning it to a
_socket_ with *zmq_bind()*.


Examples
--------

### Assigning a local address to a socket

~~~{.example}
rc = zmq_bind(socket, 'ipc:///tmp/feeds/0')
~~~

### Connecting a socket

~~~{.example}
rc = zmq_connect(socket, 'ipc:///tmp/feeds/0')
~~~


See also
--------

[zmq_bind][]
[zmq_connect][]
[zmq_getsockopt][]
[zmq_inproc][]
[zmq_pgm][]
[zmq_tcp][]
