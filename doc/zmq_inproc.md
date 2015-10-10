% zmq_inproc


Name
----

zmq_inproc - ØMQ local in-process (inter-thread) communication transport


Synopsis
--------

The in-process transport passes messages via memory directly between threads
sharing a single ØMQ _context_.

> No I/O threads are involved in passing messages using the _inproc_
> transport. Therefore, if you are using a ØMQ _context_ for in-process
> messaging only you can initialise the _context_ with zero I/O threads.
> See [zmq_ctx_set][] for details.


Addressing
----------

A ØMQ endpoint is a string consisting of a _transport_`://` followed by an
_address_. The _transport_ specifies the underlying protocol to use. The
_address_ specifies the transport-specific address to connect to.

For the in-process transport, the transport is `inproc`, and the meaning of
the _address_ part is defined below.

### Assigning a local address to a socket

When assigning a local address to a _socket_ using *zmq_bind()* with the
_inproc_ transport, the _endpoint_ shall be interpreted as an arbitrary string
identifying the _name_ to create. The _name_ must be unique within the ØMQ
_context_ associated with the _socket_ and may be up to 256 characters in
length.  No other restrictions are placed on the format of the _name_.

### Connecting a socket

When connecting a _socket_ to a peer address using *zmq_connect()* with the
_inproc_ transport, the _endpoint_ shall be interpreted as an arbitrary string
identifying the _name_ to connect to.  The _name_ must have been previously
created by assigning it to at least one _socket_ within the same ØMQ _context_
as the _socket_ being connected.


Examples
--------

### Assigning a local address to a socket

~~~{.example}
rc = zmq_bind(socket, 'inproc://#1')
rc = zmq_bind(socket, 'inproc://my-endpoint')
~~~

### Connecting a socket

~~~{.example}
rc = zmq_connect(socket, 'inproc://#1')
rc = zmq_connect(socket, 'inproc://my-endpoint')
~~~


See also
--------

[zmq_ctx_set][]
[zmq_bind][]
[zmq_connect][]
[zmq_ipc][]
[zmq_pgm][]
[zmq_tcp][]
