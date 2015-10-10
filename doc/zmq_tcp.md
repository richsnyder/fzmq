% zmq_tcp


Name
----

zmq_tcp - ØMQ unicast transport using TCP


Synopsis
--------

TCP is an ubiquitous, reliable, unicast transport. When connecting distributed
applications over a network with ØMQ, using the TCP transport will likely be
your first choice.


Addressing
----------

A ØMQ endpoint is a string consisting of a _transport_`://` followed by an
_address_. The _transport_ specifies the underlying protocol to use. The
_address_ specifies the transport-specific address to connect to.

For the TCP transport, the transport is `tcp`, and the meaning of the
_address_ part is defined below.

### Assigning a local address to a socket

When assigning a local address to a socket using *zmq_bind()* with the _tcp_
transport, the _endpoint_ shall be interpreted as an _interface_ followed by a
colon and the TCP port number to use.

An _interface_ may be specified by either of the following:

* The wild-card `*`, meaning all available interfaces.
* The primary IPv4 or IPv6 address assigned to the interface, in its numeric
  representation.
* The non-portable interface name as defined by the operating system.

The TCP port number may be specified by:

* A numeric value, usually above 1024 on POSIX systems.
* The wild-card `*`, meaning a system-assigned ephemeral port.

When using ephemeral ports, the caller should retrieve the actual assigned
port using the _ZMQ_LAST_ENDPOINT_ socket option. See [zmq_getsockopt][] for
details.

### Unbinding wild-card addres from a socket

When wild-card `*` _endpoint_ was used in *zmq_bind()*, the caller should use
real _endpoint_ obtained from the _ZMQ_LAST_ENDPOINT_ socket option to unbind
this _endpoint_ from a socket using *zmq_unbind()*.

### Connecting a socket

When connecting a socket to a peer address using *zmq_connect()* with the _tcp_
transport, the _endpoint_ shall be interpreted as a _peer address_ followed by
a colon and the TCP port number to use.  You can optionally specify a
_source_endpoint_ which will be used as the source address for your connection;
tcp://_source_endpoint_;_endpoint_, see the _interface_ description above for
details.

A _peer address_ may be specified by either of the following:

* The DNS name of the peer.
* The IPv4 or IPv6 address of the peer, in its numeric representation.

> A description of the ØMQ Message Transport Protocol (ZMTP) which is used by
> the TCP transport can be found at <http://rfc.zeromq.org/spec:15>


Examples
--------

### Assigning a local address to a socket

~~~{.example}
rc = zmq_bind(socket, 'tcp://*:5555')
rc = zmq_bind(socket, 'tcp://127.0.0.1:5555')
rc = zmq_bind(socket, 'tcp://eth0:5555')
~~~

### Connecting a socket

~~~{.example}
rc = zmq_connect(socket, 'tcp://192.168.1.1:5555')
rc = zmq_connect(socket, 'tcp://server1:5555')
rc = zmq_connect(socket, 'tcp://eth1:0;server1:5555')
rc = zmq_connect(socket, 'tcp://192.168.1.17:5555;192.168.1.1:5555')
~~~


See also
--------

[zmq_bind][]
[zmq_connect][]
[zmq_pgm][]
[zmq_ipc][]
[zmq_inproc][]
