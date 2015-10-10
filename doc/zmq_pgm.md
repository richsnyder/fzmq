% zmq_pgm


Name
----

zmq_pgm - ØMQ reliable multicast transport using PGM


Synopsis
--------

PGM (Pragmatic General Multicast) is a protocol for reliable multicast
transport of data over IP networks.


Description
-----------

ØMQ implements two variants of PGM, the standard protocol where PGM datagrams
are layered directly on top of IP datagrams as defined by RFC 3208 (the _pgm_
transport) and "Encapsulated PGM" or EPGM where PGM datagrams are encapsulated
inside UDP datagrams (the _epgm_ transport).

The _pgm_ and _epgm_ transports can only be used with the _ZMQ_PUB_ and
_ZMQ_SUB_ socket types.

Further, PGM sockets are rate limited by default. For details, refer to the
_ZMQ_RATE_, and _ZMQ_RECOVERY_IVL_ options documented in [zmq_setsockopt][].

The _pgm_ transport implementation requires access to raw IP sockets.
Additional privileges may be required on some operating systems for this
operation. Applications not requiring direct interoperability with other PGM
implementations are encouraged to use the _epgm_ transport instead which does
not require any special privileges.


ADDRESSING
----------

A ØMQ endpoint is a string consisting of a _transport_`://` followed by an
_address_. The _transport_ specifies the underlying protocol to use. The
_address_ specifies the transport-specific address to connect to.

For the PGM transport, the transport is `pgm`, and for the EPGM protocol the
transport is `epgm`. The meaning of the _address_ part is defined below.

### Connecting a socket

When connecting a socket to a peer address using *zmq_connect()* with the _pgm_
or _epgm_ transport, the _endpoint_ shall be interpreted as an _interface_
followed by a semicolon, followed by a _multicast address_, followed by a colon
and a port number.

An _interface_ may be specified by either of the following:

* The interface name as defined by the operating system.
* The primary IPv4 address assigned to the interface, in its numeric
  representation.

> Interface names are not standardised in any way and should be assumed to
> be arbitrary and platform dependent. On Win32 platforms no short interface
> names exist, thus only the primary IPv4 address may be used to specify an
> _interface_. The _interface_ part can be omitted, in that case the default
> one will be selected.

A _multicast address_ is specified by an IPv4 multicast address in its numeric
representation.


WIRE FORMAT
-----------

Consecutive PGM datagrams are interpreted by ØMQ as a single continuous stream
of data where ØMQ messages are not necessarily aligned with PGM datagram
boundaries and a single ØMQ message may span several PGM datagrams. This stream
of data consists of ØMQ messages encapsulated in _frames_ as described in
[zmq_tcp][].

### PGM datagram payload

The following ABNF grammar represents the payload of a single PGM datagram as
used by ØMQ:

~~~
datagram               = (offset data)
offset                 = 2OCTET
data                   = *OCTET
~~~

In order for late joining consumers to be able to identify message boundaries,
each PGM datagram payload starts with a 16-bit unsigned integer in network byte
order specifying either the offset of the first message _frame_ in the datagram
or containing the value `0xFFFF` if the datagram contains solely an
intermediate part of a larger message.

Note that offset specifies where the first message begins rather than the first
message part. Thus, if there are trailing message parts at the beginning of
the packet the offset ignores them and points to first initial message part
in the packet.

The following diagram illustrates the layout of a single PGM datagram payload:

~~~
+------------------+----------------------+
| offset (16 bits) |         data         |
+------------------+----------------------+
~~~

The following diagram further illustrates how three example ØMQ frames are laid
out in consecutive PGM datagram payloads:

~~~
First datagram payload
+--------------+-------------+---------------------+
| Frame offset |   Frame 1   |   Frame 2, part 1   |
|    0x0000    | (Message 1) | (Message 2, part 1) |
+--------------+-------------+---------------------+

Second datagram payload
+--------------+---------------------+
| Frame offset |   Frame 2, part 2   |
| 0xFFFF       | (Message 2, part 2) |
+--------------+---------------------+

Third datagram payload
+--------------+----------------------------+-------------+
| Frame offset |   Frame 2, final 8 bytes   |   Frame 3   |
| 0x0008       | (Message 2, final 8 bytes) | (Message 3) |
+--------------+----------------------------+-------------+
~~~


Example
-------

### Connecting a socket

~~~{.example}
rc = zmq_connect(socket, 'epgm://eth0;239.192.1.1:5555')
rc = zmq_connect(socket, 'pgm://192.168.1.1;239.192.1.1:5555')
~~~


See also
--------

[zmq_connect][]
[zmq_setsockopt][]
[zmq_tcp][]
[zmq_ipc][]
[zmq_inproc][]
