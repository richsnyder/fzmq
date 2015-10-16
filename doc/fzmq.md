% fzmq


Name
----

fzmq - Fortran binding to the ØMQ lightweight messaging kernel


Synopsis
--------

~~~{.synopsis}
USE zmq
~~~

~~~{.synopsis}
fc [flags] files -lfzmq -lzmq [libraries]
~~~


Description
-----------

The ØMQ lightweight messaging kernel is a library which extends the standard
socket interfaces with features traditionally provided by specialised
_messaging middleware_ products.  ØMQ sockets provide an abstraction of
asynchronous _message queues_, multiple _messaging patterns_, message
filtering (_subscriptions_), seamless access to multiple _transport protocols_
and more.

This documentation presents an overview of ØMQ concepts, describes how ØMQ
abstracts standard sockets and provides a reference manual for the functions
provided by the Fortran binding to the ØMQ library.


### Context

Before using any ØMQ library functions you must create a ØMQ _context_.  When
you exit your application you must destroy the _context_.  These functions let
you work with _contexts_:

Create a new ØMQ context
  ~ [zmq_ctx_new][]

Work with context properties
  ~ [zmq_ctx_set][]
    [zmq_ctx_get][]

Destroy a ØMQ context
  ~ [zmq_ctx_shutdown][]
    [zmq_ctx_term][]

#### Thread safety

A ØMQ _context_ is thread safe and may be shared among as many application
threads as necessary, without any additional locking required on the part of
the caller.

Individual ØMQ _sockets_ are _not_ thread safe except in the case where full
memory barriers are issued when migrating a socket from one thread to another.


#### Multiple contexts

Multiple _contexts_ may coexist within a single application. Thus, an
application can use ØMQ directly and at the same time make use of any number of
additional libraries or components which themselves make use of ØMQ as long as
the above guidelines regarding thread safety are adhered to.


### Messages

A ØMQ message is a discrete unit of data passed between applications or
components of the same application.  ØMQ messages have no internal structure
and from the point of view of ØMQ itself they are considered to be opaque
binary data.

The following functions are provided to work with messages:

Initialise a message
  ~ [zmq_msg_init][]
    [zmq_msg_init_size][]
    [zmq_msg_init_data][]

Sending and receiving a message
  ~ [zmq_msg_send][]
    [zmq_msg_recv][]

Release a message
  ~ [zmq_msg_close][]

Access message content
  ~ [zmq_msg_data][]
    [zmq_msg_size][]
    [zmq_msg_more][]

Work with message properties
  ~ [zmq_msg_gets][]
    [zmq_msg_get][]
    [zmq_msg_set][]

Message manipulation
  ~ [zmq_msg_copy][]
    [zmq_msg_move][]


### Sockets

ØMQ sockets present an abstraction of a asynchronous _message queue_, with the
exact queueing semantics depending on the socket type in use. See
[zmq_socket][] for the socket types provided.

The following functions are provided to work with sockets:

Creating a socket
  ~ [zmq_socket][]

Closing a socket
  ~ [zmq_close][]

Manipulating socket options
  ~ [zmq_getsockopt][]
    [zmq_setsockopt][]

Establishing a message flow
  ~ [zmq_bind][]
    [zmq_connect][]

Sending and receiving messages
  ~ [zmq_msg_send][]
    [zmq_msg_recv][]
    [zmq_send][]
    [zmq_recv][]
    [zmq_send_const][]

Monitoring socket events
  ~ [zmq_socket_monitor][]

#### Input/output multiplexing

ØMQ provides a mechanism for applications to multiplex input/output events over
a set containing both ØMQ sockets and standard sockets.  This mechanism mirrors
the standard *poll()* system call, and is described in detail in [zmq_poll][].


### Transports

A ØMQ socket can use multiple different underlying transport mechanisms.  Each
transport mechanism is suited to a particular purpose and has its own
advantages and drawbacks.

The following transport mechanisms are provided:

Unicast transport using TCP
  ~ [zmq_tcp][]

Reliable multicast transport using PGM
  ~ [zmq_pgm][]

Local inter-process communication transport
  ~ [zmq_ipc][]

Local in-process (inter-thread) communication transport
  ~ [zmq_inproc][]

Unicast transport for use on clusters
  ~ [zmq_tipc][]


### Proxies

ØMQ provides _proxies_ to create fanout and fan-in topologies.  A proxy
connects a _frontend_ socket to a _backend_ socket and switches all messages
between the two sockets, opaquely.  A proxy may optionally capture all traffic
to a third socket.  To start a proxy in an application thread, use
[zmq_proxy][].


### Security

A ØMQ socket can select a security mechanism.  Both peers must use the same
security mechanism.

The following security mechanisms are provided for IPC and TCP connections:

Null security
  ~ [zmq_null][]

Plain-text authentication using username and password
  ~ [zmq_plain][]

Elliptic curve authentication and encryption
  ~ [zmq_curve][]

Generate a CURVE keypair in armored text format
  ~ [zmq_curve_keypair][]

Convert an armored key into a 32-byte binary key
  ~ [zmq_z85_decode][]

Convert a 32-byte binary CURVE key to an armored text string
  ~ [zmq_z85_encode][]


Error handling
--------------

The ØMQ library functions handle errors using the standard conventions found on
POSIX systems.  Generally, this means that upon failure a ØMQ library function
shall return either a *C_NULL_PTR* value (if returning a pointer) or a negative
value (if returning an integer), and the actual error code shall be stored in
the _errno_ variable.  The *zmq_errno()* function is provided to retrieve the
value of the _errno_ variable and the *zmq_strerror()* function is provided to
translate error codes into error message strings.

The following error-related functions are provided:

Error handling
  ~ [zmq_errno][]
    [zmq_strerror][]


Miscellaneous
-------------

The following miscellaneous functions are provided:

Report ØMQ library version
  ~ [zmq_version][]


Resources
---------

FZMQ web site: TBD

Main ØMQ web site: <http://www.zeromq.org/>


Copying
-------

Free use of this software is granted under the terms of the Mozilla Public
License, v. 2.0 (MPL v2).   Free use of ØMQ is granted under the terms of the
GNU Lesser General Public License (LGPL).  For details, see the `COPYING` file
included with the FZMQ distribution.  
