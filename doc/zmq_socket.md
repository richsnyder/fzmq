% zmq_socket


Name
----

zmq_socket - create a ØMQ socket


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_socket(context, type_) RESULT(code)

	TYPE(C_PTR), INTENT(IN) :: context
	INTEGER(KIND = C_INT), INTENT(IN) :: type_
	TYPE(C_PTR) :: code
~~~


Description
-----------

The *zmq_socket()* function shall create a ØMQ socket within the specified
_context_ and return an opaque handle to the newly created socket.  The *type_*
argument specifies the socket type, which determines the semantics of
communication over the socket.

The newly created socket is initially unbound, and not associated with any
endpoints.  In order to establish a message flow a socket must first be
connected to at least one endpoint with [zmq_connect][], or at least one
endpoint must be created for accepting incoming connections with [zmq_bind][].

### Key differences to conventional sockets

Generally speaking, conventional sockets present a _synchronous_ interface to
either connection-oriented reliable byte streams (*SOCK_STREAM*), or
connectionless unreliable datagrams (*SOCK_DGRAM*).  In comparison, ØMQ sockets
present an abstraction of an asynchronous _message queue_, with the exact
queueing semantics depending on the socket type in use.  Where conventional
sockets transfer streams of bytes or discrete datagrams, ØMQ sockets transfer
discrete _messages_.

ØMQ sockets being _asynchronous_ means that the timings of the physical
connection setup and tear down, reconnect and effective delivery are
transparent to the user and organized by ØMQ itself.  Further, messages may be
_queued_ in the event that a peer is unavailable to receive them.

Conventional sockets allow only strict one-to-one (two peers), many-to-one
(many clients, one server), or in some cases one-to-many (multicast)
relationships.  With the exception of *ZMQ_PAIR*, ØMQ sockets may be connected
_to multiple endpoints_ using *zmq_connect()*, while simultaneously accepting
incoming connections _from multiple endpoints_ bound to the socket using
*zmq_bind()*, thus allowing many-to-many relationships.

### Thread safety

ØMQ _sockets_ are _not_ thread safe.  Applications MUST NOT use a socket from
multiple threads except after migrating a socket from one thread to another
with a "full fence" memory barrier.

### Socket types

The following sections present the socket types defined by ØMQ, grouped by the
general _messaging pattern_ which is built from related socket types.


Request-reply pattern
---------------------

The request-reply pattern is used for sending requests from a *ZMQ_REQ*
_client_ to one or more *ZMQ_REP* _services_, and receiving subsequent replies
to each request sent.

The request-reply pattern is formally defined by
<http://rfc.zeromq.org/spec:28>.

### ZMQ_REQ

A socket of type *ZMQ_REQ* is used by a _client_ to send requests to and
receive replies from a _service_.  This socket type allows only an alternating
sequence of *zmq_send(request)* and subsequent *zmq_recv(reply)* calls.  Each
request sent is round-robined among all _services_, and each reply received is
matched with the last issued request.

If no services are available, then any send operation on the socket shall block
until at least one _service_ becomes available.  The REQ socket shall not
discard messages.

------------------------- -------------------------------------
Compatible peer sockets   *ZMQ_REP*, *ZMQ_ROUTER*
Direction                 Bidirectional
Send/receive pattern      Send, Receive, Send, Receive, ...
Outgoing routing strategy Round-robin
Incoming routing strategy Last peer
Action in mute state      Block
------------------------- -------------------------------------

### ZMQ_REP

A socket of type *ZMQ_REP* is used by a _service_ to receive requests from and
send replies to a _client_.  This socket type allows only an alternating
sequence of *zmq_recv(request)* and subsequent *zmq_send(reply)* calls.  Each
request received is fair-queued from among all _clients_, and each reply sent
is routed to the _client_ that issued the last request.  If the original
requester does not exist any more the reply is silently discarded.

------------------------- -------------------------------------
Compatible peer sockets   *ZMQ_REQ*, *ZMQ_DEALER*
Direction                 Bidirectional
Send/receive pattern      Receive, Send, Receive, Send, ...
Incoming routing strategy Fair-queued
Outgoing routing strategy Last peer
------------------------- -------------------------------------

### ZMQ_DEALER

A socket of type *ZMQ_DEALER* is an advanced pattern used for extending
request/reply sockets.  Each message sent is round-robined among all connected
peers, and each message received is fair-queued from all connected peers.

When a *ZMQ_DEALER* socket enters the _mute_ state due to having reached the
high water mark for all peers, or if there are no peers at all, then any
[zmq_send][] operations on the socket shall block until the mute state ends or
at least one peer becomes available for sending; messages are not discarded.

When a *ZMQ_DEALER* socket is connected to a *ZMQ_REP* socket each message sent
must consist of an empty message part, the _delimiter_, followed by one or more
_body parts_.

------------------------- -------------------------------------
Compatible peer sockets   *ZMQ_ROUTER*, *ZMQ_REP*, *ZMQ_DEALER*
Direction                 Bidirectional
Send/receive pattern      Unrestricted
Outgoing routing strategy Round-robin
Incoming routing strategy Fair-queued
Action in mute state      Block
------------------------- -------------------------------------

### ZMQ_ROUTER

A socket of type *ZMQ_ROUTER* is an advanced socket type used for extending
request/reply sockets.  When receiving messages a *ZMQ_ROUTER* socket shall
prepend a message part containing the _identity_ of the originating peer to the
message before passing it to the application.  Messages received are
fair-queued from among all connected peers. When sending messages a
*ZMQ_ROUTER* socket shall remove the first part of the message and use it to
determine the _identity_ of the peer the message shall be routed to.  If the
peer does not exist anymore the message shall be silently discarded by default,
unless the *ZMQ_ROUTER_MANDATORY* socket option is set to `1`.

When a *ZMQ_ROUTER* socket enters the _mute_ state due to having reached the
high water mark for all peers, then any messages sent to the socket shall be dropped until the mute state ends.  Likewise, any messages routed to a peer for which the individual high water mark has been reached shall also be dropped.

When a *ZMQ_REQ* socket is connected to a *ZMQ_ROUTER* socket, in addition to
the _identity_ of the originating peer each message received shall contain an
empty _delimiter_ message part.  Hence, the entire structure of each received
message as seen by the application becomes: one or more _identity_ parts,
_delimiter_ part, one or more _body parts_.  When sending replies to a
*ZMQ_REQ* socket the application must include the _delimiter_ part.

------------------------- -------------------------------------
Compatible peer sockets   *ZMQ_DEALER*, *ZMQ_REQ*, *ZMQ_ROUTER*
Direction                 Bidirectional
Send/receive pattern      Unrestricted
Outgoing routing strategy See text
Incoming routing strategy Fair-queued
Action in mute state      Drop
------------------------- -------------------------------------


Publish-subscribe pattern
-------------------------

The publish-subscribe pattern is used for one-to-many distribution of data from
a single _publisher_ to multiple _subscribers_ in a fan out fashion.

The publish-subscribe pattern is formally defined by
<http://rfc.zeromq.org/spec:29>.

### ZMQ_PUB

A socket of type *ZMQ_PUB* is used by a _publisher_ to distribute data.
Messages sent are distributed in a fan out fashion to all connected peers.
The *zmq_msg_recv()* and *zmq_recv()* functions are not implemented for this
socket type.

When a *ZMQ_PUB* socket enters the _mute_ state due to having reached the
high water mark for a _subscriber_, then any messages that would be sent to the
_subscriber_ in question shall instead be dropped until the mute state
ends.  The *zmq_send()* function shall never block for this socket type.

------------------------- -------------------------------------
Compatible peer sockets   *ZMQ_SUB*, *ZMQ_XSUB*
Direction                 Unidirectional
Send/receive pattern      Send only
Incoming routing strategy N/A
Outgoing routing strategy Fan out
Action in mute state      Drop
------------------------- -------------------------------------

### ZMQ_SUB

A socket of type *ZMQ_SUB* is used by a _subscriber_ to subscribe to data
distributed by a _publisher_.  Initially a *ZMQ_SUB* socket is not subscribed
to any messages.  Use the *ZMQ_SUBSCRIBE* option of [zmq_setsockopt][] to
specify which messages to subscribe to.  The *zmq_msg_send()* and *zmq_send()*
functions are not implemented for this socket type.

------------------------- -------------------------------------
Compatible peer sockets   *ZMQ_PUB*, *ZMQ_XPUB*
Direction                 Unidirectional
Send/receive pattern      Receive only
Incoming routing strategy Fair-queued
Outgoing routing strategy N/A
------------------------- -------------------------------------

### ZMQ_XPUB

Same as *ZMQ_PUB* except that you can receive subscriptions from the peers
in form of incoming messages.  A subscription message is a byte `1` (for
subscriptions) or byte `0` (for unsubscriptions) followed by the subscription
body.  Messages without a sub/unsub prefix are also received, but have no
effect on subscription status.

------------------------- -------------------------------------
Compatible peer sockets   *ZMQ_SUB*, *ZMQ_XSUB*
Direction                 Unidirectional
Send/receive pattern      Send messages, receive subscriptions
Incoming routing strategy N/A
Outgoing routing strategy Fan out
Action in mute state      Drop
------------------------- -------------------------------------

### ZMQ_XSUB

Same as *ZMQ_SUB* except that you subscribe by sending subscription messages to
the socket.  A subscription message is a byte `1` (for subscriptions) or byte
`0` (for unsubscriptions) followed by the subscription body.  Messages without
a sub/unsub prefix may also be sent, but have no effect on subscription status.

------------------------- -------------------------------------
Compatible peer sockets   *ZMQ_PUB*, *ZMQ_XPUB*
Direction                 Unidirectional
Send/receive pattern      Receive messages, send subscriptions
Incoming routing strategy Fair-queued
Outgoing routing strategy N/A
Action in mute state      Drop
------------------------- -------------------------------------


Pipeline pattern
----------------

The pipeline pattern is used for distributing data to _nodes_ arranged in a
pipeline.  Data always flows down the pipeline, and each stage of the pipeline
is connected to at least one _node_.  When a pipeline stage is connected to
multiple _nodes_ data is round-robined among all connected _nodes_.

The pipeline pattern is formally defined by <http://rfc.zeromq.org/spec:30>.

### ZMQ_PUSH

A socket of type *ZMQ_PUSH* is used by a pipeline _node_ to send messages
to downstream pipeline _nodes_.  Messages are round-robined to all connected
downstream _nodes_.  The *zmq_msg_recv()* and *zmq_recv()* functions are not
implemented for this socket type.

When a *ZMQ_PUSH* socket enters the _mute_ state due to having reached the
high water mark for all downstream _nodes_, or if there are no downstream
_nodes_ at all, then any *zmq_msg_send()* or *zmq_send()* operations on the
socket shall block until the mute state ends or at least one downstream _node_
becomes available for sending; messages are not discarded.

------------------------- -------------------------------------
Compatible peer sockets   *ZMQ_PULL*
Direction                 Unidirectional
Send/receive pattern      Send only
Incoming routing strategy N/A
Outgoing routing strategy Round-robin
Action in mute state      Block
------------------------- -------------------------------------

### ZMQ_PULL

A socket of type *ZMQ_PULL* is used by a pipeline _node_ to receive messages
from upstream pipeline _nodes_.  Messages are fair-queued from among all
connected upstream _nodes_.  The *zmq_msg_send()* and *zmq_send()* functions
are not implemented for this socket type.

------------------------- -------------------------------------
Compatible peer sockets   *ZMQ_PUSH*
Direction                 Unidirectional
Send/receive pattern      Receive only
Incoming routing strategy Fair-queued
Outgoing routing strategy N/A
Action in mute state      Block
------------------------- -------------------------------------


Exclusive pair pattern
----------------------

The exclusive pair pattern is used to connect a peer to precisely one other
peer.  This pattern is used for inter-thread communication across the inproc
transport.

The exclusive pair pattern is formally defined by
<http://rfc.zeromq.org/spec:31>.

### ZMQ_PAIR

A socket of type *ZMQ_PAIR* can only be connected to a single peer at any one
time.  No message routing or filtering is performed on messages sent over a
*ZMQ_PAIR* socket.

When a *ZMQ_PAIR* socket enters the _mute_ state due to having reached the high
water mark for the connected peer, or if no peer is connected, then any
*zmq_msg_send()* or *zmq_send()* operations on the socket shall block until the
peer becomes available for sending; messages are not discarded.

*ZMQ_PAIR* sockets are designed for inter-thread communication across the
[zmq_inproc][] transport and do not implement functionality such as
auto-reconnection.  *ZMQ_PAIR* sockets are considered experimental and may
have other missing or broken aspects.

------------------------- -------------------------------------
Compatible peer sockets   *ZMQ_PAIR*
Direction                 Bidirectional
Send/receive pattern      Unrestricted
Incoming routing strategy N/A
Outgoing routing strategy N/A
Action in mute state      Block
------------------------- -------------------------------------


Native Pattern
--------------

The native pattern is used for communicating with TCP peers and allows
asynchronous requests and replies in either direction.

### ZMQ_STREAM

A socket of type *ZMQ_STREAM* is used to send and receive TCP data from a
non-ØMQ peer, when using the tcp:// transport.  A *ZMQ_STREAM* socket can
act as client and/or server, sending and/or receiving TCP data asynchronously.

When receiving TCP data, a *ZMQ_STREAM* socket shall prepend a message part
containing the _identity_ of the originating peer to the message before passing
it to the application.  Messages received are fair-queued from among all
connected peers.

When sending TCP data, a *ZMQ_STREAM* socket shall remove the first part of the
message and use it to determine the _identity_ of the peer the message shall be
routed to, and unroutable messages shall cause an *EHOSTUNREACH* or *EAGAIN*
error.

To open a connection to a server, use the *zmq_connect()* call, and then fetch
the socket identity using the *ZMQ_IDENTITY* *zmq_getsockopt()* call.

To close a specific connection, send the identity frame followed by a
zero-length message.

When a connection is made, a zero-length message will be received by the
application.  Similarly, when the peer disconnects (or the connection is lost),
a zero-length message will be received by the application.

The *ZMQ_SNDMORE* flag is ignored on data frames.  You must send one identity
frame followed by one data frame.

Also, please note that omitting the *ZMQ_SNDMORE* flag will prevent sending
further data (from any client) on the same socket.

------------------------- -------------------------------------
Compatible peer sockets   None
Direction                 Bidirectional
Send/receive pattern      Unrestricted
Outgoing routing strategy See text
Incoming routing strategy Fair-queued
Action in mute state      *EAGAIN*
------------------------- -------------------------------------


Return value
------------

The *zmq_socket()* function shall return an opaque handle to the newly created
socket if successful.  Otherwise, it shall return *C_NULL_PTR* and set _errno_
to one of the values defined below.


Errors
------

*EINVAL*
	~ The requested socket _type_ is invalid.

*EFAULT*
	~ The provided _context_ is invalid.

*EMFILE*
	~ The limit on the total number of open ØMQ sockets has been reached.

*ETERM*
	~ The context specified was terminated.


Example
-------

### Creating a simple HTTP server using ZMQ_STREAM

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
INTEGER(KIND = C_INT) :: rc
INTEGER(KIND = C_INT) :: nbytes
INTEGER(KIND = C_SIZE_T) :: id_size
INTEGER(KIND = C_SIZE_T) :: response_size
CHARACTER(KIND = C_CHAR, LEN = 256), TARGET :: id
CHARACTER(KIND = C_CHAR, LEN = 256), TARGET :: raw
CHARACTER(KIND = C_CHAR, LEN = 256), TARGET :: response

response = 'HTTP/1.0 200 OK' // CHAR(13) // CHAR(10) // &
					 'Content-Type: text/plain' // CHAR(13) // CHAR(10) // &
					 CHAR(13) // CHAR(10) // &
					 'Hello, world!'
response_size = LEN(TRIM(response), KIND = C_SIZE_T)

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_STREAM)
rc = zmq_bind(socket, 'tcp://*:8080')

DO WHILE (.TRUE.)
	DO WHILE (.TRUE.)
    id_size = zmq_recv(socket, C_LOC(id), 256_C_SIZE_T, 0)
		nbytes = zmq_recv(socket, C_LOC(raw), 256_C_SIZE_T, 0)
		IF (nbytes >= 3 .AND. raw(1:3) == 'GET') THEN
			EXIT
		END IF
	END DO
	nbytes = zmq_send(socket, C_LOC(id), id_size, ZMQ_SNDMORE)
	nbytes = zmq_send(socket, C_LOC(response), response_size, ZMQ_SNDMORE)
	nbytes = zmq_send(socket, C_LOC(id), id_size, ZMQ_SNDMORE)
	nbytes = zmq_send(socket, C_NULL_PTR, 0_C_SIZE_T, ZMQ_SNDMORE)
END DO

rc = zmq_close(socket)
rc = zmq_ctx_term(context)
~~~


See also
--------

[zmq_bind][]
[zmq_connect][]
[zmq_msg_send][]
[zmq_msg_recv][]
[zmq_send][]
[zmq_recv][]
[zmq_setsockopt][]
[zmq_inproc][]
[fzmq][]
