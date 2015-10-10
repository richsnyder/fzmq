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
_context_ and return an opaque handle to the newly created socket. The _type_
argument specifies the socket type, which determines the semantics of
communication over the socket.

The newly created socket is initially unbound, and not associated with any
endpoints. In order to establish a message flow a socket must first be
connected to at least one endpoint with [zmq_connect][], or at least one
endpoint must be created for accepting incoming connections with [zmq_bind][].

### Key differences to conventional sockets

Generally speaking, conventional sockets present a _synchronous_ interface to
either connection-oriented reliable byte streams (SOCK_STREAM), or
connection-less unreliable datagrams (SOCK_DGRAM). In comparison, ØMQ sockets
present an abstraction of an asynchronous _message queue_, with the exact
queueing semantics depending on the socket type in use. Where conventional
sockets transfer streams of bytes or discrete datagrams, ØMQ sockets transfer
discrete _messages_.

ØMQ sockets being _asynchronous_ means that the timings of the physical
connection setup and tear down, reconnect and effective delivery are transparent
to the user and organized by ØMQ itself. Further, messages may be _queued_ in
the event that a peer is unavailable to receive them.

Conventional sockets allow only strict one-to-one (two peers), many-to-one
(many clients, one server), or in some cases one-to-many (multicast)
relationships. With the exception of _ZMQ_PAIR_, ØMQ sockets may be connected
_to multiple endpoints_ using *zmq_connect()*, while simultaneously accepting
incoming connections _from multiple endpoints_ bound to the socket using
*zmq_bind()*, thus allowing many-to-many relationships.

#### Thread safety

ØMQ _sockets_ are _not_ thread safe. Applications MUST NOT use a socket
from multiple threads except after migrating a socket from one thread to
another with a "full fence" memory barrier.

#### Socket types

The following sections present the socket types defined by ØMQ, grouped by the
general _messaging pattern_ which is built from related socket types.


Request-reply pattern
---------------------

The request-reply pattern is used for sending requests from a _ZMQ_REQ_
_client_ to one or more _ZMQ_REP_ _services_, and receiving subsequent replies
to each request sent.

The request-reply pattern is formally defined by
<http://rfc.zeromq.org/spec:28>.

### ZMQ_REQ

A socket of type _ZMQ_REQ_ is used by a _client_ to send requests to and
receive replies from a _service_. This socket type allows only an alternating
sequence of *zmq_send(request)* and subsequent *zmq_recv(reply)* calls. Each
request sent is round-robined among all _services_, and each reply received is
matched with the last issued request.

If no services are available, then any send operation on the socket shall
block until at least one _service_ becomes available. The REQ socket shall
not discard messages.

Summary of ZMQ_REQ characteristics
------------------------- -------------------------------------
Compatible peer sockets   _ZMQ_REP_, _ZMQ_ROUTER_
Direction                 Bidirectional
Send/receive pattern      Send, Receive, Send, Receive, ...
Outgoing routing strategy Round-robin
Incoming routing strategy Last peer
Action in mute state      Block
------------------------- -------------------------------------

### ZMQ_REP

A socket of type _ZMQ_REP_ is used by a _service_ to receive requests from and
send replies to a _client_. This socket type allows only an alternating
sequence of *zmq_recv(request)* and subsequent *zmq_send(reply)* calls. Each
request received is fair-queued from among all _clients_, and each reply sent
is routed to the _client_ that issued the last request. If the original
requester does not exist any more the reply is silently discarded.

Summary of ZMQ_REP characteristics
------------------------- -------------------------------------
Compatible peer sockets   _ZMQ_REQ_, _ZMQ_DEALER_
Direction                 Bidirectional
Send/receive pattern      Receive, Send, Receive, Send, ...
Incoming routing strategy Fair-queued
Outgoing routing strategy Last peer
------------------------- -------------------------------------

### ZMQ_DEALER

A socket of type _ZMQ_DEALER_ is an advanced pattern used for extending
request/reply sockets. Each message sent is round-robined among all connected
peers, and each message received is fair-queued from all connected peers.

When a _ZMQ_DEALER_ socket enters the _mute_ state due to having reached the
high water mark for all peers, or if there are no peers at all, then any
[zmq_send][] operations on the socket shall block until the mute state ends or
at least one peer becomes available for sending; messages are not discarded.

When a _ZMQ_DEALER_ socket is connected to a _ZMQ_REP_ socket each message sent
must consist of an empty message part, the _delimiter_, followed by one or more
_body parts_.

Summary of ZMQ_DEALER characteristics
------------------------- -------------------------------------
Compatible peer sockets   _ZMQ_ROUTER_, _ZMQ_REP_, _ZMQ_DEALER_
Direction                 Bidirectional
Send/receive pattern      Unrestricted
Outgoing routing strategy Round-robin
Incoming routing strategy Fair-queued
Action in mute state      Block
------------------------- -------------------------------------

### ZMQ_ROUTER

A socket of type _ZMQ_ROUTER_ is an advanced socket type used for extending
request/reply sockets. When receiving messages a _ZMQ_ROUTER_ socket shall
prepend a message part containing the _identity_ of the originating peer to the
message before passing it to the application. Messages received are fair-queued
from among all connected peers. When sending messages a _ZMQ_ROUTER_ socket
shall remove the first part of the message and use it to determine the
_identity_ of the peer the message shall be routed to. If the peer does not
exist anymore the message shall be silently discarded by default, unless
_ZMQ_ROUTER_MANDATORY_ socket option is set to `1`.

When a _ZMQ_ROUTER_ socket enters the _mute_ state due to having reached the
high water mark for all peers, then any messages sent to the socket shall be dropped until the mute state ends. Likewise, any messages routed to a peer for which the individual high water mark has been reached shall also be dropped.

When a _ZMQ_REQ_ socket is connected to a _ZMQ_ROUTER_ socket, in addition to the
_identity_ of the originating peer each message received shall contain an empty
_delimiter_ message part. Hence, the entire structure of each received message
as seen by the application becomes: one or more _identity_ parts, _delimiter_
part, one or more _body parts_. When sending replies to a _ZMQ_REQ_ socket the
application must include the _delimiter_ part.

Summary of ZMQ_ROUTER characteristics
------------------------- -------------------------------------
Compatible peer sockets   _ZMQ_DEALER_, _ZMQ_REQ_, _ZMQ_ROUTER_
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

The publish-subscribe pattern is formally defined by <http://rfc.zeromq.org/spec:29>.

### ZMQ_PUB

A socket of type _ZMQ_PUB_ is used by a _publisher_ to distribute data.
Messages sent are distributed in a fan out fashion to all connected peers.
The linkzmq:zmq_recv[3] function is not implemented for this socket type.

When a _ZMQ_PUB_ socket enters the _mute_ state due to having reached the
high water mark for a _subscriber_, then any messages that would be sent to the
_subscriber_ in question shall instead be dropped until the mute state
ends. The *zmq_send()* function shall never block for this socket type.

Summary of ZMQ_PUB characteristics
------------------------- -------------------------------------
Compatible peer sockets   _ZMQ_SUB_, _ZMQ_XSUB_
Direction                 Unidirectional
Send/receive pattern      Send only
Incoming routing strategy N/A
Outgoing routing strategy Fan out
Action in mute state      Drop
------------------------- -------------------------------------

### ZMQ_SUB

A socket of type _ZMQ_SUB_ is used by a _subscriber_ to subscribe to data
distributed by a _publisher_. Initially a _ZMQ_SUB_ socket is not subscribed to
any messages, use the _ZMQ_SUBSCRIBE_ option of [zmq_setsockopt][] to specify
which messages to subscribe to. The _zmq_send()_ function is not implemented
for this socket type.

Summary of ZMQ_SUB characteristics
------------------------- -------------------------------------
Compatible peer sockets   _ZMQ_PUB_, _ZMQ_XPUB_
Direction                 Unidirectional
Send/receive pattern      Receive only
Incoming routing strategy Fair-queued
Outgoing routing strategy N/A
------------------------- -------------------------------------

### ZMQ_XPUB

Same as _ZMQ_PUB_ except that you can receive subscriptions from the peers
in form of incoming messages. Subscription message is a byte 1 (for
subscriptions) or byte 0 (for unsubscriptions) followed by the subscription
body. Messages without a sub/unsub prefix are also received, but have no
effect on subscription status.

Summary of ZMQ_XPUB characteristics
------------------------- -------------------------------------
Compatible peer sockets   _ZMQ_SUB_, _ZMQ_XSUB_
Direction                 Unidirectional
Send/receive pattern      Send messages, receive subscriptions
Incoming routing strategy N/A
Outgoing routing strategy Fan out
Action in mute state      Drop
------------------------- -------------------------------------

### ZMQ_XSUB

Same as _ZMQ_SUB_ except that you subscribe by sending subscription messages to
the socket. Subscription message is a byte 1 (for subscriptions) or byte 0
(for unsubscriptions) followed by the subscription body. Messages without a
sub/unsub prefix may also be sent, but have no effect on subscription status.

Summary of ZMQ_XSUB characteristics
------------------------- -------------------------------------
Compatible peer sockets   _ZMQ_PUB_, _ZMQ_XPUB_
Direction                 Unidirectional
Send/receive pattern      Receive messages, send subscriptions
Incoming routing strategy Fair-queued
Outgoing routing strategy N/A
Action in mute state      Drop
------------------------- -------------------------------------


Pipeline pattern
----------------

The pipeline pattern is used for distributing data to _nodes_ arranged in
a pipeline. Data always flows down the pipeline, and each stage of the pipeline
is connected to at least one _node_. When a pipeline stage is connected to
multiple _nodes_ data is round-robined among all connected _nodes_.

The pipeline pattern is formally defined by <http://rfc.zeromq.org/spec:30>.

### ZMQ_PUSH

A socket of type _ZMQ_PUSH_ is used by a pipeline _node_ to send messages
to downstream pipeline _nodes_. Messages are round-robined to all connected
downstream _nodes_. The _zmq_recv()_ function is not implemented for this
socket type.

When a _ZMQ_PUSH_ socket enters the _mute_ state due to having reached the
high water mark for all downstream _nodes_, or if there are no downstream
_nodes_ at all, then any [zmq_send][] operations on the socket shall block
until the mute state ends or at least one downstream _node_ becomes available
for sending; messages are not discarded.

Summary of ZMQ_PUSH characteristics
------------------------- -------------------------------------
Compatible peer sockets   _ZMQ_PULL_
Direction                 Unidirectional
Send/receive pattern      Send only
Incoming routing strategy N/A
Outgoing routing strategy Round-robin
Action in mute state      Block
------------------------- -------------------------------------

### ZMQ_PULL

A socket of type _ZMQ_PULL_ is used by a pipeline _node_ to receive messages
from upstream pipeline _nodes_. Messages are fair-queued from among all
connected upstream _nodes_. The *zmq_send()* function is not implemented for
this socket type.

Summary of ZMQ_PULL characteristics
------------------------- -------------------------------------
Compatible peer sockets   _ZMQ_PUSH_
Direction                 Unidirectional
Send/receive pattern      Receive only
Incoming routing strategy Fair-queued
Outgoing routing strategy N/A
Action in mute state      Block
------------------------- -------------------------------------


Exclusive pair pattern
----------------------

The exclusive pair pattern is used to connect a peer to precisely one other
peer. This pattern is used for inter-thread communication across the inproc
transport.

The exclusive pair pattern is formally defined by
<http://rfc.zeromq.org/spec:31>.

### ZMQ_PAIR

A socket of type _ZMQ_PAIR_ can only be connected to a single peer at any one
time.  No message routing or filtering is performed on messages sent over a
_ZMQ_PAIR_ socket.

When a _ZMQ_PAIR_ socket enters the _mute_ state due to having reached the
high water mark for the connected peer, or if no peer is connected, then
any [zmq_send][] operations on the socket shall block until the peer becomes
available for sending; messages are not discarded.

> _ZMQ_PAIR_ sockets are designed for inter-thread communication across the
> [zmq_inproc][] transport and do not implement functionality such as
> auto-reconnection. _ZMQ_PAIR_ sockets are considered experimental and may
> have other missing or broken aspects.

Summary of ZMQ_PAIR characteristics
------------------------- -------------------------------------
Compatible peer sockets   _ZMQ_PAIR_
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

A socket of type _ZMQ_STREAM_ is used to send and receive TCP data from a
non-ØMQ peer, when using the tcp:// transport. A _ZMQ_STREAM_ socket can
act as client and/or server, sending and/or receiving TCP data asynchronously.

When receiving TCP data, a _ZMQ_STREAM_ socket shall prepend a message part
containing the _identity_ of the originating peer to the message before passing
it to the application. Messages received are fair-queued from among all
connected peers.

When sending TCP data, a _ZMQ_STREAM_ socket shall remove the first part of the
message and use it to determine the _identity_ of the peer the message shall be
routed to, and unroutable messages shall cause an EHOSTUNREACH or EAGAIN error.

To open a connection to a server, use the zmq_connect call, and then fetch the
socket identity using the _ZMQ_IDENTITY_ *zmq_getsockopt()* call.

To close a specific connection, send the identity frame followed by a
zero-length message (see Example section).

When a connection is made, a zero-length message will be received by the
application.  Similarly, when the peer disconnects (or the connection is lost),
a zero-length message will be received by the application.

The _ZMQ_SNDMORE_ flag is ignored on data frames. You must send one identity
frame followed by one data frame.

Also, please note that omitting the _ZMQ_SNDMORE_ flag will prevent sending
further data (from any client) on the same socket.

Summary of ZMQ_STREAM characteristics
------------------------- -------------------------------------
Compatible peer sockets   None
Direction                 Bidirectional
Send/receive pattern      Unrestricted
Outgoing routing strategy See text
Incoming routing strategy Fair-queued
Action in mute state      EAGAIN
------------------------- -------------------------------------


Return value
------------

The *zmq_socket()* function shall return an opaque handle to the newly created
socket if successful. Otherwise, it shall return _C_NULL_PTR_ and set _errno_
to one of the values defined below.


Errors
------

EINVAL
	~ The requested socket _type_ is invalid.

EFAULT
	~ The provided _context_ is invalid.

EMFILE
	~ The limit on the total number of open ØMQ sockets has been reached.

ETERM
	~ The context specified was terminated.


Example
-------

### Creating a simple HTTP server using ZMQ_STREAM

~~~{.example}
----
void *ctx = zmq_ctx_new ();
assert (ctx);
/* Create ZMQ_STREAM socket */
void *socket = zmq_socket (ctx, ZMQ_STREAM);
assert (socket);
int rc = zmq_bind (socket, "tcp://*:8080");
assert (rc == 0);
/* Data structure to hold the ZMQ_STREAM ID */
uint8_t id [256];
size_t id_size = 256;
/* Data structure to hold the ZMQ_STREAM received data */
uint8_t raw [256];
size_t raw_size = 256;
while (1) {
	/*  Get HTTP request; ID frame and then request */
	id_size = zmq_recv (socket, id, 256, 0);
	assert (id_size > 0);
	do {
		raw_size = zmq_recv (socket, raw, 256, 0);
		assert (raw_size >= 0);
	} while (raw_size == 256);
	/* Prepares the response */
	char http_response [] =
		"HTTP/1.0 200 OK\r\n"
		"Content-Type: text/plain\r\n"
		"\r\n"
		"Hello, World!";
	/* Sends the ID frame followed by the response */
	zmq_send (socket, id, id_size, ZMQ_SNDMORE);
	zmq_send (socket, http_response, strlen (http_response), ZMQ_SNDMORE);
	/* Closes the connection by sending the ID frame followed by a zero response */
	zmq_send (socket, id, id_size, ZMQ_SNDMORE);
	zmq_send (socket, 0, 0, ZMQ_SNDMORE);
	/* NOTE: If we don't use ZMQ_SNDMORE, then we won't be able to send more */
	/* message to any client */
}
zmq_close (socket);
zmq_ctx_destroy (ctx);
----
~~~


See also
--------

[zmq_init][]
[zmq_setsockopt][]
[zmq_bind][]
[zmq_connect][]
[zmq_send][]
[zmq_recv][]
[zmq_inproc][]
