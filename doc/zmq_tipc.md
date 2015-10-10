% zmq_tipc


Name
----

zmq_tipc - ØMQ unicast transport using TIPC


Synopsis
--------

TIPC is a cluster IPC protocol with a location transparent addressing scheme.


Addressing
----------

A ØMQ endpoint is a string consisting of a _transport_`://` followed by an
_address_. The _transport_ specifies the underlying protocol to use. The
_address_ specifies the transport-specific address to connect to.

For the TIPC transport, the transport is `tipc`, and the meaning of the
_address_ part is defined below.


### Assigning a port name to a socket

When assigning a port name to a socket using *zmq_bind()* with the _tipc_
transport, the _endpoint_ is defined in the form: {type, lower, upper}.

* Type is the numerical (u32) ID of your service.
* Lower and upper specify a range for your service.

Publishing the same service with overlapping lower/upper ID's will
cause connection requests to be distributed over these in a round-robin
manner.


### Connecting a socket

When connecting a socket to a peer address using *zmq_connect()* with the
_tipc_ transport, the _endpoint_ shall be interpreted as a service ID, followed
by a comma and the instance ID.

The instance ID must be within the lower/upper range of a published port name
for the endpoint to be valid.


Examples
--------

### Assigning a local address to a socket

~~~{.example}
rc = zmq_bind(socket, 'tipc://{5555,0,0}')
rc = zmq_bind(socket, 'tipc://{5555,0,100}')
~~~

### Connecting a socket

~~~{.example}
rc = zmq_connect(socket, 'tipc://{555,50}')
~~~


See also
--------

[zmq_bind][]
[zmq_connect][]
[zmq_tcp][]
[zmq_pgm][]
[zmq_ipc][]
[zmq_inproc][]
