% zmq_plain


Name
----

zmq_plain - clear-text authentication


Synopsis
--------

The PLAIN mechanism defines a simple username/password mechanism that
lets a server authenticate a client. PLAIN makes no attempt at security
or confidentiality. It is intended for use on internal networks where
security requirements are low. The PLAIN mechanism is defined by this
document: <http://rfc.zeromq.org/spec:24>.


Usage
-----

To use PLAIN, the server shall set the _ZMQ_PLAIN_SERVER_ option, and the
client shall set the _ZMQ_PLAIN_USERNAME_ and _ZMQ_PLAIN_PASSWORD_ socket
options. Which peer binds, and which connects, is not relevant.


See also
--------

[zmq_setsockopt][]
[zmq_null][]
[zmq_curve][]
