% zmq_gssapi


Name
----

zmq_gssapi - secure authentication and confidentiality


Synopsis
--------

The GSSAPI mechanism defines a mechanism for secure authentication and
confidentiality for communications between a client and a server using the
Generic Security Service Application Program Interface (GSSAPI). The GSSAPI
mechanism can be used on both public and private networks. GSSAPI itself is
defined in IETF RFC-2743: <http://tools.ietf.org/html/rfc2743>. The ØMQ GSSAPI
mechanism is defined by this document: <http://rfc.zeromq.org/spec:38>.


Client and server roles
-----------------------

A socket using GSSAPI can be either client or server, but not both.

To become either a GSSAPI client or server, the application sets the
_ZMQ_GSSAPI_PRINCIPAL_ option to provide the socket with the name of the
pricipal for whom GSSAPI credentials should be acquired.

To become a GSSAPI server, the application addtionally sets the
_ZMQ_GSSAPI_SERVER_ option on the socket.

To become a GSSAPI client, the application additionally sets the
_ZMQ_GSSAPI_SERVICE_PRINCIPAL_ option to the name of the principal of the
server to which it intends to connect.


Optional encryption
-------------------

By default, the GSSAPI mechanism will encrypt all communications between client
and server. If encryption is not desired (e.g. on private networks), the client
and server applications can disable it by setting the _ZMQ_GSSAPI_PLAINTEXT_
option. Both the client and server must set this option to the same value.


See also
--------

[zmq_setsockopt][]
[zmq_null][]
[zmq_curve][]
