% zmq_msg_gets


Name
----

zmq_msg_gets - get message metadata property


Synopsis
--------

~~~{.synopsis}
TBD
~~~


Description
-----------

The *zmq_msg_gets()* function shall return the string value for the metadata
property specified by the _property_ argument for the message pointed to by
the _message_ argument.

Metadata is defined on a per-connection basis during the ØMQ connection
handshake as specified in <rfc.zeromq.org/spec:37>.

The following ZMTP properties can be retrieved with the *zmq_msg_gets()*
function:

* Socket-Type
* Identity
* Resource

Additionally, when available for the underlying transport, the _Peer-Address_
property will return the IP address of the remote endpoint as returned by
_getnameinfo(2)_.

Other properties may be defined based on the underlying security mechanism,
see ZAP authenticated connection sample below.


Return value
------------

The *zmq_msg_gets()* function shall return the string value for the property
if successful. Otherwise it shall return _C_NULL_PTR_ and set _errno_ to one of
the values defined below. The caller shall not modify or free the returned
value, which shall be owned by the message. The encoding of the property and
value shall be UTF8.


Errors
------

EINVAL
  ~ The requested _property_ is unknown.


Example
-------

### Getting the ZAP authenticated user id for a message

~~~{.example}
TBD
~~~
