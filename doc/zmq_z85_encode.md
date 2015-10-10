% zmq_z85_encode


Name
----

zmq_z85_encode - encode a binary key as Z85 printable text


Synopsis
--------

~~~{.synopsis}
TBD
~~~


Description
-----------

The *zmq_z85_encode()* function shall encode the binary block specified
by _data_ and _size_ into a string in _dest_. The size of the binary block
must be divisible by 4. The _dest_ must have sufficient space for size * 1.25
plus 1 for a null terminator. A 32-byte CURVE key is encoded as 40 ASCII
characters plus a null terminator.

The encoding shall follow the ZMQ RFC 32 specification.


Return value
------------

The *zmq_z85_encode()* function shall return _dest_ if successful, else it
shall return _C_NULL_PTR_.


Example
-------

### Encoding a CURVE key

~~~{.example}
TBD
~~~


See also
--------

[zmq_z85_decode][]
[zmq_curve_keypair][]
[zmq_curve][]
