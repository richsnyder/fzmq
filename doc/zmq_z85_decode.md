% zmq_z85_decode


Name
----

zmq_z85_decode - decode a binary key from Z85 printable text


Synopsis
--------

~~~{.synopsis}
TBD
~~~


Description
-----------

The *zmq_z85_decode()* function shall decode _string_ into _dest_.  The length
of _string_ shall be divisible by 5. _dest_ must be large enough for the
decoded value (0.8 x strlen (string)).

The encoding shall follow the ZMQ RFC 32 specification.


Return value
------------

The *zmq_z85_decode()* function shall return _dest_ if successful, else it
shall return _C_NULL_PTR_.


Example
-------

### Decoding a CURVE key

~~~{.example}
TBD
~~~


See also
--------

[zmq_z85_decode][]
[zmq_curve_keypair][]
[zmq_curve][]
