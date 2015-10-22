% zmq_z85_decode


Name
----

zmq_z85_decode - decode a binary key from Z85 printable text


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_z85_decode(dest, string) RESULT(success)

  TYPE(C_PTR), INTENT(IN) :: dest
  CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(IN) :: string
  LOGICAL :: success
~~~


Description
-----------

The *zmq_z85_decode()* function shall decode _string_ into _dest_.  The length
of _string_ shall be divisible by 5, plus one for a terminating *C_NULL_CHAR*
character.  The _dest_ pointer must point to a buffer large enough for the
decoded value, which is at least (4/5)\*(LEN(string)-1).

The encoding shall follow the ZMQ RFC 32 specification.


Return value
------------

The *zmq_z85_decode()* function shall return `.true.` if successful.
Otherwise it shall return `.false.`.


Example
-------

### Decoding a Z85 string

~~~{.example}
LOGICAL :: SUCCESS
CHARACTER(KIND = C_CHAR, LEN = 11) :: ENCODED
CHARACTER(KIND = C_CHAR, LEN = 8), TARGET :: DECODED

ENCODED = 'HelloWorld' // C_NULL_CHAR
SUCCESS = ZMQ_Z85_DECODE(C_LOC(DECODED), ENCODED)
~~~


See also
--------

[zmq_z85_decode][]
[zmq_curve_keypair][]
[zmq_curve][]
[fzmq][]
