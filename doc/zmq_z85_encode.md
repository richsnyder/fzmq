% zmq_z85_encode


Name
----

zmq_z85_encode - encode a binary key as Z85 printable text


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_z85_encode(dest, data, size) RESULT(success)
  CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(OUT) :: dest
  TYPE(C_PTR), INTENT(IN) :: data
  INTEGER(KIND = C_SIZE_T), INTENT(IN) :: size
  LOGICAL :: success
~~~


Description
-----------

The *zmq_z85_encode()* function shall encode the binary block specified by
_data_ and _size_ into the _dest_ character string.  The size of the binary
block must be divisible by 4.  The _dest_ string must be large enough to store
the encoded value, plus 1 for a terminating *C_NULL_CHAR*, which is at least
(5/4)\*(SIZE)+1.  A 32-byte CURVE key, for example, is encoded as 40 ASCII
characters plus a null terminator.

The encoding shall follow the ZMQ RFC 32 specification.


Return value
------------

The *zmq_z85_encode()* function shall return `.true.` if successful.
Otherwise it shall return `.false.`.


Example
-------

### Encoding to a Z85 string

~~~{.example}
LOGICAL :: SUCCESS
CHARACTER(KIND = C_CHAR, LEN = 8), TARGET :: DECODED
CHARACTER(KIND = C_CHAR, LEN = 11) :: ENCODED

DECODED(1:1) = CHAR(134)
DECODED(2:2) = CHAR( 79)
DECODED(3:3) = CHAR(210)
DECODED(4:4) = CHAR(111)
DECODED(5:5) = CHAR(181)
DECODED(6:6) = CHAR( 89)
DECODED(7:7) = CHAR(247)
DECODED(8:8) = CHAR( 91)
SUCCESS = ZMQ_Z85_ENCODE(ENCODED, C_LOC(DECODED), 8_C_SIZE_T)
~~~


See also
--------

[zmq_z85_decode][]
[zmq_curve_keypair][]
[zmq_curve][]
[fzmq][]
