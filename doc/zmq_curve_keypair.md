% zmq_curve_keypair


Name
----

zmq_curve_keypair - generate a new CURVE keypair


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_curve_keypair(z85_public_key, z85_secret_key) RESULT(code)

  CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(OUT) :: z85_public_key
  CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(OUT) :: z85_secret_key
  INTEGER(KIND = C_INT) :: code
~~~

Description
-----------

The *zmq_curve_keypair()* function shall return a newly generated random
keypair consisting of a public key and a secret key. The caller provides
two buffers, each at least 41 octets large, in which this method will
store the keys. The keys are encoded using [zmq_z85_encode][].


Return value
------------

The *zmq_curve_keypair()* function shall return zero if successful.  Otherwise
it returns `-1` and sets _errno_ to one of the values defined below.


Errors
------

ENOTSUP
  ~ The ØMQ library was not built with cryptographic support (libsodium).


Example
-------

### Generating a new CURVE keypair

~~~{.example}
TBD
~~~


See also
--------

[zmq_z85_decode][]
[zmq_z85_encode][]
[zmq_curve][]
