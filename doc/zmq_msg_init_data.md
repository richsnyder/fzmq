% zmq_msg_init_data


Name
----

zmq_msg_init_data - initialize a ØMQ message from a supplied buffer


Synopsis
--------

~~~{.synopsis}
PROCEDURE zmq_free_fn(data, hint)

  TYPE(C_PTR), INTENT(IN) :: data
  TYPE(C_PTR), INTENT(IN) :: hint
~~~

~~~{.synopsis}
FUNCTION zmq_msg_init_data(message, data, size, ffn, hint) RESULT(code)

  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: message
  TYPE(C_PTR), INTENT(IN) :: data
  INTEGER(KIND = C_SIZE_T), INTENT(IN) :: size
  TYPE(C_FUNPTR), INTENT(IN) :: ffn
  TYPE(C_PTR), INTENT(IN) :: hint
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_msg_init_data()* function shall initialize the message object
referenced by _message_ to represent the content referenced by the buffer
located at address _data_, _size_ bytes long.  No copy of _data_ shall be
performed and ØMQ shall take ownership of the supplied buffer.

If provided, the deallocation function _ffn_ shall be called once the data
buffer is no longer required by ØMQ, with the _data_ and _hint_ arguments
supplied to *zmq_msg_init_data()*.  If the deallocation function is not
provided, the allocated memory will not be freed, and this may cause a memory
leak.  The deallocation function needs to be thread-safe, since it will be
called from an arbitrary thread.

Never access *zmq_msg_t* members directly, instead always use the *zmq_msg*
family of functions.

The functions *zmq_msg_init()*, *zmq_msg_init_data()* and
*zmq_msg_init_size()* are mutually exclusive. Never initialize the same
message twice.


Return value
------------

The *zmq_msg_init_data()* function shall return zero if successful.  Otherwise
it shall return `-1` and set 'errno' to one of the values defined below.


Errors
------

*ENOMEM*
  ~ Insufficient storage space is available.


Example
-------

### Initializing a message from a supplied buffer

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
TYPE(C_PTR) :: ffn_ptr
TYPE(C_PTR) :: str_ptr
TYPE(ZMQ_MSG_T) :: message
INTEGER(KIND = C_INT) :: rc
INTEGER(KIND = C_INT) :: nbytes
INTEGER(KIND = C_SIZE_T) :: str_len
CHARACTER(KIND = C_CHAR, LEN = :), TARGET, ALLOCATABLE :: str

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_REQ)
rc = zmq_connect(socket, 'tcp://127.0.0.1:5555')

ALLOCATE(CHARACTER(KIND = C_CHAR, LEN = 5) :: str)

str = 'Hello'
str_ptr = C_LOC(str)
ffn_ptr = C_FUNLOC(ffn)
str_len = LEN(str, KIND = C_SIZE_T)

rc = zmq_msg_init_data(message, str_ptr, str_len, ffn_ptr, C_NULL_PTR)
nbytes = zmq_msg_send(message, socket, 0)

rc = zmq_close(socket)
rc = zmq_ctx_term(context)
~~~

with a deallocation function defined by

~~~{.example}
SUBROUTINE ffn(data, hint)
  TYPE(C_PTR), VALUE, INTENT(IN) :: data
  TYPE(C_PTR), VALUE, INTENT(IN) :: hint

  CHARACTER(KIND = C_CHAR, LEN = :), POINTER :: str

  CALL c_f_pointer(data, str)
  deallocate(str)
END SUBROUTINE
~~~


See also
--------

[zmq_msg_init_size][]
[zmq_msg_init][]
[zmq_msg_close][]
[zmq_msg_data][]
[zmq_msg_size][]
[fzmq][]
