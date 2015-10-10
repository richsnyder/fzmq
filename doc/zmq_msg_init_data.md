% zmq_msg_init_data


Name
----

zmq_msg_init_data - initialise a ØMQ message from a supplied buffer


Synopsis
--------

~~~{.synopsis}
PROCEDURE zmq_free_fn(data, hint)

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

The *zmq_msg_init_data()* function shall initialise the message object
referenced by _message_ to represent the content referenced by the buffer
located at address _data_, _size_ bytes long. No copy of _data_ shall be
performed and ØMQ shall take ownership of the supplied buffer.

If provided, the deallocation function _ffn_ shall be called once the data
buffer is no longer required by ØMQ, with the _data_ and _hint_ arguments
supplied to *zmq_msg_init_data()*.

Never access _zmq_msg_t_ members directly, instead always use the *zmq_msg*
family of functions.

The deallocation function _ffn_ needs to be thread-safe, since it will be
called from an arbitrary thread.

If the deallocation function is not provided, the allocated memory will not be
freed, and this may cause a memory leak.

The functions *zmq_msg_init()*, *zmq_msg_init_data()* and
*zmq_msg_init_size()* are mutually exclusive. Never initialize the same
message twice.


Return value
------------

The *zmq_msg_init_data()* function shall return zero if successful. Otherwise
it shall return `-1` and set 'errno' to one of the values defined below.


Errors
------

ENOMEM
  ~ Insufficient storage space is available.


Example
-------

### Initialising a message from a supplied buffer

~~~{.example}
----
void my_free (void *data, void *hint)
{
    free (data);
}

    /*  ...  */

void *data = malloc (6);
assert (data);
memcpy (data, "ABCDEF", 6);
zmq_msg_t msg;
rc = zmq_msg_init_data (&msg, data, 6, my_free, NULL);
assert (rc == 0);
----
~~~


See also
--------

[zmq_msg_init_size][]
[zmq_msg_init][]
[zmq_msg_close][]
[zmq_msg_data][]
[zmq_msg_size][]
