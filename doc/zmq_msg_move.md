% zmq_msg_move


Name
----

zmq_msg_move - move the content of a message to another message


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_msg_move(dest, src) RESULT(code)

  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: dest
  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: src
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_msg_move()* function shall move the content of the message object
referenced by _src_ to the message object referenced by _dest_.  No actual
copying of message content is performed, _dest_ is simply updated to reference
the new content.  _src_ becomes an empty message after calling
*zmq_msg_move()*.  The original content of _dest_, if any, shall be released.

Never access *zmq_msg_t* members directly, instead always use the *zmq_msg*
family of functions.


Return value
------------

The *zmq_msg_move()* function shall return zero if successful.  Otherwise it
shall return `-1` and set _errno_ to one of the values defined below.


Errors
------

*EFAULT*
  ~ Invalid message.


Example
-------

### TBD

~~~{.example}
TYPE(ZMQ_MSG_T) :: msg_src
TYPE(ZMQ_MSG_T) :: msg_dest
INTEGER(KIND = C_INT) :: rc

rc = zmq_msg_init_size(msg_src, 8_C_SIZE_T)
rc = zmq_msg_init(msg_dest)
rc = zmq_msg_move(msg_dest, msg_src)
~~~

See also
--------

[zmq_msg_copy][]
[zmq_msg_init][]
[zmq_msg_init_size][]
[zmq_msg_init_data][]
[zmq_msg_close][]
[fzmq][]
