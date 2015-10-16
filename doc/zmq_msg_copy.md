% zmq_msg_copy


Name
----

zmq_msg_copy - copy the content of a message to another message


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_msg_copy(dest, src) RESULT(code)

  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: dest
  TYPE(ZMQ_MSG_T), INTENT(INOUT) :: src
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_msg_copy()* function shall copy the message object referenced by _src_
to the message object referenced by _dest_.  The original content of _dest_, if
any, shall be released.  You must initialize _dest_ before copying to it.

The implementation may choose not to physically copy the message content, but
rather to share the underlying buffer between _src_ and _dest_.  Avoid
modifying message content after a message has been copied with
*zmq_msg_copy()*.  Doing so can result in undefined behaviour.  If what you
need is an actual hard copy, allocate a new message using *zmq_msg_init_size()*
and copy the message content.

Never access *zmq_msg_t* members directly, instead always use the *zmq_msg*
family of functions.


Return value
------------

The *zmq_msg_copy()* function shall return zero if successful.  Otherwise it
shall return `-1` and set _errno_ to one of the values defined below.


Errors
------

*EFAULT*
  ~ Invalid message.


Example
-------

### Copying a message

~~~{.example}
TYPE(ZMQ_MSG_T) :: msg_src
TYPE(ZMQ_MSG_T) :: msg_dest
INTEGER(KIND = C_INT) :: rc

rc = zmq_msg_init_size(msg_src, 8_C_SIZE_T)
rc = zmq_msg_init(msg_dest)
rc = zmq_msg_copy(msg_dest, msg_src)
~~~


See also
--------

[zmq_msg_move][]
[zmq_msg_init][]
[zmq_msg_init_size][]
[zmq_msg_init_data][]
[zmq_msg_close][]
[fzmq][]
