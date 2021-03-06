% zmq_poll


Name
----

zmq_poll - input/output multiplexing


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_poll(items, nitems, timeout) RESULT(code)

  TYPE(ZMQ_POLLITEM_T), DIMENSION(*), INTENT(INOUT) :: items
  INTEGER(KIND = C_INT), INTENT(IN) :: nitems
  INTEGER(KIND = C_LONG), INTENT(IN) :: timeout
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_poll()* function provides a mechanism for applications to multiplex
input/output events in a level-triggered fashion over a set of sockets.  Each
member of the array pointed to by the _items_ argument is a *zmq_pollitem_t*
structure, defined as follows:

~~~
TYPE, BIND(C) :: zmq_pollitem_t
  TYPE(C_PTR) :: socket
  INTEGER(KIND = C_INT) :: fd
  INTEGER(KIND = C_SHORT) :: events
  INTEGER(KIND = C_SHORT) :: revents
END TYPE zmq_pollitem_t
~~~

The _nitems_ argument specifies the number of items in the _items_ array.  

For each *zmq_pollitem_t* item, *zmq_poll()* shall examine either the ØMQ
socket referenced by _socket_ *or* the standard socket specified by the file
descriptor _fd_, for the event(s) specified in _events_.  If both _socket_ and
_fd_ are set in a single *zmq_pollitem_t*, the ØMQ socket referenced by
_socket_ shall take precedence and the value of _fd_ shall be ignored.

For each *zmq_pollitem_t* item, *zmq_poll()* shall first clear the _revents_
member, and then indicate any requested events that have occurred by setting
the bit corresponding to the event condition in the _revents_ member.  If none
of the requested events have occurred on any *zmq_pollitem_t* item,
*zmq_poll()* shall wait _timeout_ milliseconds for an event to occur on
any of the requested items.  If the value of _timeout_ is `0`, *zmq_poll()*
shall return immediately.  If the value of _timeout_ is `-1`, *zmq_poll()*
shall block indefinitely until a requested event has occurred on at least one
*zmq_pollitem_t*.

The _events_ and _revents_ members of *zmq_pollitem_t* are bit masks
constructed by OR'ing a combination of the following event flags:

*ZMQ_POLLIN*
  ~ For ØMQ sockets, at least one message may be received from the _socket_
    without blocking.  For standard sockets this is equivalent to the *POLLIN*
    flag of the *poll()* system call and generally means that at least one byte
    of data may be read from _fd_ without blocking.

*ZMQ_POLLOUT*
  ~ For ØMQ sockets, at least one message may be sent to the _socket_ without
    blocking.  For standard sockets this is equivalent to the *POLLOUT* flag of
    the *poll()* system call and generally means that at least one byte of data
    may be written to _fd_ without blocking.

*ZMQ_POLLERR*
  ~ For standard sockets, this flag is passed through *zmq_poll()* to the
    underlying *poll()* system call and generally means that some sort of error
    condition is present on the socket specified by _fd_.  For ØMQ sockets this
    flag has no effect if set in _events_, and shall never be returned in
    _revents_ by *zmq_poll()*.

The *zmq_poll()* function may be implemented or emulated using operating system
interfaces other than *poll()*, and as such may be subject to the limits of
those interfaces in ways not defined in this documentation.  Refer to your
operating system's documentation of the *poll()* system call for more
information.


Return value
------------

Upon successful completion, the *zmq_poll()* function shall return the number
of *zmq_pollitem_t* structures with events signaled in _revents_ or `0` if no
events have been signaled.  Upon failure, *zmq_poll()* shall return `-1` and
set _errno_ to one of the values defined below.


Errors
------

*ETERM*
  ~ At least one of the members of the _items_ array refers to a _socket_ whose
    associated ØMQ _context_ was terminated.

*EFAULT*
  ~ The provided _items_ was not valid.

*EINTR*
  ~ The operation was interrupted by delivery of a signal before any events
    were available.


Example
-------

### Polling indefinitely for input events on two ØMQ sockets

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket1
TYPE(C_PTR) :: socket2
TYPE(ZMQ_MSG_T) :: message
INTEGER(KIND = C_INT) :: rc
TYPE(ZMQ_POLLITEM_T), DIMENSION(2) :: items

context = zmq_ctx_new()
socket1 = zmq_socket(context, ZMQ_REP)
socket2 = zmq_socket(context, ZMQ_REP)

items[1]%socket = socket1
items[1]%events = ZMQ_POLLIN
items[2]%socket = socket2
items[2]%socket = ZMQ_POLLIN

rc = zmq_poll(items, 2, -1)
IF (rc > 0) THEN
  rc = zmq_msg_init(message)
  IF (IAND(items(1)%revents, ZMQ_POLLIN) /= 0) THEN
    rc = zmq_msg_recv(message, socket1, 0)
  ELSE IF (IAND(items(2)%revents, ZMQ_POLLIN) /= 0) THEN
    rc = zmq_msg_recv(message, socket2, 0)
  END IF
  rc = zmq_msg_close(message)
END IF

rc = zmq_close(socket1)
rc = zmq_close(socket2)
rc = zmq_ctx_term(context)
~~~


See also
--------

[zmq_socket][]
[zmq_msg_send][]
[zmq_msg_recv][]
[zmq_send][]
[zmq_recv][]
[fzmq][]
