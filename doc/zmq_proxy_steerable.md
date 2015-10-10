% zmq_proxy_steerable


Name
----

zmq_proxy_steerable - built-in ØMQ proxy with control flow


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_proxy_steerable(frontend, backend, capture, control) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: frontend
  TYPE(C_PTR), INTENT(IN) :: backend
  TYPE(C_PTR), INTENT(IN) :: capture
  TYPE(C_PTR), INTENT(IN) :: control
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_proxy_steerable()* function starts the built-in ØMQ proxy in the
current application thread, as *zmq_proxy()* does. Please, refer to this
function for the general description and usage. We describe here only the
additional control flow provided by the socket passed as the fourth argument
_control_.

If the control socket is not _C_NULL_PTR_, the proxy supports control flow. If
_PAUSE_ is received on this socket, the proxy suspends its activities. If
_RESUME_ is received, it goes on. If _TERMINATE_ is received, it terminates
smoothly. At start, the proxy runs normally as if *zmq_proxy()* was used.

If the control socket is _C_NULL_PTR_, the function behave exactly as if
*zmq_proxy()* had been called.

Refer to [zmq_socket][] for a description of the available socket types.
Refer to [zmq_proxy][] for a description of the *zmq_proxy()*.


Return value
------------

The *zmq_proxy_steerable()* function returns 0 if _TERMINATE_ is sent to its
control socket. Otherwise, it returns `-1` and _errno_ set to ETERM (the
ØMQ _context_ associated with either of the specified sockets was terminated).


Example
-------

### Creating a shared queue proxy

~~~{.example}
frontend = zmq_socket(context, ZMQ_ROUTER)
backend = zmq_socket(context, ZMQ_DEALER)
control = zmq_socket(context, ZMQ_SUB)
rc = zmq_bind(frontend, 'tcp://*:5555')
rc = zmq_bind(backend, 'tcp://*:5556')
rc = zmq_connect(control, 'tcp://*:5557')
rc = zmq_setsockopt(control, ZMQ_SUBSCRIBE, "")
rc = zmq_proxy_steerable(frontend, backend, C_NULL_PTR, control)
~~~

### Set up a controller in another node, process or whatever

~~~{.example}
control = zmq_socket(context, ZMQ_PUB)
signal = 'PAUSE'
nbytes = zmq_send(control, C_LOC(signal), 5_C_SIZE_T, 0)
signal = 'RESUME'
nbytes = zmq_send(control, C_LOC(signal), 6_C_SIZE_T, 0)
signal = 'TERMINATE'
nbytes = zmq_send(control, C_LOC(signal), 9_C_SIZE_T, 0)
~~~


See also
--------

[zmq_proxy][]
[zmq_bind][]
[zmq_connect][]
[zmq_socket][]
