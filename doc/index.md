% Table of Contents

@PROJECT_NAME@/@PROJECT_VERSION@ API Reference
----------------------------------------------

* [zmq_bind][] - accept incoming connections on a socket
* [zmq_close][] - close ØMQ socket
* [zmq_connect][] - create outgoing connection from socket
* [zmq_ctx_destroy][] - terminate a ØMQ context
* [zmq_ctx_get][] - get context options
* [zmq_ctx_new][] - create new ØMQ context
* [zmq_ctx_set][] - set context options
* [zmq_ctx_shutdown][] - shutdown a ØMQ context
* [zmq_ctx_term][] - destroy a ØMQ context
* [zmq_curve][] - secure authentication and confidentiality
* [zmq_curve_keypair][] - generate a new CURVE keypair
* [zmq_disconnect][] - disconnect a socket
* [zmq_errno][] - retrieve value of errno for the calling thread
* [zmq_getsockopt][] - get ØMQ socket options
* [zmq_gssapi][] - secure authentication and confidentiality
* [zmq_has][] - check a ØMQ capability
* [zmq_init][] - initialise ØMQ context
* [zmq_inproc][] - ØMQ local in-process (inter-thread) communication transport
* [zmq_ipc][] - ØMQ local inter-process communication transport
* [zmq_msg_close][] - release ØMQ message
* [zmq_msg_copy][] - copy content of a message to another message
* [zmq_msg_data][] - retrieve pointer to message content
* [zmq_msg_gets][] - get message metadata property
* [zmq_msg_get][] - get message property
* [zmq_msg_init][] - initialise empty ØMQ message
* [zmq_msg_init_data][] - initialise ØMQ message from a supplied buffer
* [zmq_msg_init_size][] - initialise ØMQ message of a specified size
* [zmq_msg_more][] - indicate if there are more message parts to receive
* [zmq_msg_move][] - move content of a message to another message
* [zmq_msg_recv][] - receive a message part from a socket
* [zmq_msg_send][] - send a message part on a socket
* [zmq_msg_set][] - set message property
* [zmq_msg_size][] - retrieve message content size in bytes
* [zmq_null][] - no security or confidentiality
* [zmq_pgm][] - ØMQ reliable multicast transport using PGM
* [zmq_plain][] - clear-text authentication
* [zmq_poll][] - input/output multiplexing
* [zmq_proxy][] - built-in ØMQ proxy
* [zmq_proxy_steerable][] - built-in ØMQ proxy with control flow
* [zmq_recv][] - receive a message part from a socket
* [zmq_recvmsg][] - receive a message part from a socket
* [zmq_send][] - send a message part on a socket
* [zmg_sendmsg][] - send a message part on a socket
* [zmq_send_const][] - send a constant-memory message part on a socket
* [zmq_setsockopt][] - set ØMQ socket options
* [zmq_socket][] - create ØMQ socket
* [zmq_socket_monitor][] - monitor socket events
* [zmq_strerror][] - get ØMQ error message string
* [zmq_tcp][] - ØMQ unicast transport using TCP
* [zmq_term][] - terminate ØMQ context
* [zmq_tipc][] - ØMQ unicast transport using TIPC
* [zmq_unbind][] - Stop accepting connections on a socket
* [zmq_version][] - report ØMQ library version
* [zmq_z85_decode][] - decode a binary key from Z85 printable text
* [zmq_z85_encode][] - encode a binary key as Z85 printable text
