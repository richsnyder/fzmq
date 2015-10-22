% zmq_getsockopt


Name
----

zmq_getsockopt - get ØMQ socket options


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_getsockopt(socket, option_name, bin_value, option_len) RESULT(code)
FUNCTION zmq_getsockopt(socket, option_name, i32_value) RESULT(code)
FUNCTION zmq_getsockopt(socket, option_name, i64_value) RESULT(code)
FUNCTION zmq_getsockopt(socket, option_name, str_value) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: socket
  INTEGER(KIND = C_INT), INTENT(IN) :: option_name
  TYPE(C_PTR), INTENT(IN) :: option_value
  INTEGER(KIND = C_SIZE_T), INTENT(INOUT) :: option_len
  INTEGER(KIND = C_INT), INTENT(OUT) :: i32_value
  INTEGER(KIND = C_INT64_T), INTENT(OUT) :: i64_value
  CHARACTER(LEN = *), INTENT(INOUT) :: str_value
  INTEGER(KIND = C_INT) :: code
~~~


Description
-----------

The *zmq_getsockopt()* function shall retrieve the value for the option
specified by the _option_name_ argument for the ØMQ socket pointed to by the
_socket_ argument, and store it in the _*_value_ argument.  If present, the
_option_len_ argument is the size in bytes of the buffer pointed to by
_bin_value_; upon successful completion, *zmq_getsockopt()* shall modify the
_option_len_ argument to indicate the actual size of the option value stored in
the buffer.

The following options can be retrieved with the *zmq_getsockopt()* function:

ZMQ_AFFINITY
  ~ *Retrieve I/O thread affinity*.  The _ZMQ_AFFINITY_ option shall retrieve
    the I/O thread affinity for newly created connections on the specified
    _socket_.

    Affinity determines which threads from the ØMQ I/O thread pool associated
    with the socket's _context_ shall handle newly created connections.  A
    value of zero specifies no affinity, meaning that work shall be distributed
    fairly among all ØMQ I/O threads in the thread pool.  For non-zero values,
    the lowest bit corresponds to thread 1, second lowest bit to thread 2 and
    so on.  For example, a value of 3 specifies that subsequent connections on
    _socket_ shall be handled exclusively by I/O threads 1 and 2.

    ----------------------- ------------
    Option value type       uint64_t
    Option value unit       N/A (bitmap)
    Default value           0
    Applicable socket types N/A
    ----------------------- ------------

ZMQ_BACKLOG
  ~ *Retrieve maximum length of the queue of outstanding connections*.  The
    *ZMQ_BACKLOG* option shall retrieve the maximum length of the queue of
    outstanding peer connections for the specified _socket_; this only applies
    to connection-oriented transports.  For details refer to your operating
    system documentation for the *listen* function.

    ----------------------- --------------------------------------------
    Option value type       int
    Option value unit       connections
    Default value           100
    Applicable socket types all, only for connection-oriented transports
    ----------------------- --------------------------------------------

ZMQ_CURVE_PUBLICKEY
  ~ *Retrieve current CURVE public key*.  Retrieves the current long term
    public key for the socket.  You can provide either a 32 byte buffer, to
    retrieve the binary key value, or a 41 byte buffer, to retrieve the key in
    a printable Z85 format.  NOTE: to fetch a printable key, the buffer must be
    41 bytes large to hold the 40-char key value and one null byte.

    ----------------------- ------------------------------
    Option value type       binary data or Z85 text string
    Option value size       32 or 41
    Default value           null
    Applicable socket types all, when using TCP transport
    ----------------------- ------------------------------

ZMQ_CURVE_SECRETKEY
  ~ *Retrieve current CURVE secret key*.  Retrieves the current long term
    secret key for the socket.  You can provide either a 32 byte buffer, to
    retrieve the binary key value, or a 41 byte buffer, to retrieve the key in
    a printable Z85 format.  NOTE: to fetch a printable key, the buffer must be
    41 bytes large to hold the 40-char key value and one null byte.

    ----------------------- ------------------------------
    Option value type       binary data or Z85 text string
    Option value size       32 or 41
    Default value           null
    Applicable socket types all, when using TCP transport
    ----------------------- ------------------------------

ZMQ_CURVE_SERVERKEY
  ~ *Retrieve current CURVE server key*.  Retrieves the current server key for
    the client socket.  You can provide either a 32 byte buffer, to retrieve
    the binary key value, or a 41-byte buffer, to retrieve the key in a
    printable Z85 format.  NOTE: to fetch a printable key, the buffer must be
    41 bytes large to hold the 40-char key value and one null byte.

    ----------------------- ------------------------------
    Option value type       binary data or Z85 text string
    Option value size       32 or 41
    Default value           null
    Applicable socket types all, when using TCP transport
    ----------------------- ------------------------------

ZMQ_EVENTS
  ~ *Retrieve socket event state*.  The *ZMQ_EVENTS* option shall retrieve the
    event state for the specified _socket_.  The returned value is a bit mask
    constructed by OR'ing a combination of the following event flags:

    ZMQ_POLLIN
      ~ Indicates that at least one message may be received from the specified
        socket without blocking.

    ZMQ_POLLOUT
      ~ Indicates that at least one message may be sent to the specified socket
        without blocking.

    The combination of a file descriptor returned by the _ZMQ_FD_ option being
    ready for reading but no actual events returned by a subsequent retrieval
    of the _ZMQ_EVENTS_ option is valid; applications should simply ignore this
    case and restart their polling operation/event loop.

    ----------------------- -----------
    Option value type       int
    Option value unit       N/A (flags)
    Default value           N/A
    Applicable socket types all
    ----------------------- -----------

ZMQ_FD
  ~ *Retrieve file descriptor associated with the socket*.  The _ZMQ_FD_ option
    shall retrieve the file descriptor associated with the specified _socket_.
    The returned file descriptor can be used to integrate the socket into an
    existing event loop; the ØMQ library shall signal any pending events on the
    socket in an _edge-triggered_ fashion by making the file descriptor become
    ready for reading.

    > The ability to read from the returned file descriptor does not
    > necessarily indicate that messages are available to be read from, or can
    > be written to, the underlying socket; applications must retrieve the
    > actual event state with a subsequent retrieval of the _ZMQ_EVENTS_
    > option.

    > The returned file descriptor is also used internally by the *zmq_send()*
    > and *zmq_recv()* functions. As the descriptor is edge triggered,
    > applications must update the state of _ZMQ_EVENTS_ after each invocation
    > of *zmq_send()* or *zmq_recv()*.To be more explicit: after calling
    > *zmq_send()* the socket may become readable (and vice versa) without
    > triggering a read event on the file descriptor.

    The returned file descriptor is intended for use with a _poll_ or similar
    system call only. Applications must never attempt to read or write data
    to it directly, neither should they try to close it.

    ----------------------- ---------------------------------------
    Option value type       int on POSIX systems, SOCKET on Windows
    Option value unit       N/A
    Default value           N/A
    Applicable socket types all
    ----------------------- ---------------------------------------

ZMQ_GSSAPI_PLAINTEXT
  ~ *Retrieve GSSAPI plaintext or encrypted status*.  Returns the
    _ZMQ_GSSAPI_PLAINTEXT_ option, if any, previously set on the socket.  A
    value of `1` means  that communications will be plaintext.  A value
    of `0` means communications will be encrypted.

    ----------------------- -------------------------------------
    Option value type       int
    Option value unit       0, 1
    Default value           0 (false)
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

ZMQ_GSSAPI_PRINCIPAL
  ~ *Retrieve the name of the GSSAPI principal*.  The _ZMQ_GSSAPI_PRINCIPAL_
    option shall retrieve the principal name set for the GSSAPI security
    mechanism. The returned value shall be a NULL-terminated string and MAY be
    empty. The returned size SHALL include the terminating null byte.

    ----------------------- -------------------------------------
    Option value type       NULL-terminated character string
    Option value unit       N/A
    Default value           null string
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

ZMQ_GSSAPI_SERVER
  ~ *Retrieve current GSSAPI server role*.  Returns the _ZMQ_GSSAPI_SERVER_
    option, if any, previously set on the socket.

    ----------------------- -------------------------------------
    Option value type       int
    Option value unit       0, 1
    Default value           0 (false)
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

ZMQ_GSSAPI_SERVICE_PRINCIPAL
  ~ *Retrieve the name of the GSSAPI service principal*.  The
    _ZMQ_GSSAPI_SERVICE_PRINCIPAL_ option shall retrieve the principal name of
    the GSSAPI server to which a GSSAPI client socket intends to connect.  The
    returned value shall be a NULL-terminated string and MAY be empty. The
    returned size SHALL include the terminating null byte.

    ----------------------- -------------------------------------
    Option value type       NULL-terminated character string
    Option value unit       N/A
    Default value           null string
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

ZMQ_HANDSHAKE_IVL
  ~ *Retrieve maximum handshake interval*.  The _ZMQ_HANDSHAKE_IVL_ option
    shall retrieve the maximum handshake interval for the specified _socket_.
    Handshaking is the exchange of socket configuration information (socket
    type, identity, security) that occurs when a connection is first opened,
    only for connection-oriented transports. If handshaking does not complete
    within the configured time, the connection shall be closed.  The value 0
    means no handshake time limit.

    ----------------------- -----------------------------------------------------------
    Option value type       int
    Option value unit       milliseconds
    Default value           30000
    Applicable socket types all but ZMQ_STREAM, only for connection-oriented transports
    ----------------------- -----------------------------------------------------------

ZMQ_IDENTITY
  ~ *Retrieve socket identity*.  The _ZMQ_IDENTITY_ option shall retrieve the
    identity of the specified _socket_.  Socket identity is used only by
    request/reply pattern. Namely, it can be used in tandem with ROUTER socket
    to route messages to the peer with specific identity.

    Identity should be at least one byte and at most 255 bytes long. Identities
    starting with binary zero are reserved for use by ØMQ infrastructure.

    ----------------------- ----------------------------------------
    Option value type       binary data
    Option value unit       N/A
    Default value           NULL
    Applicable socket types ZMQ_REP, ZMQ_REQ, ZMQ_ROUTER, ZMQ_DEALER
    ----------------------- ----------------------------------------

ZMQ_IMMEDIATE
  ~ *Retrieve attach-on-connect value*.  Retrieve the state of the attach on
    connect value. If set to `1`, will delay the attachment of a pipe on
    connect until the underlying connection has completed.  This will cause the
    socket to block if there are no other connections, but will prevent queues
    from filling on pipes awaiting connection.

    ----------------------- --------------------------------------------
    Option value type       int
    Option value unit       boolean
    Default value           0 (false)
    Applicable socket types all, primarily when using TCP/IPC transports
    ----------------------- --------------------------------------------

ZMQ_IPV4ONLY
  ~ *Retrieve IPv4-only socket override status*.  Retrieve the IPv4-only option
    for the socket. This option is deprecated. Please use the _ZMQ_IPV6_
    option.

    ----------------------- ------------------------------
    Option value type       int
    Option value unit       boolean
    Default value           1 (true)
    Applicable socket types all, when using TCP transports
    ----------------------- ------------------------------

ZMQ_IPV6
  ~ *Retrieve IPv6 socket status*.  Retrieve the IPv6 option for the socket. A
    value of `1` means IPv6 is enabled on the socket, while `0` means the
    socket will use only IPv4.  When IPv6 is enabled the socket will connect
    to, or accept connections from, both IPv4 and IPv6 hosts.

    ----------------------- ------------------------------
    Option value type       int
    Option value unit       boolean
    Default value           0 (false)
    Applicable socket types all, when using TCP transports
    ----------------------- ------------------------------

ZMQ_LAST_ENDPOINT
  ~ *Retrieve the last endpoint set*.  The _ZMQ_LAST_ENDPOINT_ option shall
    retrieve the last endpoint bound for TCP and IPC transports. The returned
    value will be a string in the form of a ZMQ DSN. Note that if the TCP host
    is _INADDR_ANY_, indicated by a `*`, then the returned address will be
    `0.0.0.0` (for IPv4).

    ----------------------- ---------------------------------------
    Option value type       NULL-terminated character string
    Option value unit       N/A
    Default value           NULL
    Applicable socket types all, when binding TCP or IPC transports
    ----------------------- ---------------------------------------

ZMQ_LINGER
  ~ *Retrieve linger period for socket shutdown*.  The _ZMQ_LINGER_ option
    shall retrieve the linger period for the specified _socket_.  The linger
    period determines how long pending messages which have yet to be sent to a
    peer shall linger in memory after a socket is closed with [zmq_close][],
    and further affects the termination of the socket's context with
    [zmq_ctx_term][]. The following outlines the different behaviours:
      * The default value of `-1` specifies an infinite linger period. Pending
        messages shall not be discarded after a call to *zmq_close()*;
        attempting to terminate the socket's context with *zmq_ctx_term()*
        shall block until all pending messages have been sent to a peer.
      * The value of `0` specifies no linger period. Pending messages shall be
        discarded immediately when the socket is closed with *zmq_close()*.
      * Positive values specify an upper bound for the linger period in
        milliseconds.  Pending messages shall not be discarded after a call to
        *zmq_close()*; attempting to terminate the socket's context with
        *zmq_ctx_term()* shall block until either all pending messages have
        been sent to a peer, or the linger period expires, after which any
        pending messages shall be discarded.

    ----------------------- -------------
    Option value type       int
    Option value unit       milliseconds
    Default value           -1 (infinite)
    Applicable socket types all
    ----------------------- -------------

ZMQ_MAXMSGSIZE
  ~ *Maximum acceptable inbound message size*.  The option shall retrieve limit
    for the inbound messages. If a peer sends a message larger than
    _ZMQ_MAXMSGSIZE_ it is disconnected. Value of `-1` means _no limit_.

    ----------------------- -------
    Option value type       int64_t
    Option value unit       bytes
    Default value           -1
    Applicable socket types all
    ----------------------- -------

ZMQ_MECHANISM
  ~ *Retrieve current security mechanism*.  The _ZMQ_MECHANISM_ option shall
    retrieve the current security mechanism for the socket.

    ----------------------- ---------------------------------------------
    Option value type       int
    Option value unit       ZMQ_NULL, ZMQ_PLAIN, ZMQ_CURVE, or ZMQ_GSSAPI
    Default value           ZMQ_NULL
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- ---------------------------------------------

ZMQ_MULTICAST_HOPS
  ~ *Maximum network hops for multicast packets*.  The option shall retrieve
    time-to-live used for outbound multicast packets.  The default of `1` means
    that the multicast packets don't leave the local network.

    ----------------------- ------------------------------------
    Option value type       int
    Option value unit       network hops
    Default value           1
    Applicable socket types all, when using multicast transports
    ----------------------- ------------------------------------

ZMQ_PLAIN_PASSWORD
  ~ *Retrieve current password*.  The _ZMQ_PLAIN_PASSWORD_ option shall
    retrieve the last password set for the PLAIN security mechanism. The
    returned value shall be a NULL-terminated string and MAY be empty. The
    returned size SHALL include the terminating null byte.

    ----------------------- -------------------------------------
    Option value type       NULL-terminated character string
    Option value unit       N/A
    Default value           null string
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

ZMQ_PLAIN_SERVER
  ~ *Retrieve current PLAIN server role*.  Returns the _ZMQ_PLAIN_SERVER_
    option, if any, previously set on the socket.

    ----------------------- -------------------------------------
    Option value type       int
    Option value unit       0, 1
    Default value           int
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

ZMQ_PLAIN_USERNAME
  ~ *Retrieve current PLAIN username*.  The _ZMQ_PLAIN_USERNAME_ option shall
    retrieve the last username set for the PLAIN security mechanism. The
    returned value shall be a NULL-terminated string and MAY be empty. The
    returned size SHALL include the terminating null byte.

    ----------------------- -------------------------------------
    Option value type       NULL-terminated character string
    Option value unit       N/A
    Default value           null string
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

ZMQ_RATE
  ~ *Retrieve multicast data rate*.  The _ZMQ_RATE_ option shall retrieve the
    maximum send or receive data rate for multicast transports using the
    specified _socket_.

    ----------------------- ------------------------------------
    Option value type       int
    Option value unit       kilobits per second
    Default value           100
    Applicable socket types all, when using multicast transports
    ----------------------- ------------------------------------

ZMQ_RCVBUF
  ~ *Retrieve kernel receive buffer size*.  The _ZMQ_RCVBUF_ option shall
    retrieve the underlying kernel receive buffer size for the specified
    _socket_. A value of zero means that the OS default is in effect. For
    details refer to your operating system documentation for the _SO_RCVBUF_
    socket option.

    ----------------------- ------
    Option value type       int
    Option value unit       bytes
    Default value           0
    Applicable socket types all
    ----------------------- ------

ZMQ_RCVHWM
  ~ *Retrieve high water mark for inbound messages*.  The _ZMQ_RCVHWM_ option
    shall return the high water mark for inbound messages on the specified
    _socket_. The high water mark is a hard limit on the maximum number of
    outstanding messages ØMQ shall queue in memory for any single peer that the
    specified _socket_ is communicating with. A value of zero means no limit.

    If this limit has been reached the socket shall enter an exceptional state
    and depending on the socket type, ØMQ shall take appropriate action such as
    blocking or dropping sent messages. Refer to the individual socket
    descriptions in [zmq_socket][] for details on the exact action taken for
    each socket type.

    ----------------------- --------
    Option value type       int
    Option value unit       messages
    Default value           1000
    Applicable socket types all
    ----------------------- --------

ZMQ_RCVMORE
  ~ *More message data parts to follow*.  The _ZMQ_RCVMORE_ option shall return
    `.TRUE.` if the message part last received from the _socket_ was a data
    part with more parts to follow. If there are no data parts to follow, this
    option shall return `.FALSE.`.

    Refer to [zmq_send][] and [zmq_recv][] for a detailed description of
    multi-part messages.

    ----------------------- -------
    Option value type       int
    Option value unit       boolean
    Default value           N/A
    Applicable socket types all
    ----------------------- -------

ZMQ_RCVTIMEO
  ~ *Maximum time before a socket operation returns with EAGAIN*.  Retrieve the
    timeout for recv operation on the socket.  If the value is `0`,
    *zmq_recv()* will return immediately, with a EAGAIN error if there is no
    message to receive. If the value is `-1`, it will block until a message is
    available. For all other values, it will wait for a message for that amount
    of time before returning with an EAGAIN error.

    ----------------------- -------------
    Option value type       int
    Option value unit       milliseconds
    Default value           -1 (infinite)
    Applicable socket types all
    ----------------------- -------------

ZMQ_RECONNECT_IVL
  ~ *Retrieve reconnection interval*.  The _ZMQ_RECONNECT_IVL_ option shall
    retrieve the initial reconnection interval for the specified _socket_.  The
    reconnection interval is the period ØMQ shall wait between attempts to
    reconnect disconnected peers when using connection-oriented transports. The
    value `-1` means no reconnection.

    > The reconnection interval may be randomized by ØMQ to prevent
    > reconnection storms in topologies with a large number of peers per
    > socket.

    ----------------------- --------------------------------------------
    Option value type       int
    Option value unit       milliseconds
    Default value           100
    Applicable socket types all, only for connection-oriented transports
    ----------------------- --------------------------------------------

ZMQ_RECONNECT_IVL_MAX
  ~ *Retrieve maximum reconnection interval*.  The _ZMQ_RECONNECT_IVL_MAX_
    option shall retrieve the maximum reconnection interval for the specified
    _socket_.  This is the maximum period ØMQ shall wait between attempts to
    reconnect. On each reconnect attempt, the previous interval shall be
    doubled until _ZMQ_RECONNECT_IVL_MAX_ is reached. This allows for
    exponential backoff strategy. Default value means no exponential backoff is
    performed and reconnect interval calculations are only based on
    _ZMQ_RECONNECT_IVL_.

    > Values less than _ZMQ_RECONNECT_IVL_ will be ignored.

    ----------------------- -------------------------------------------
    Option value type       int
    Option value unit       milliseconds
    Default value           0 (only use _ZMQ_RECONNECT_IVL_)
    Applicable socket types all, only for connection-oriented transport
    ----------------------- -------------------------------------------

ZMQ_RECOVERY_IVL
  ~ *Get multicast recovery interval*.  The _ZMQ_RECOVERY_IVL_ option shall
    retrieve the recovery interval for multicast transports using the specified
    _socket_.  The recovery interval determines the maximum time in
    milliseconds that a receiver can be absent from a multicast group before
    unrecoverable data loss will occur.

    ----------------------- ------------------------------------
    Option value type       int
    Option value unit       milliseconds
    Default value           10000
    Applicable socket types all, when using multicast transports
    ----------------------- ------------------------------------

ZMQ_SNDBUF
  ~ *Retrieve kernel transmit buffer size*.  The _ZMQ_SNDBUF_ option shall
    retrieve the underlying kernel transmit buffer size for the specified
    _socket_. A value of zero means that the OS default is in effect. For
    details refer to your operating system documentation for the _SO_SNDBUF_
    socket option.

    ----------------------- -----
    Option value type       int
    Option value unit       bytes
    Default value           0
    Applicable socket types all
    ----------------------- -----

ZMQ_SNDHWM
  ~ *Retrieves high water mark for outbound messages*.  The _ZMQ_SNDHWM_ option
    shall return the high water mark for outbound messages on the specified
    _socket_. The high water mark is a hard limit on the maximum number of
    outstanding messages ØMQ shall queue in memory for any single peer that the
    specified _socket_ is communicating with. A value of zero means no limit.

    If this limit has been reached the socket shall enter an exceptional state
    and depending on the socket type, ØMQ shall take appropriate action such as
    blocking or dropping sent messages. Refer to the individual socket
    descriptions in [zmq_socket][] for details on the exact action taken for
    each socket type.

    ----------------------- --------
    Option value type       int
    Option value unit       messages
    Default value           1000
    Applicable socket types all
    ----------------------- --------

ZMQ_SNDTIMEO
  ~ *Maximum time before a socket operation returns with EAGAIN*.  Retrieve the
    timeout for send operation on the socket. If the value is `0`, *zmq_send()*
    will return immediately, with a EAGAIN error if the message cannot be sent.
    If the value is `-1`, it will block until the message is sent.  For all
    other values, it will try to send the message for that amount of time
    before returning with an EAGAIN error.

    ----------------------- -------------
    Option value type       int
    Option value unit       milliseconds
    Default value           -1 (infinite)
    Applicable socket types all
    ----------------------- -------------

ZMQ_TCP_KEEPALIVE
  ~ *Override SO_KEEPALIVE socket option*.  Override _SO_KEEPALIVE_ socket
    option (where supported by OS).  The default value of `-1` means to skip
    any overrides and leave it to OS default.

    ----------------------- ------------------------------
    Option value type       int
    Option value unit       -1,0,1
    Default value           -1 (leave to OS default)
    Applicable socket types all, when using TCP transports
    ----------------------- ------------------------------

ZMQ_TCP_KEEPALIVE_CNT
  ~ *Override TCP_KEEPCNT socket option*.  Override _TCP_KEEPCNT_ socket
    option (where supported by OS).  The default value of `-1` means to skip
    any overrides and leave it to OS default.

    ----------------------- ------------------------------
    Option value type       int
    Option value unit       -1,>0
    Default value           -1 (leave to OS default)
    Applicable socket types all, when using TCP transports
    ----------------------- ------------------------------

ZMQ_TCP_KEEPALIVE_IDLE
  ~ *Override TCP_KEEPCNT(or TCP_KEEPALIVE on some OS)*.  Override
    _TCP_KEEPCNT_ (or _TCP_KEEPALIVE_ on some OS) socket option (where
    supported by OS). The default value of `-1` means to skip any overrides and
    leave it to OS default.

    ----------------------- ------------------------------
    Option value type       int
    Option value unit       -1,>0
    Default value           -1 (leave to OS default)
    Applicable socket types all, when using TCP transports
    ----------------------- ------------------------------

ZMQ_TCP_KEEPALIVE_INTVL
  ~ *Override TCP_KEEPINTVL socket option*.  Override _TCP_KEEPINTVL_ socket
    option (where supported by OS).  The default value of `-1` means to skip
    any overrides and leave it to OS default.

    ----------------------- ------------------------------
    Option value type       int
    Option value unit       -1,>0
    Default value           -1 (leave to OS default)
    Applicable socket types all, when using TCP transports
    ----------------------- ------------------------------

ZMQ_TOS
  ~ *Retrieve the Type-of-Service socket override status*.  Retrieve the
    _IP_TOS_ option for the socket.

    ----------------------- --------------------------------------------
    Option value type       int
    Option value unit       >0
    Default value           0
    Applicable socket types all, only for connection-oriented transports
    ----------------------- --------------------------------------------

ZMQ_TYPE
  ~ *Retrieve socket type*.  The _ZMQ_TYPE_ option shall retrieve the socket
    type for the specified _socket_.  The socket type is specified at socket
    creation time and cannot be modified afterwards.

    ----------------------- ---
    Option value type       int
    Option value unit       N/A
    Default value           N/A
    Applicable socket types all
    ----------------------- ---

ZMQ_ZAP_DOMAIN
  ~ *Retrieve RFC 27 authentication domain*.  The _ZMQ_ZAP_DOMAIN_ option shall
    retrieve the last ZAP domain set for the socket. The returned value shall
    be a NULL-terminated string and MAY be empty. The returned size SHALL
    include the terminating null byte.

    ----------------------- -----------------------------
    Option value type       character string
    Option value unit       N/A
    Default value           not set
    Applicable socket types all, when using TCP transport
    ----------------------- -----------------------------


Return value
------------

The *zmq_getsockopt()* function shall return zero if successful. Otherwise it
shall return `-1` and set _errno_ to one of the values defined below.


Errors
------

EINVAL
  ~ The requested option _option_name_ is unknown, or the requested
    _option_len_ or _option_value_ is invalid, or the size of the buffer
    pointed to by _option_value_, as specified by _option_len_, is insufficient
    for storing the option value.

ETERM
  ~ The ØMQ _context_ associated with the specified _socket_ was terminated.

ENOTSOCK
  ~ The provided _socket_ was invalid.

EINTR
  ~ The operation was interrupted by delivery of a signal.


Example
-------

### Retrieving the high water mark for outgoing messages

~~~{.example}
INTEGER(KIND = C_INT) :: sndhwm
rc = zmq_getsockopt(socket, ZMQ_SNDHWM, sndhwm)
~~~


See also
--------

[zmq_setsockopt][]
[zmq_socket][]
