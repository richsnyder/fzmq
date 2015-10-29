% zmq_setsockopt


Name
----

zmq_setsockopt - set ØMQ socket options


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_setsockopt(socket, option_name, bin_value, option_len) RESULT(code)
FUNCTION zmq_setsockopt(socket, option_name, i32_value) RESULT(code)
FUNCTION zmq_setsockopt(socket, option_name, i64_value) RESULT(code)
FUNCTION zmq_setsockopt(socket, option_name, str_value) RESULT(code)

  TYPE(C_PTR), INTENT(IN) :: socket
  INTEGER(KIND = C_INT), INTENT(IN) :: option_name
  TYPE(C_PTR), INTENT(IN) :: bin_value
  INTEGER(KIND = C_SIZE_T), INTENT(IN) :: option_len
  INTEGER(KIND = C_INT), INTENT(IN) :: i32_value
  INTEGER(KIND = C_INT64_T), INTENT(IN) :: i64_value
  CHARACTER(LEN = *), INTENT(IN) :: str_value
  INTEGER(KIND = C_INT) :: code
~~~

All options, with the exception of *ZMQ_SUBSCRIBE*, *ZMQ_UNSUBSCRIBE*,
*ZMQ_LINGER*, *ZMQ_ROUTER_HANDOVER*, *ZMQ_ROUTER_MANDATORY*,
*ZMQ_PROBE_ROUTER*, *ZMQ_XPUB_VERBOSE*, *ZMQ_REQ_CORRELATE*, and
*ZMQ_REQ_RELAXED*, only take effect for subsequent socket bind/connects.
Specifically, security options take effect for subsequent bind/connect calls,
and can be changed at any time to affect subsequent binds and/or connects.


Description
-----------

The *zmq_setsockopt()* function shall set the option specified by the
_option_name_ argument to the value pointed to by the given _*_value_ argument
for the ØMQ socket pointed to by the _socket_ argument.  

#### Overloaded forms

This function is overloaded for different option types.

*zmq_setsockopt(socket, option_name, bin_value, option_len)*
  ~ *Set binary data option*.  This form can be used to set options of any
    type, but is the required form for binary data options.  The _option_len_
    argument is the size of the option value in bytes.  When used to set a
    character string option, _bin_value_ must contain a trailing `C_NULL_CHAR`
    and this character must be counted in _option_len_.

*zmq_setsockopt(socket, option_name, i32_value)*
  ~ *Set integer option*.  This form can be used to set integer options.

*zmq_setsockopt(socket, option_name, i64_value)*
  ~ *Set 64-bit integer option*.  This form sets the value of a 64-bit signed
    integer option.

*zmq_setsockopt(socket, option_name, str_value)*
  ~ *Set character string option*.  This form can be used to set a character
    string option.  The string _should not_ contain a trailing `C_NULL_CHAR`.

#### Socket options

The following socket options can be set with the *zmq_setsockopt()* function:

*ZMQ_AFFINITY*
  ~ *Set I/O thread affinity*.  The *ZMQ_AFFINITY* option shall set the I/O
    thread affinity for newly created connections on the specified _socket_.

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

*ZMQ_BACKLOG*
  ~ *Set maximum length of the queue of outstanding connections*.  The
    *ZMQ_BACKLOG* option shall set the maximum length of the queue of
    outstanding peer connections for the specified _socket_; this only applies
    to connection-oriented transports.  For details refer to your operating
    system documentation for the *listen* function.

    ----------------------- --------------------------------------------
    Option value type       int
    Option value unit       connections
    Default value           100
    Applicable socket types all, only for connection-oriented transports
    ----------------------- --------------------------------------------

*ZMQ_CONNECT_RID*
  ~ *Assign the next outbound connection id*.  The *ZMQ_CONNECT_RID* option
    sets the peer id of the next host connected via the *zmq_connect()* call,
    and immediately readies that connection for data transfer with the named
    id.  This option applies only to the first subsequent call to
    *zmq_connect()*, calls thereafter use default connection behavior.

    Typical use is to set this socket option ahead of each *zmq_connect()*
    attempt to a new host.  Each connection MUST be assigned a unique name.
    Assigning a name that is already in use is not allowed.

    Useful when connecting ROUTER to ROUTER, or STREAM to STREAM, as it allows
    for immediate sending to peers.  Outbound id framing requirements for
    ROUTER and STREAM sockets apply.

    The peer id should be from 1 to 255 bytes long and MAY NOT start with
    binary zero.

    ----------------------- ----------------------
    Option value type       binary data
    Option value unit       N/A
    Default value           NULL
    Applicable socket types ZMQ_ROUTER, ZMQ_STREAM
    ----------------------- ----------------------

*ZMQ_CONFLATE*
  ~ *Keep only last message*.  If set, a socket shall keep only one message in
    its inbound/outbound queue, this message being the last message
    received/the last message to be sent.  Ignores *ZMQ_RCVHWM* and
    *ZMQ_SNDHWM* options.  Does not support multi-part messages as only one
    part is kept in the socket internal queue.

    ----------------------- ------------------------------------------------
    Option value type       int
    Option value unit       boolean
    Default value           0 (false)
    Applicable socket types ZMQ_PULL, ZMQ_PUSH, ZMQ_SUB, ZMQ_PUB, ZMQ_DEALER
    ----------------------- ------------------------------------------------

*ZMQ_CURVE_PUBLICKEY*
  ~ *Set CURVE public key*.  Sets the socket's long term public key.  You must
    set this on CURVE client sockets, see [zmq_curve][].  You can provide the
    key as 32 binary bytes, or as a 40-character string encoded in the Z85
    encoding format and terminated in a null byte.  The public key must always
    be used with the matching secret key.  To generate a public/secret key
    pair, use [zmq_curve_keypair][].

    An option value size of 40 is supported for backwards compatibility, though
    it is deprecated.

    ----------------------- ------------------------------
    Option value type       binary data or Z85 text string
    Option value size       32 or 41
    Default value           null
    Applicable socket types all, when using TCP transport
    ----------------------- ------------------------------

ZMQ_CURVE_SECRETKEY
  ~ *Set CURVE secret key*.  Sets the socket's long term secret key.  You must
    set this on both CURVE client and server sockets, see [zmq_curve][].  You
    can provide the key as 32 binary bytes, or as a 40-character string encoded
    in the Z85 encoding format and terminated in a null byte.  To generate a
    public/secret key pair, use [zmq_curve_keypair][].

    An option value size of 40 is supported for backwards compatibility,
    though is deprecated.

    ----------------------- ------------------------------
    Option value type       binary data or Z85 text string
    Option value size       32 or 41
    Default value           null
    Applicable socket types all, when using TCP transport
    ----------------------- ------------------------------

*ZMQ_CURVE_SERVER*
  ~ *Set CURVE server role*.  Defines whether the socket will act as server for
    CURVE security, see [zmq_curve][].  A value of `1` means the socket will
    act as CURVE server. A value of `0` means the socket will not act as CURVE
    server, and its security role then depends on other option settings.
    Setting this to `0` shall reset the socket security to NULL.  When you set
    this you must also set the server's secret key using the
    *ZMQ_CURVE_SECRETKEY* option.  A server socket does not need to know its
    own public key.

    ----------------------- -----------------------------
    Option value type       int
    Option value unit       0, 1
    Default value           0
    Applicable socket types all, when using TCP transport
    ----------------------- -----------------------------

*ZMQ_CURVE_SERVERKEY*
  ~ *Set CURVE server key*.  Sets the socket's long term server key.  You must
    set this on CURVE client sockets, see [zmq_curve][].  You can provide the
    key as 32 binary bytes, or as a 40-character string encoded in the Z85
    encoding format and terminated in a null byte.  This key must have been
    generated together with the server's secret key.  To generate a
    public/secret key pair, use [zmq_curve_keypair][].

    An option value size of 40 is supported for backwards compatibility,
    though is deprecated.

    ----------------------- ------------------------------
    Option value type       binary data or Z85 text string
    Option value size       32 or 41
    Default value           null
    Applicable socket types all, when using TCP transport
    ----------------------- ------------------------------

*ZMQ_GSSAPI_PLAINTEXT*
  ~ *Disable GSSAPI encryption*.  Defines whether communications on the socket
    will encrypted, see [zmq_gssapi][].  A value of `1` means  that
    communications will be plaintext.  A value of `0` means communications will
    be encrypted.

    ----------------------- -------------------------------------
    Option value type       int
    Option value unit       0, 1
    Default value           0 (false)
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

*ZMQ_GSSAPI_PRINCIPAL*
  ~ *Set name of GSSAPI principal*.  Sets the name of the pricipal for whom
    GSSAPI credentials should be acquired.

    ----------------------- -------------------------------------
    Option value type       character string
    Option value unit       N/A
    Default value           not set
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

*ZMQ_GSSAPI_SERVER*
  ~ *Set GSSAPI server role*.  Defines whether the socket will act as server
    for GSSAPI security, see [zmq_gssapi][].  A value of `1` means the socket
    will act as GSSAPI server.  A value of `0` means the socket will act as
    GSSAPI client.

    ----------------------- -------------------------------------
    Option value type       int
    Option value unit       0, 1
    Default value           0 (false)
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

*ZMQ_GSSAPI_SERVICE_PRINCIPAL*
  ~ *Set name of GSSAPI service principal*.  Sets the name of the pricipal of
    the GSSAPI server to which a GSSAPI client intends to connect.

    ----------------------- -------------------------------------
    Option value type       character string
    Option value unit       N/A
    Default value           not set
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

*ZMQ_HANDSHAKE_IVL*
  ~ *Set maximum handshake interval*.  The *ZMQ_HANDSHAKE_IVL* option shall set
    the maximum handshake interval for the specified _socket_.  Handshaking is
    the exchange of socket configuration information (socket type, identity,
    security) that occurs when a connection is first opened, only for
    connection-oriented transports.  If handshaking does not complete within
    the configured time, the connection shall be closed.  The value `0` means
    no handshake time limit.

    ----------------------- -----------------------------------------------------------
    Option value type       int
    Option value unit       milliseconds
    Default value           30000
    Applicable socket types all but ZMQ_STREAM, only for connection-oriented transports
    ----------------------- -----------------------------------------------------------

*ZMQ_IDENTITY*
  ~ *Set socket identity*.  The *ZMQ_IDENTITY* option shall set the identity of
    the specified _socket_ when connecting to a ROUTER socket.  The identity
    should be from 1 to 255 bytes long and may contain any values.

    If two clients use the same identity when connecting to a ROUTER, the
    results shall depend on the *ZMQ_ROUTER_HANDOVER* option setting.  If that
    is not set (or set to the default of zero), the ROUTER socket shall reject
    clients trying to connect with an already-used identity.  If that option
    is set to `1`, the ROUTER socket shall hand-over the connection to the new
    client and disconnect the existing one.

    ----------------------- ----------------------------------------
    Option value type       binary data
    Option value unit       N/A
    Default value           NULL
    Applicable socket types ZMQ_REP, ZMQ_REQ, ZMQ_ROUTER, ZMQ_DEALER
    ----------------------- ----------------------------------------

*ZMQ_IMMEDIATE*
  ~ *Queue messages only to completed connections*.  By default queues will
    fill on outgoing connections even if the connection has not completed.
    This can lead to "lost" messages on sockets with round-robin routing (REQ,
    PUSH, DEALER).  If this option is set to `1`, messages shall be queued only
    to completed connections.  This will cause the socket to block if there are
    no other connections, but will prevent queues from filling on pipes
    awaiting connection.

    ----------------------- --------------------------------------------
    Option value type       int
    Option value unit       boolean
    Default value           0 (false)
    Applicable socket types all, primarily when using TCP/IPC transports
    ----------------------- --------------------------------------------

*ZMQ_IPV6*
  ~ *Enable IPv6 on socket*.  Set the IPv6 option for the socket.  A value of
    `1` means IPv6 is enabled on the socket, while `0` means the socket will
    use only IPv4.  When IPv6 is enabled the socket will connect to, or accept
    connections from, both IPv4 and IPv6 hosts.

    ----------------------- ------------------------------
    Option value type       int
    Option value unit       boolean
    Default value           0 (false)
    Applicable socket types all, when using TCP transports
    ----------------------- ------------------------------

*ZMQ_LINGER*
  ~ *Set linger period for socket shutdown*.  The *ZMQ_LINGER* option shall set
    the linger period for the specified _socket_.  The linger period determines
    how long pending messages which have yet to be sent to a peer shall linger
    in memory after a socket is disconnected with [zmq_disconnect][] or closed
    with [zmq_close][], and further affects the termination of the socket's
    context with [zmq_ctx_term][]. The following outlines the different
    behaviours:
      * The default value of `-1` specifies an infinite linger period.  Pending
        messages shall not be discarded after a call to *zmq_disconnect()* or
        *zmq_close()*; attempting to terminate the socket's context with
        *zmq_ctx_term()* shall block until all pending messages have been sent
        to a peer.
      * The value of `0` specifies no linger period.  Pending messages shall be
        discarded immediately after a call to *zmq_disconnect()* or
        *zmq_close()*.
      * Positive values specify an upper bound for the linger period in
        milliseconds.  Pending messages shall not be discarded after a call to
        *zmq_disconnect()* or *zmq_close()*; attempting to terminate the
        socket's context with *zmq_ctx_term()* shall block until either all
        pending messages have been sent to a peer, or the linger period
        expires, after which any pending messages shall be discarded.

    ----------------------- -------------
    Option value type       int
    Option value unit       milliseconds
    Default value           -1 (infinite)
    Applicable socket types all
    ----------------------- -------------

*ZMQ_MAXMSGSIZE*
  ~ *Maximum acceptable inbound message size*.  Limits the size of the inbound
    message.  If a peer sends a message larger than *ZMQ_MAXMSGSIZE* it is
    disconnected.  A value of `-1` means _no limit_.

    ----------------------- -------
    Option value type       int64_t
    Option value unit       bytes
    Default value           -1
    Applicable socket types all
    ----------------------- -------

*ZMQ_MULTICAST_HOPS*
  ~ *Maximum network hops for multicast packets*.  Sets the time-to-live field
    in every multicast packet sent from this socket.  The default is `1` which
    means that the multicast packets don't leave the local network.

    ----------------------- ------------------------------------
    Option value type       int
    Option value unit       network hops
    Default value           1
    Applicable socket types all, when using multicast transports
    ----------------------- ------------------------------------

*ZMQ_PLAIN_PASSWORD*
  ~ *Set PLAIN security password*.  Sets the password for outgoing connections
    over TCP or IPC.  If you set this to a non-null value, the security
    mechanism used for connections shall be PLAIN, see [zmq_plain][].  If you
    set this to a null value, the security mechanism used for connections shall
    be NULL, see [zmq_null][].

    ----------------------- -------------------------------------
    Option value type       NULL-terminated character string
    Option value unit       N/A
    Default value           not set
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

*ZMQ_PLAIN_SERVER*
  ~ *Set PLAIN server role*.  Defines whether the socket will act as server for
    PLAIN security, see [zmq_plain][]. nA value of `1` means the socket will
    act as a PLAIN server.  A value of `0` means the socket will not act as a
    PLAIN server, and its security role then depends on other option settings.
    Setting this to `0` shall reset the socket security to NULL.

    ----------------------- -------------------------------------
    Option value type       int
    Option value unit       0, 1
    Default value           int
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

*ZMQ_PLAIN_USERNAME*
  ~ *Set PLAIN security username*.  Sets the username for outgoing connections
    over TCP or IPC.  If you set this to a non-null value, the security
    mechanism used for connections shall be PLAIN, see [zmq_plain][].  If you
    set this to a null value, the security mechanism used for connections shall
    be NULL, see [zmq_null][].

    ----------------------- -------------------------------------
    Option value type       character string
    Option value unit       N/A
    Default value           not set
    Applicable socket types all, when using TCP or IPC transports
    ----------------------- -------------------------------------

*ZMQ_PROBE_ROUTER*
  ~ *Bootstrap connections to ROUTER sockets*.  When set to `1`, the socket
    will automatically send an empty message when a new connection is made or
    accepted.  You may set this on REQ, DEALER, or ROUTER sockets connected to
    a ROUTER socket.  The application must filter such empty messages.  The
    *ZMQ_PROBE_ROUTER* option in effect provides the ROUTER application with an
    event signaling the arrival of a new peer.

    Do not set this option on a socket that talks to any other socket types:
    the results are undefined.

    ----------------------- -------------------------------
    Option value type       int
    Option value unit       0, 1
    Default value           0
    Applicable socket types ZMQ_ROUTER, ZMQ_DEALER, ZMQ_REQ
    ----------------------- -------------------------------

*ZMQ_RATE*
  ~ *Set multicast data rate*.  The *ZMQ_RATE* option shall set the maximum
    send or receive data rate for multicast transports such as [zmq_pgm][]
    using the specified _socket_.

    ----------------------- ------------------------------------
    Option value type       int
    Option value unit       kilobits per second
    Default value           100
    Applicable socket types all, when using multicast transports
    ----------------------- ------------------------------------

*ZMQ_RCVBUF*
  ~ *Set kernel receive buffer size*.  The *ZMQ_RCVBUF* option shall set the
    underlying kernel receive buffer size for the _socket_ to the specified
    size in bytes.  A value of zero means leave the OS default unchanged.  For
    details refer to your operating system documentation for the *SO_RCVBUF*
    socket option.

    ----------------------- ------
    Option value type       int
    Option value unit       bytes
    Default value           0
    Applicable socket types all
    ----------------------- ------

*ZMQ_RCVHWM*
  ~ *Set high water mark for inbound messages*.  The *ZMQ_RCVHWM* option shall
    set the high water mark for inbound messages on the specified _socket_.
    The high water mark is a hard limit on the maximum number of outstanding
    messages ØMQ shall queue in memory for any single peer that the specified
    _socket_ is communicating with.  A value of zero means no limit.

    If this limit has been reached the socket shall enter an exceptional state
    and depending on the socket type, ØMQ shall take appropriate action such as
    blocking or dropping sent messages.  Refer to the individual socket
    descriptions in [zmq_socket][] for details on the exact action taken for
    each socket type.

    ----------------------- --------
    Option value type       int
    Option value unit       messages
    Default value           1000
    Applicable socket types all
    ----------------------- --------

*ZMQ_RCVTIMEO*
  ~ *Maximum time before a recv operation returns with EAGAIN*.  Sets the
    timeout for receive operation on the socket.  If the value is `0`,
    *zmq_recv()* and *zmq_msg_recv()* will return immediately, with an *EAGAIN*
    error if there is no message to receive.  If the value is `-1`, it will
    block until a message is available.  For all other values, it will wait for
    a message for that amount of time before returning with an *EAGAIN* error.

    ----------------------- -------------
    Option value type       int
    Option value unit       milliseconds
    Default value           -1 (infinite)
    Applicable socket types all
    ----------------------- -------------

*ZMQ_RECONNECT_IVL*
  ~ *Set reconnection interval*.  The *ZMQ_RECONNECT_IVL* option shall set the
    initial reconnection interval for the specified _socket_.  The reconnection
    interval is the period ØMQ shall wait between attempts to reconnect
    disconnected peers when using connection-oriented transports.  The value
    `-1` means no reconnection.

    The reconnection interval may be randomized by ØMQ to prevent
    reconnection storms in topologies with a large number of peers per
    socket.

    ----------------------- --------------------------------------------
    Option value type       int
    Option value unit       milliseconds
    Default value           100
    Applicable socket types all, only for connection-oriented transports
    ----------------------- --------------------------------------------

*ZMQ_RECONNECT_IVL_MAX*
  ~ *Set maximum reconnection interval*.  The *ZMQ_RECONNECT_IVL_MAX* option
    shall set the maximum reconnection interval for the specified _socket_.  
    This is the maximum period ØMQ shall wait between attempts to reconnect.
    On each reconnect attempt, the previous interval shall be doubled until
    *ZMQ_RECONNECT_IVL_MAX* is reached.  This allows for exponential backoff
    strategy.  Default value means no exponential backoff is performed and
    reconnect interval calculations are only based on *ZMQ_RECONNECT_IVL*.

    Values less than *ZMQ_RECONNECT_IVL* will be ignored.

    ----------------------- -------------------------------------------
    Option value type       int
    Option value unit       milliseconds
    Default value           0 (only use *ZMQ_RECONNECT_IVL*)
    Applicable socket types all, only for connection-oriented transport
    ----------------------- -------------------------------------------

*ZMQ_RECOVERY_IVL*
  ~ *Set multicast recovery interval*.  The *ZMQ_RECOVERY_IVL* option shall set
    the recovery interval for multicast transports using the specified
    _socket_.  The recovery interval determines the maximum time in
    milliseconds that a receiver can be absent from a multicast group before
    unrecoverable data loss will occur.

    Exercise care when setting large recovery intervals as the data needed for
    recovery will be held in memory.  For example, a 1 minute recovery interval
    at a data rate of 1Gbps requires a 7GB in-memory buffer.

    ----------------------- ------------------------------------
    Option value type       int
    Option value unit       milliseconds
    Default value           10000
    Applicable socket types all, when using multicast transports
    ----------------------- ------------------------------------

*ZMQ_REQ_CORRELATE*
  ~ *Match replies with requests*.  The default behavior of REQ sockets is to
    rely on the ordering of messages to match requests and responses and that
    is usually sufficient.  When this option is set to `1`, the REQ socket will
    prefix outgoing messages with an extra frame containing a request id.  That
    means the full message is (request id, identity, 0, user frames...).  The
    REQ socket will discard all incoming messages that don't begin with these
    two frames.

    ----------------------- -------
    Option value type       int
    Option value unit       0, 1
    Default value           0
    Applicable socket types ZMQ_REQ
    ----------------------- -------

*ZMQ_REQ_RELAXED*
  ~ *Relax strict alternation between request and reply*.  By default, a REQ
    socket does not allow initiating a new request with *zmq_send()* or
    *zmq_msg_send()* until the reply to the previous one has been received.  
    When set to `1`, sending another message is allowed and has the effect of
    disconnecting the underlying connection to the peer from which the reply
    was expected, triggering a reconnection attempt on transports that support
    it.  The request-reply state machine is reset and a new request is sent to
    the next available peer.

    If set to `1`, also enable *ZMQ_REQ_CORRELATE* to ensure correct matching
    of requests and replies.  Otherwise a late reply to an aborted request can
    be reported as the reply to the superseding request.

    ----------------------- -------
    Option value type       int
    Option value unit       0, 1
    Default value           0
    Applicable socket types ZMQ_REQ
    ----------------------- -------

*ZMQ_ROUTER_HANDOVER*
  ~ *Handle duplicate client identities on ROUTER sockets*.  If two clients use
    the same identity when connecting to a ROUTER, the results shall depend on
    the *ZMQ_ROUTER_HANDOVER* option setting.  If that is not set (or set to
    the default of zero), the ROUTER socket shall reject clients trying to
    connect with an already-used identity.  If the option is set to `1`, the
    ROUTER socket shall hand-over the connection to the new client and
    disconnect the existing one.

    ----------------------- ----------
    Option value type       int
    Option value unit       0, 1
    Default value           0
    Applicable socket types ZMQ_ROUTER
    ----------------------- ----------

*ZMQ_ROUTER_MANDATORY*
  ~ *Accept only routable messages on ROUTER sockets*.  Sets the ROUTER socket
    behavior when an unroutable message is encountered.  A value of `0` is the
    default and discards the message silently when it cannot be routed or the
    peer's SNDHWM is reached.  A value of `1` returns an *EHOSTUNREACH* error
    code if the message cannot be routed or *EAGAIN* error code if the SNDHWM
    is reached and *ZMQ_DONTWAIT* was used.  Without *ZMQ_DONTWAIT* it will
    block until the SNDTIMEO is reached or a spot in the send queue opens up.

    ----------------------- ----------
    Option value type       int
    Option value unit       0, 1
    Default value           0
    Applicable socket types ZMQ_ROUTER
    ----------------------- ----------

*ZMQ_ROUTER_RAW*
  ~ *Switch ROUTER socket to raw mode*.  Sets the raw mode on the ROUTER, when
    set to `1`.  When the ROUTER socket is in raw mode, and when using the
    tcp:// transport, it will read and write TCP data without ØMQ framing.
    This lets ØMQ applications talk to non-ØMQ applications.  When using raw
    mode, you cannot set explicit identities, and the *ZMQ_SNDMORE* flag is
    ignored when sending data messages.  In raw mode you can close a specific
    connection by sending it a zero-length message (following the identity
    frame).

    This option is deprecated, please use *ZMQ_STREAM* sockets instead.

    ----------------------- ----------
    Option value type       int
    Option value unit       0, 1
    Default value           0
    Applicable socket types ZMQ_ROUTER
    ----------------------- ----------

*ZMQ_SNDBUF*
  ~ *Set kernel transmit buffer size*.  The *ZMQ_SNDBUF* option shall set the
    underlying kernel transmit buffer size for the _socket_ to the specified
    size in bytes.  A value of zero means leave the OS default unchanged.  For
    details please refer to your operating system documentation for the
    *SO_SNDBUF* socket option.

    ----------------------- -----
    Option value type       int
    Option value unit       bytes
    Default value           0
    Applicable socket types all
    ----------------------- -----

*ZMQ_SNDHWM*
  ~ *Set high water mark for outbound messages*.  The *ZMQ_SNDHWM* option shall
    set the high water mark for outbound messages on the specified _socket_.
    The high water mark is a hard limit on the maximum number of outstanding
    messages ØMQ shall queue in memory for any single peer that the specified
    _socket_ is communicating with.  A value of zero means no limit.

    If this limit has been reached the socket shall enter an exceptional state
    and depending on the socket type, ØMQ shall take appropriate action such as
    blocking or dropping sent messages.  Refer to the individual socket
    descriptions in [zmq_socket][] for details on the exact action taken for
    each socket type.

    ØMQ does not guarantee that the socket will accept as many as *ZMQ_SNDHWM*
    messages, and the actual limit may be as much as 60-70% lower depending on
    the flow of messages on the socket.

    ----------------------- --------
    Option value type       int
    Option value unit       messages
    Default value           1000
    Applicable socket types all
    ----------------------- --------

*ZMQ_SNDTIMEO*
  ~ *Maximum time before a send operation returns with EAGAIN*.  Sets the
    timeout for send operation on the socket. If the value is `0`, *zmq_send()*
    and *zmq_msg_send()* will return immediately, with a *EAGAIN* error if the
    message cannot be sent.  If the value is `-1`, it will block until the
    message is sent.  For all other values, it will try to send the message for
    that amount of time before returning with an *EAGAIN* error.

    ----------------------- -------------
    Option value type       int
    Option value unit       milliseconds
    Default value           -1 (infinite)
    Applicable socket types all
    ----------------------- -------------

*ZMQ_SUBSCRIBE*
  ~ *Establish message filter*.  The *ZMQ_SUBSCRIBE* option shall establish a
    new message filter on a *ZMQ_SUB* socket.  Newly created *ZMQ_SUB* sockets
    shall filter out all incoming messages, therefore you should call this
    option to establish an initial message filter.

    An empty _option_value_ of length zero shall subscribe to all incoming
    messages.  A non-empty _option_value_ shall subscribe to all messages
    beginning with the specified prefix.  Multiple filters may be attached to a
    single *ZMQ_SUB* socket, in which case a message shall be accepted if it
    matches at least one filter.

    ----------------------- -----------
    Option value type       binary data
    Option value unit       N/A
    Default value           N/A
    Applicable socket types ZMQ_SUB
    ----------------------- -----------

*ZMQ_TCP_KEEPALIVE*
  ~ *Override SO_KEEPALIVE socket option*.  Override *SO_KEEPALIVE* socket
    option (where supported by OS).  The default value of `-1` means to skip
    any overrides and leave it to OS default.

    ----------------------- ------------------------------
    Option value type       int
    Option value unit       -1,0,1
    Default value           -1 (leave to OS default)
    Applicable socket types all, when using TCP transports
    ----------------------- ------------------------------

*ZMQ_TCP_KEEPALIVE_CNT*
  ~ *Override TCP_KEEPCNT socket option*.  Override *TCP_KEEPCNT* socket option
    (where supported by OS).  The default value of `-1` means to skip any
    overrides and leave it to OS default.

    ----------------------- ------------------------------
    Option value type       int
    Option value unit       -1,>0
    Default value           -1 (leave to OS default)
    Applicable socket types all, when using TCP transports
    ----------------------- ------------------------------

*ZMQ_TCP_KEEPALIVE_IDLE*
  ~ *Override TCP_KEEPCNT (or TCP_KEEPALIVE on some OS)*.  Override
    *TCP_KEEPCNT* (or *TCP_KEEPALIVE* on some OS) socket option (where
    supported by OS).  The default value of `-1` means to skip any overrides
    and leave it to OS default.

    ----------------------- ------------------------------
    Option value type       int
    Option value unit       -1,>0
    Default value           -1 (leave to OS default)
    Applicable socket types all, when using TCP transports
    ----------------------- ------------------------------

*ZMQ_TCP_KEEPALIVE_INTVL*
  ~ *Override TCP_KEEPINTVL socket option*.  Override *TCP_KEEPINTVL* socket
    option (where supported by OS).  The default value of `-1` means to skip
    any overrides and leave it to OS default.

    ----------------------- ------------------------------
    Option value type       int
    Option value unit       -1,>0
    Default value           -1 (leave to OS default)
    Applicable socket types all, when using TCP transports
    ----------------------- ------------------------------

*ZMQ_TOS*
  ~ *Set the Type-of-Service on socket*.  Sets the ToS fields (Differentiated
    services (DS) and Explicit Congestion Notification (ECN) field of the IP
    header.  The ToS field is typically used to specify a packets priority.
    The availability of this option is dependent on intermediate network
    equipment that inspects the ToS field and provides a path for low-delay,
    high-throughput, highly-reliable service, etc.

    ----------------------- --------------------------------------------
    Option value type       int
    Option value unit       >0
    Default value           0
    Applicable socket types all, only for connection-oriented transports
    ----------------------- --------------------------------------------

*ZMQ_UNSUBSCRIBE*
  ~ *Remove message filter*.  The *ZMQ_UNSUBSCRIBE* option shall remove an
    existing message filter on a *ZMQ_SUB* socket.  The filter specified must
    match an existing filter previously established with the *ZMQ_SUBSCRIBE*
    option.  If the socket has several instances of the same filter attached
    the *ZMQ_UNSUBSCRIBE* option shall remove only one instance, leaving the
    rest in place and functional.

    ----------------------- -----------
    Option value type       binary data
    Option value unit       N/A
    Default value           N/A
    Applicable socket types ZMQ_SUB
    ----------------------- -----------

*ZMQ_XPUB_VERBOSE*
  ~ *Provide all subscription messages on XPUB sockets*.  Sets the *ZMQ_XPUB*
    socket behavior on new subscriptions and unsubscriptions.  A value of `0`
    is the default and passes only new subscription messages to upstream.  A
    value of `1` passes all subscription messages upstream.

    ----------------------- --------
    Option value type       int
    Option value unit       0, 1
    Default value           0
    Applicable socket types ZMQ_XPUB
    ----------------------- --------

*ZMQ_ZAP_DOMAIN*
  ~ *Set RFC 27 authentication domain*.  Sets the domain for ZAP (ZMQ RFC 27)
    authentication.  For NULL security (the default on all tcp:// connections),
    ZAP authentication only happens if you set a non-empty domain.  For PLAIN
    and CURVE security, ZAP requests are always made, if there is a ZAP handler
    present.  See <http://rfc.zeromq.org/spec:27> for more details.

    ----------------------- -----------------------------
    Option value type       character string
    Option value unit       N/A
    Default value           not set
    Applicable socket types all, when using TCP transport
    ----------------------- -----------------------------

*ZMQ_TCP_ACCEPT_FILTER*
  ~ *Assign filters to allow new TCP connections*.  Assign an arbitrary number
    of filters that will be applied for each new TCP transport connection on a
    listening socket.  If no filters are applied, then the TCP transport allows
    connections from any IP address.  If at least one filter is applied then
    new connection source ip should be matched.  To clear all filters call
    *zmq_setsockopt(socket, ZMQ_TCP_ACCEPT_FILTER, C_NULL_PTR, 0)*.  Filter is
    a null-terminated string with ipv6 or ipv4 CIDR.

    This option is deprecated, please use authentication via the ZAP API and IP
    address whitelisting / blacklisting.

    ----------------------- ------------------------------------------------
    Option value type       binary data
    Option value unit       N/A
    Default value           no filters (allow from all)
    Applicable socket types all listening sockets, when using TCP transports
    ----------------------- ------------------------------------------------

*ZMQ_IPC_FILTER_GID*
  ~ *Assign group ID filters to allow new IPC connections*.  Assign an
    arbitrary number of filters that will be applied for each new IPC transport
    connection on a listening socket.  If no IPC filters are applied, then the
    IPC transport allows connections from any process.  If at least one UID,
    GID, or PID filter is applied then new connection credentials should be
    matched.  To clear all GID filters call *zmq_setsockopt(socket,
    ZMQ_IPC_FILTER_GID, C_NULL_PTR, 0)*.

    GID filters are only available on platforms supporting *SO_PEERCRED* or
    *LOCAL_PEERCRED* socket options (currently only Linux and later versions
    of OS X).

    This option is deprecated, please use authentication via the ZAP API and
    IPC whitelisting / blacklisting.

    ----------------------- ------------------------------------------------
    Option value type       gid_t
    Option value unit       N/A
    Default value           no filters (allow from all)
    Applicable socket types all listening sockets, when using IPC transports
    ----------------------- ------------------------------------------------

*ZMQ_IPC_FILTER_PID*
  ~ *Assign process ID filters to allow new IPC connections*.  Assign an
    arbitrary number of filters that will be applied for each new IPC transport
    connection on a listening socket.  If no IPC filters are applied, then the
    IPC transport allows connections from any process.  If at least one UID,
    GID, or PID filter is applied then new connection credentials should be
    matched.  To clear all PID filters call *zmq_setsockopt(socket,
    ZMQ_IPC_FILTER_PID, C_NULL_PTR, 0)*.

    PID filters are only available on platforms supporting the *SO_PEERCRED*
    socket option (currently only Linux).

    This option is deprecated, please use authentication via the ZAP API and
    IPC whitelisting / blacklisting.

    ----------------------- ------------------------------------------------
    Option value type       pid_t
    Option value unit       N/A
    Default value           no filters (allow from all)
    Applicable socket types all listening sockets, when using IPC transports
    ----------------------- ------------------------------------------------

*ZMQ_IPC_FILTER_UID*
  ~ *Assign user ID filters to allow new IPC connections*.  Assign an arbitrary
    number of filters that will be applied for each new IPC transport
    connection on a listening socket.  If no IPC filters are applied, then the
    IPC transport allows connections from any process.  If at least one UID,
    GID, or PID filter is applied then new connection credentials should be
    matched.  To clear all UID filters call *zmq_setsockopt(socket,
    ZMQ_IPC_FILTER_UID, C_NULL_PTR, 0)*.

    UID filters are only available on platforms supporting *SO_PEERCRED* or
    *LOCAL_PEERCRED* socket options (currently only Linux and later versions
    of OS X).

    This option is deprecated, please use authentication via the ZAP API and
    IPC whitelisting / blacklisting.

    ----------------------- ------------------------------------------------
    Option value type       uid_t
    Option value unit       N/A
    Default value           no filters (allow from all)
    Applicable socket types all listening sockets, when using IPC transports
    ----------------------- ------------------------------------------------

*ZMQ_IPV4ONLY*
  ~ *Use IPv4-only on socket*.  Set the IPv4-only option for the socket. This
    option is deprecated.  Please use the *ZMQ_IPV6* option.

    ----------------------- ------------------------------
    Option value type       int
    Option value unit       boolean
    Default value           1 (true)
    Applicable socket types all, when using TCP transports
    ----------------------- ------------------------------


Return value
------------

The *zmq_setsockopt()* function shall return zero if successful.  Otherwise it
shall return `-1` and set _errno_ to one of the values defined below.


Errors
------

*EINVAL*
  ~ The requested option _option_name_ is unknown, or the requested
  _option_len_ or _option_value_ is invalid.

*ETERM*
  ~ The ØMQ _context_ associated with the specified _socket_ was terminated.

*ENOTSOCK*
  ~ The provided _socket_ was invalid.

*EINTR*
  ~ The operation was interrupted by delivery of a signal.


Example
-------

### Subscribing to messages on a *ZMQ_SUB* socket

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
INTEGER(KIND = C_INT) :: rc

context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_SUB)
rc = zmq_setsockopt(socket, ZMQ_SUBSCRIBE, '')
rc = zmq_setsockopt(socket, ZMQ_SUBSCRIBE, 'ANIMALS.CATS')
rc = zmq_close(socket)
rc = zmq_ctx_term(context)
~~~

### Set socket identity

~~~{.example}
TYPE(C_PTR) :: context
TYPE(C_PTR) :: socket
INTEGER(KIND = C_INT) :: rc
INTEGER(KIND = C_SIZE_T) :: option_len
CHARACTER(KIND = C_CHAR, LEN = 8), TARGET :: id

id = 'fzmq'
option_len = 4_C_SIZE_T
context = zmq_ctx_new()
socket = zmq_socket(context, ZMQ_REQ)
rc = zmq_setsockopt(socket, ZMQ_IDENTITY, C_LOC(id), option_len)
rc = zmq_close(socket)
rc = zmq_ctx_term(context)
~~~


See also
--------

[zmq_getsockopt][]
[zmq_socket][]
[zmq_plain][]
[zmq_curve][]
[fzmq][]
