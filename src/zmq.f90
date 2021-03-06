! DISTRIBUTION STATEMENT A. Approved for public release; distribution is
! unlimited.  Granted clearance per 88ABW-2015-5731.
!
! This file is declared a work of the U.S. Government and is not subject to
! copyright protection in the United States.

MODULE ZMQ

  USE, INTRINSIC :: ISO_C_BINDING, ONLY : &
      C_ASSOCIATED, C_CHAR, C_F_POINTER, C_FUNPTR, C_INT, C_INT16_T, &
      C_INT32_T, C_INT64_T, C_LOC, C_LONG, C_NULL_CHAR, C_PTR, C_SHORT, &
      C_SIGNED_CHAR, C_SIZE_T, C_SIZEOF
  IMPLICIT NONE
  PRIVATE

  TYPE, PUBLIC, BIND(C) :: ZMQ_EVENT_T
    INTEGER(KIND = C_INT16_T) :: EVENT
    INTEGER(KIND = C_INT32_T) :: VALUE
  END TYPE

  TYPE, PUBLIC, BIND(C) :: ZMQ_MSG_T
    INTEGER(KIND = C_SIGNED_CHAR), DIMENSION(64) :: D
  END TYPE

  TYPE, PUBLIC, BIND(C) :: ZMQ_POLLITEM_T
    TYPE(C_PTR) :: SOCKET
    INTEGER(KIND = C_INT) :: FD
    INTEGER(KIND = C_SHORT) :: EVENTS
    INTEGER(KIND = C_SHORT) :: REVENTS
  END TYPE

  ! ERROR NUMBERS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ENOTSUP
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: EPROTONOSUPPORT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ENOBUFS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ENETDOWN
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: EADDRINUSE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: EADDRNOTAVAIL
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ECONNREFUSED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: EINPROGRESS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ENOTSOCK
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: EMSGSIZE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: EAFNOSUPPORT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ENETUNREACH
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ECONNABORTED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ECONNRESET
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ENOTCONN
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ETIMEDOUT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: EHOSTUNREACH
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ENETRESET
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: EFSM
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ENOCOMPATPROTO
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ETERM
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: EMTHREAD

  ! VERSION NUMBERS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_VERSION_MAJOR
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_VERSION_MINOR
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_VERSION_PATCH
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: FZMQ_VERSION_MAJOR
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: FZMQ_VERSION_MINOR
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: FZMQ_VERSION_PATCH

  ! CONTEXT OPTIONS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_IO_THREADS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_MAX_SOCKETS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_SOCKET_LIMIT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_THREAD_PRIORITY
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_THREAD_SCHED_POLICY

  ! CONTEXT DEFAULTS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_IO_THREADS_DFLT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_MAX_SOCKETS_DFLT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_THREAD_PRIORITY_DFLT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_THREAD_SCHED_POLICY_DFLT

  ! SOCKET TYPES
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_PAIR
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_PUB
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_SUB
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_REQ
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_REP
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_DEALER
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_ROUTER
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_PULL
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_PUSH
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_XPUB
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_XSUB
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_STREAM

  ! SOCKET OPTIONS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_AFFINITY
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_IDENTITY
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_SUBSCRIBE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_UNSUBSCRIBE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_RATE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_RECOVERY_IVL
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_SNDBUF
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_RCVBUF
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_RCVMORE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_FD
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENTS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_TYPE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_LINGER
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_RECONNECT_IVL
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_BACKLOG
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_RECONNECT_IVL_MAX
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_MAXMSGSIZE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_SNDHWM
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_RCVHWM
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_MULTICAST_HOPS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_RCVTIMEO
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_SNDTIMEO
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_LAST_ENDPOINT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_ROUTER_MANDATORY
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_TCP_KEEPALIVE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_TCP_KEEPALIVE_CNT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_TCP_KEEPALIVE_IDLE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_TCP_KEEPALIVE_INTVL
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_IMMEDIATE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_XPUB_VERBOSE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_ROUTER_RAW
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_IPV6
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_MECHANISM
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_PLAIN_SERVER
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_PLAIN_USERNAME
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_PLAIN_PASSWORD
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_CURVE_SERVER
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_CURVE_PUBLICKEY
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_CURVE_SECRETKEY
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_CURVE_SERVERKEY
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_PROBE_ROUTER
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_REQ_CORRELATE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_REQ_RELAXED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_CONFLATE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_ZAP_DOMAIN
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_ROUTER_HANDOVER
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_TOS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_CONNECT_RID
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_GSSAPI_SERVER
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_GSSAPI_PRINCIPAL
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_GSSAPI_SERVICE_PRINCIPAL
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_GSSAPI_PLAINTEXT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_HANDSHAKE_IVL
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_SOCKS_PROXY
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_XPUB_NODROP

  ! MESSAGE OPTIONS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_MORE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_SRCFD
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_SHARED

  ! SEND/RECV OPTIONS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_DONTWAIT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_SNDMORE

  ! SECURITY MECHANISMS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_NULL
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_PLAIN
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_CURVE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_GSSAPI

  ! EVENTS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENT_CONNECTED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENT_CONNECT_DELAYED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENT_CONNECT_RETRIED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENT_LISTENING
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENT_BIND_FAILED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENT_ACCEPTED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENT_ACCEPT_FAILED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENT_CLOSED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENT_CLOSE_FAILED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENT_DISCONNECTED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENT_MONITOR_STOPPED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_EVENT_ALL

  ! POLLING OPTIONS
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_POLLIN
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_POLLOUT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_POLLERR

  ! PROBE CAPABILITIES
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_HAS_CAPABILITIES

  ! DEPRECATED
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_XREQ
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_XREP
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_TCP_ACCEPT_FILTER
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_IPC_FILTER_PID
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_IPC_FILTER_UID
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_IPC_FILTER_GID
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_IPV4ONLY
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_DELAY_ATTACH_ON_CONNECT
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_NOBLOCK
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_FAIL_UNROUTABLE
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_ROUTER_BEHAVIOR
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_STREAMER
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_FORWARDER
  INTEGER(KIND = C_INT), PUBLIC, BIND(C) :: ZMQ_QUEUE

  ! PUBLIC KINDS
  PUBLIC :: C_CHAR
  PUBLIC :: C_FUNPTR
  PUBLIC :: C_INT
  PUBLIC :: C_INT16_T
  PUBLIC :: C_INT32_T
  PUBLIC :: C_INT64_T
  PUBLIC :: C_LONG
  PUBLIC :: C_NULL_CHAR
  PUBLIC :: C_PTR
  PUBLIC :: C_SHORT
  PUBLIC :: C_SIZE_T

  ! PUBLIC FUNCTIONS
  PUBLIC :: C_ASSOCIATED
  PUBLIC :: C_F_POINTER
  PUBLIC :: C_LOC
  PUBLIC :: C_SIZEOF
  PUBLIC :: ZMQ_BIND
  PUBLIC :: ZMQ_CLOSE
  PUBLIC :: ZMQ_CONNECT
  PUBLIC :: ZMQ_CTX_DESTROY
  PUBLIC :: ZMQ_CTX_GET
  PUBLIC :: ZMQ_CTX_NEW
  PUBLIC :: ZMQ_CTX_SET
  PUBLIC :: ZMQ_CTX_SHUTDOWN
  PUBLIC :: ZMQ_CTX_TERM
  PUBLIC :: ZMQ_CURVE_KEYPAIR
  PUBLIC :: ZMQ_DISCONNECT
  PUBLIC :: ZMQ_ERRNO
  PUBLIC :: ZMQ_GETSOCKOPT
  PUBLIC :: ZMQ_HAS
  PUBLIC :: ZMQ_INIT
  PUBLIC :: ZMQ_MSG_CLOSE
  PUBLIC :: ZMQ_MSG_COPY
  PUBLIC :: ZMQ_MSG_DATA
  PUBLIC :: ZMQ_MSG_GET
  PUBLIC :: ZMQ_MSG_INIT
  PUBLIC :: ZMQ_MSG_INIT_DATA
  PUBLIC :: ZMQ_MSG_INIT_SIZE
  PUBLIC :: ZMQ_MSG_MORE
  PUBLIC :: ZMQ_MSG_MOVE
  PUBLIC :: ZMQ_MSG_RECV
  PUBLIC :: ZMQ_MSG_SEND
  PUBLIC :: ZMQ_MSG_SET
  PUBLIC :: ZMQ_MSG_SIZE
  PUBLIC :: ZMQ_POLL
  PUBLIC :: ZMQ_PROXY
  PUBLIC :: ZMQ_PROXY_STEERABLE
  PUBLIC :: ZMQ_RECV
  PUBLIC :: ZMQ_RECVMSG
  PUBLIC :: ZMQ_SEND
  PUBLIC :: ZMQ_SEND_CONST
  PUBLIC :: ZMQ_SENDMSG
  PUBLIC :: ZMQ_SETSOCKOPT
  PUBLIC :: ZMQ_SOCKET
  PUBLIC :: ZMQ_SOCKET_MONITOR
  PUBLIC :: ZMQ_STRERROR
  PUBLIC :: ZMQ_TERM
  PUBLIC :: ZMQ_UNBIND
  PUBLIC :: ZMQ_VERSION
  PUBLIC :: ZMQ_Z85_DECODE
  PUBLIC :: ZMQ_Z85_ENCODE
  PUBLIC :: FZMQ_VERSION

  ! ALTERNATE VERSIONS
  PUBLIC :: FZMQ_BIND
  PUBLIC :: FZMQ_CONNECT
  PUBLIC :: FZMQ_DISCONNECT
  PUBLIC :: FZMQ_HAS
  PUBLIC :: FZMQ_SOCKET_MONITOR
  PUBLIC :: FZMQ_UNBIND

  INTERFACE

    FUNCTION ZMQ_CLOSE(SOCKET) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      INTEGER(KIND = C_INT) :: ZMQ_CLOSE
    END FUNCTION

    FUNCTION ZMQ_CTX_DESTROY(CONTEXT) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: CONTEXT
      INTEGER(KIND = C_INT) :: ZMQ_CTX_DESTROY
    END FUNCTION

    FUNCTION ZMQ_CTX_GET(CONTEXT, OPTION_NAME) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: CONTEXT
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: OPTION_NAME
      INTEGER(KIND = C_INT) :: ZMQ_CTX_GET
    END FUNCTION

    FUNCTION ZMQ_CTX_NEW() BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
      IMPLICIT NONE
      TYPE(C_PTR) :: ZMQ_CTX_NEW
    END FUNCTION

    FUNCTION ZMQ_CTX_SET(CONTEXT, OPTION_NAME, OPTION_VALUE) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: CONTEXT
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: OPTION_NAME
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: OPTION_VALUE
      INTEGER(KIND = C_INT) :: ZMQ_CTX_SET
    END FUNCTION

    FUNCTION ZMQ_CTX_SHUTDOWN(CONTEXT) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: CONTEXT
      INTEGER(KIND = C_INT) :: ZMQ_CTX_SHUTDOWN
    END FUNCTION

    FUNCTION ZMQ_CTX_TERM(CONTEXT) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: CONTEXT
      INTEGER(KIND = C_INT) :: ZMQ_CTX_TERM
    END FUNCTION

    FUNCTION ZMQ_CURVE_KEYPAIR(Z85_PUBLIC_KEY, Z85_SECRET_KEY) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_INT
      IMPLICIT NONE
      CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(OUT) :: Z85_PUBLIC_KEY
      CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(OUT) :: Z85_SECRET_KEY
      INTEGER(KIND = C_INT) :: ZMQ_CURVE_KEYPAIR
    END FUNCTION

    FUNCTION ZMQ_ERRNO() BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT
      IMPLICIT NONE
      INTEGER(KIND = C_INT) :: ZMQ_ERRNO
    END FUNCTION

    FUNCTION ZMQ_INIT(IO_THREADS) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_INT
      IMPLICIT NONE
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: IO_THREADS
      TYPE(C_PTR) :: ZMQ_INIT
    END FUNCTION

    FUNCTION ZMQ_MSG_CLOSE(MESSAGE) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      INTEGER(KIND = C_INT) :: ZMQ_MSG_CLOSE
    END FUNCTION

    FUNCTION ZMQ_MSG_COPY(DEST, SRC) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: DEST
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: SRC
      INTEGER(KIND = C_INT) :: ZMQ_MSG_COPY
    END FUNCTION

    FUNCTION ZMQ_MSG_DATA(MESSAGE) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      TYPE(C_PTR) :: ZMQ_MSG_DATA
    END FUNCTION

    FUNCTION ZMQ_MSG_GET(MESSAGE, PROPERTY) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: PROPERTY
      INTEGER(KIND = C_INT) :: ZMQ_MSG_GET
    END FUNCTION

    FUNCTION ZMQ_MSG_INIT(MESSAGE) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      INTEGER(KIND = C_INT) :: ZMQ_MSG_INIT
    END FUNCTION

    FUNCTION ZMQ_MSG_INIT_DATA(MESSAGE, DATA, SIZE, FFN, HINT) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_FUNPTR, C_INT, C_PTR, C_SIZE_T
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      TYPE(C_PTR), VALUE, INTENT(IN) :: DATA
      INTEGER(KIND = C_SIZE_T), VALUE, INTENT(IN) :: SIZE
      TYPE(C_FUNPTR), VALUE, INTENT(IN) :: FFN
      TYPE(C_PTR), VALUE, INTENT(IN) :: HINT
      INTEGER(KIND = C_INT) :: ZMQ_MSG_INIT_DATA
    END FUNCTION

    FUNCTION ZMQ_MSG_INIT_SIZE(MESSAGE, SIZE) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_SIZE_T
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      INTEGER(KIND = C_SIZE_T), VALUE, INTENT(IN) :: SIZE
      INTEGER(KIND = C_INT) :: ZMQ_MSG_INIT_SIZE
    END FUNCTION

    FUNCTION ZMQ_MSG_MOVE(DEST, SRC) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: DEST
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: SRC
      INTEGER(KIND = C_INT) :: ZMQ_MSG_MOVE
    END FUNCTION

    FUNCTION ZMQ_MSG_RECV(MESSAGE, SOCKET, FLAGS) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: FLAGS
      INTEGER(KIND = C_INT) :: ZMQ_MSG_RECV
    END FUNCTION

    FUNCTION ZMQ_MSG_SEND(MESSAGE, SOCKET, FLAGS) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: FLAGS
      INTEGER(KIND = C_INT) :: ZMQ_MSG_SEND
    END FUNCTION

    FUNCTION ZMQ_MSG_SET(MESSAGE, PROPERTY, VALUE) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: PROPERTY
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: VALUE
      INTEGER(KIND = C_INT) :: ZMQ_MSG_SET
    END FUNCTION

    FUNCTION ZMQ_MSG_SIZE(MESSAGE) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_SIZE_T
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      INTEGER(KIND = C_SIZE_T) :: ZMQ_MSG_SIZE
    END FUNCTION

    FUNCTION ZMQ_POLL(ITEMS, NITEMS, TIMEOUT) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_LONG
      IMPORT :: ZMQ_POLLITEM_T
      IMPLICIT NONE
      TYPE(ZMQ_POLLITEM_T), DIMENSION(*), INTENT(INOUT) :: ITEMS
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: NITEMS
      INTEGER(KIND = C_LONG), VALUE, INTENT(IN) :: TIMEOUT
      INTEGER(KIND = C_INT) :: ZMQ_POLL
    END FUNCTION

    FUNCTION ZMQ_PROXY(FRONTEND, BACKEND, CAPTURE) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: FRONTEND
      TYPE(C_PTR), VALUE, INTENT(IN) :: BACKEND
      TYPE(C_PTR), VALUE, INTENT(IN) :: CAPTURE
      INTEGER(KIND = C_INT) :: ZMQ_PROXY
    END FUNCTION

    FUNCTION ZMQ_PROXY_STEERABLE(FRONTEND, BACKEND, CAPTURE, CONTROL) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: FRONTEND
      TYPE(C_PTR), VALUE, INTENT(IN) :: BACKEND
      TYPE(C_PTR), VALUE, INTENT(IN) :: CAPTURE
      TYPE(C_PTR), VALUE, INTENT(IN) :: CONTROL
      INTEGER(KIND = C_INT) :: ZMQ_PROXY_STEERABLE
    END FUNCTION

    FUNCTION ZMQ_RECVMSG(SOCKET, MESSAGE, FLAGS) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: FLAGS
      INTEGER(KIND = C_INT) :: ZMQ_RECVMSG
    END FUNCTION

    FUNCTION ZMQ_SENDMSG(SOCKET, MESSAGE, FLAGS) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: FLAGS
      INTEGER(KIND = C_INT) :: ZMQ_SENDMSG
    END FUNCTION

    FUNCTION ZMQ_SOCKET(CONTEXT, TYPE_) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: CONTEXT
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: TYPE_
      TYPE(C_PTR) :: ZMQ_SOCKET
    END FUNCTION

    FUNCTION ZMQ_TERM(CONTEXT) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: CONTEXT
      INTEGER(KIND = C_INT) :: ZMQ_TERM
    END FUNCTION

    SUBROUTINE ZMQ_VERSION(MAJOR, MINOR, PATCH) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT
      IMPLICIT NONE
      INTEGER(KIND = C_INT), INTENT(OUT) :: MAJOR
      INTEGER(KIND = C_INT), INTENT(OUT) :: MINOR
      INTEGER(KIND = C_INT), INTENT(OUT) :: PATCH
    END SUBROUTINE

    FUNCTION FZMQ_BIND(SOCKET, ENDPOINT) BIND(C, NAME = 'zmq_bind')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(IN) :: ENDPOINT
      INTEGER(KIND = C_INT) :: FZMQ_BIND
    END FUNCTION

    FUNCTION FZMQ_CONNECT(SOCKET, ENDPOINT) BIND(C, NAME = 'zmq_connect')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(IN) :: ENDPOINT
      INTEGER(KIND = C_INT) :: FZMQ_CONNECT
    END FUNCTION

    FUNCTION FZMQ_DISCONNECT(SOCKET, ENDPOINT) BIND(C, NAME = 'zmq_disconnect')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(IN) :: ENDPOINT
      INTEGER(KIND = C_INT) :: FZMQ_DISCONNECT
    END FUNCTION

    FUNCTION FZMQ_GETSOCKOPT(SOCKET, OPTION_NAME, OPTION_VALUE, OPTION_LEN) &
        BIND(C, NAME = 'zmq_getsockopt')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR, C_SIZE_T
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: OPTION_NAME
      TYPE(C_PTR), VALUE, INTENT(IN) :: OPTION_VALUE
      INTEGER(KIND = C_SIZE_T), INTENT(INOUT) :: OPTION_LEN
      INTEGER(KIND = C_INT) :: FZMQ_GETSOCKOPT
    END FUNCTION

    FUNCTION FZMQ_HAS(CAPABILITY) BIND(C, NAME = 'zmq_has')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_INT
      IMPLICIT NONE
      CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(IN) :: CAPABILITY
      INTEGER(KIND = C_INT) :: FZMQ_HAS
    END FUNCTION

    FUNCTION FZMQ_MSG_MORE(MESSAGE) BIND(C, NAME = 'zmq_msg_more')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT
      IMPORT :: ZMQ_MSG_T
      IMPLICIT NONE
      TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
      INTEGER(KIND = C_INT) :: FZMQ_MSG_MORE
    END FUNCTION

    FUNCTION FZMQ_RECV(SOCKET, BUF, LEN, FLAGS) BIND(C, NAME = 'zmq_recv')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR, C_SIZE_T
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      TYPE(C_PTR), VALUE, INTENT(IN) :: BUF
      INTEGER(KIND = C_SIZE_T), VALUE, INTENT(IN) :: LEN
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: FLAGS
      INTEGER(KIND = C_INT) :: FZMQ_RECV
    END FUNCTION

    FUNCTION FZMQ_SEND(SOCKET, BUF, LEN, FLAGS) BIND(C, NAME = 'zmq_send')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR, C_SIZE_T
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      TYPE(C_PTR), VALUE, INTENT(IN) :: BUF
      INTEGER(KIND = C_SIZE_T), VALUE, INTENT(IN) :: LEN
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: FLAGS
      INTEGER(KIND = C_INT) :: FZMQ_SEND
    END FUNCTION

    FUNCTION FZMQ_SEND_CONST(SOCKET, BUF, LEN, FLAGS) &
        BIND(C, NAME = 'zmq_send_const')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR, C_SIZE_T
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      TYPE(C_PTR), VALUE, INTENT(IN) :: BUF
      INTEGER(KIND = C_SIZE_T), VALUE, INTENT(IN) :: LEN
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: FLAGS
      INTEGER(KIND = C_INT) :: FZMQ_SEND_CONST
    END FUNCTION

    FUNCTION FZMQ_SETSOCKOPT(SOCKET, OPTION_NAME, OPTION_VALUE, OPTION_LEN) &
        BIND(C, NAME = 'zmq_setsockopt')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR, C_SIZE_T
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: OPTION_NAME
      TYPE(C_PTR), VALUE, INTENT(IN) :: OPTION_VALUE
      INTEGER(KIND = C_SIZE_T), VALUE, INTENT(IN) :: OPTION_LEN
      INTEGER(KIND = C_INT) :: FZMQ_SETSOCKOPT
    END FUNCTION

    FUNCTION FZMQ_SOCKET_MONITOR(SOCKET, ENDPOINT, EVENTS) &
        BIND(C, NAME = 'zmq_socket_monitor')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(IN) :: ENDPOINT
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: EVENTS
      INTEGER(KIND = C_INT) :: FZMQ_SOCKET_MONITOR
    END FUNCTION

    FUNCTION FZMQ_STRERROR(ERRNUM) BIND(C, NAME = 'zmq_strerror')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_INT, C_PTR
      IMPLICIT NONE
      INTEGER(KIND = C_INT), VALUE, INTENT(IN) :: ERRNUM
      TYPE(C_PTR) :: FZMQ_STRERROR
    END FUNCTION

    FUNCTION FZMQ_UNBIND(SOCKET, ENDPOINT) BIND(C, NAME = 'zmq_unbind')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_INT, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: SOCKET
      CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(IN) :: ENDPOINT
      INTEGER(KIND = C_INT) :: FZMQ_UNBIND
    END FUNCTION

    FUNCTION FZMQ_Z85_DECODE(DEST, STRING) BIND(C, NAME = 'zmq_z85_decode')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: DEST
      CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(IN) :: STRING
      TYPE(C_PTR) :: FZMQ_Z85_DECODE
    END FUNCTION

    FUNCTION FZMQ_Z85_ENCODE(DEST, DATA, SIZE) BIND(C, NAME = 'zmq_z85_encode')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_PTR, C_SIZE_T
      IMPLICIT NONE
      CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(OUT) :: DEST
      TYPE(C_PTR), VALUE, INTENT(IN) :: DATA
      INTEGER(KIND = C_SIZE_T), VALUE, INTENT(IN) :: SIZE
      TYPE(C_PTR) :: FZMQ_Z85_ENCODE
    END FUNCTION

  END INTERFACE

  INTERFACE ZMQ_GETSOCKOPT
    MODULE PROCEDURE ZMQ_GETSOCKOPT_API, &
                     ZMQ_GETSOCKOPT_INT, &
                     ZMQ_GETSOCKOPT_INT64, &
                     ZMQ_GETSOCKOPT_STR
  END INTERFACE

  INTERFACE ZMQ_RECV
    MODULE PROCEDURE ZMQ_RECV_API, &
                     ZMQ_RECV_STR
  END INTERFACE

  INTERFACE ZMQ_SEND
    MODULE PROCEDURE ZMQ_SEND_API, &
                     ZMQ_SEND_STR
  END INTERFACE

  INTERFACE ZMQ_SEND_CONST
    MODULE PROCEDURE ZMQ_SEND_CONST_API, &
                     ZMQ_SEND_CONST_INT, &
                     ZMQ_SEND_CONST_INT64, &
                     ZMQ_SEND_CONST_STR
  END INTERFACE

  INTERFACE ZMQ_SETSOCKOPT
    MODULE PROCEDURE ZMQ_SETSOCKOPT_API, &
                     ZMQ_SETSOCKOPT_INT, &
                     ZMQ_SETSOCKOPT_INT64, &
                     ZMQ_SETSOCKOPT_STR
  END INTERFACE

CONTAINS

  SUBROUTINE FZMQ_VERSION(MAJOR, MINOR, PATCH)
    INTEGER(KIND = C_INT), INTENT(OUT) :: MAJOR
    INTEGER(KIND = C_INT), INTENT(OUT) :: MINOR
    INTEGER(KIND = C_INT), INTENT(OUT) :: PATCH

    MAJOR = FZMQ_VERSION_MAJOR
    MINOR = FZMQ_VERSION_MINOR
    PATCH = FZMQ_VERSION_PATCH
  END SUBROUTINE

  FUNCTION ZMQ_BIND(SOCKET, ENDPOINT) RESULT(CODE)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    CHARACTER(LEN = *), INTENT(IN) :: ENDPOINT
    INTEGER(KIND = C_INT) :: CODE

    CODE = FZMQ_BIND(SOCKET, TRIM(ENDPOINT) // C_NULL_CHAR)
  END FUNCTION

  FUNCTION ZMQ_CONNECT(SOCKET, ENDPOINT) RESULT(CODE)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    CHARACTER(LEN = *), INTENT(IN) :: ENDPOINT
    INTEGER(KIND = C_INT) :: CODE

    CODE = FZMQ_CONNECT(SOCKET, TRIM(ENDPOINT) // C_NULL_CHAR)
  END FUNCTION

  FUNCTION ZMQ_DISCONNECT(SOCKET, ENDPOINT) RESULT(CODE)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    CHARACTER(LEN = *), INTENT(IN) :: ENDPOINT
    INTEGER(KIND = C_INT) :: CODE

    CODE = FZMQ_DISCONNECT(SOCKET, TRIM(ENDPOINT) // C_NULL_CHAR)
  END FUNCTION

  FUNCTION ZMQ_GETSOCKOPT_API(SOCKET, OPTION_NAME, OPTION_VALUE, OPTION_LEN) &
      RESULT(CODE)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(IN) :: OPTION_NAME
    TYPE(C_PTR), INTENT(IN) :: OPTION_VALUE
    INTEGER(KIND = C_SIZE_T), INTENT(INOUT) :: OPTION_LEN
    INTEGER(KIND = C_INT) :: CODE

    CODE = FZMQ_GETSOCKOPT(SOCKET, OPTION_NAME, OPTION_VALUE, OPTION_LEN)
  END FUNCTION

  FUNCTION ZMQ_GETSOCKOPT_INT(SOCKET, OPTION_NAME, OPTION_VALUE) RESULT(CODE)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(IN) :: OPTION_NAME
    INTEGER(KIND = C_INT), INTENT(OUT) :: OPTION_VALUE
    INTEGER(KIND = C_INT) :: CODE

    TYPE(C_PTR) :: OPTION_PTR
    INTEGER(KIND = C_SIZE_T) :: OPTION_LEN
    INTEGER(KIND = C_INT), TARGET :: OPTION_VAL

    OPTION_LEN = C_SIZEOF(OPTION_VAL)
    OPTION_PTR = C_LOC(OPTION_VAL)
    CODE = FZMQ_GETSOCKOPT(SOCKET, OPTION_NAME, OPTION_PTR, OPTION_LEN)
    OPTION_VALUE = OPTION_VAL
  END FUNCTION

  FUNCTION ZMQ_GETSOCKOPT_INT64(SOCKET, OPTION_NAME, OPTION_VALUE) RESULT(CODE)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(IN) :: OPTION_NAME
    INTEGER(KIND = C_INT64_T), INTENT(OUT) :: OPTION_VALUE
    INTEGER(KIND = C_INT) :: CODE

    TYPE(C_PTR) :: OPTION_PTR
    INTEGER(KIND = C_SIZE_T) :: OPTION_LEN
    INTEGER(KIND = C_INT64_T), TARGET :: OPTION_VAL

    OPTION_LEN = C_SIZEOF(OPTION_VAL)
    OPTION_PTR = C_LOC(OPTION_VAL)
    CODE = FZMQ_GETSOCKOPT(SOCKET, OPTION_NAME, OPTION_PTR, OPTION_LEN)
    OPTION_VALUE = OPTION_VAL
  END FUNCTION

  FUNCTION ZMQ_GETSOCKOPT_STR(SOCKET, OPTION_NAME, OPTION_VALUE) RESULT(CODE)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(IN) :: OPTION_NAME
    CHARACTER(LEN = *), INTENT(INOUT) :: OPTION_VALUE
    INTEGER(KIND = C_INT) :: CODE

    INTEGER :: EOS
    TYPE(C_PTR) :: OPTION_PTR
    INTEGER(KIND = C_SIZE_T) :: OPTION_LEN
    CHARACTER(LEN = :), ALLOCATABLE, TARGET :: OPTION_VAL

    OPTION_LEN = LEN(OPTION_VALUE) + 1
    ALLOCATE(CHARACTER(LEN = OPTION_LEN) :: OPTION_VAL)
    OPTION_PTR = C_LOC(OPTION_VAL)
    CODE = FZMQ_GETSOCKOPT(SOCKET, OPTION_NAME, OPTION_PTR, OPTION_LEN)
    EOS = 1
    DO
      IF (OPTION_VAL(EOS:EOS) == C_NULL_CHAR .OR. EOS >= OPTION_LEN) THEN
        EXIT
      END IF
      EOS = EOS + 1
    END DO
    EOS = EOS - 1
    OPTION_VALUE = OPTION_VAL(1:EOS)
    DEALLOCATE(OPTION_VAL)
  END FUNCTION

  FUNCTION ZMQ_HAS(CAPABILITY) RESULT(CODE)
    CHARACTER(LEN = *), INTENT(IN) :: CAPABILITY
    INTEGER(KIND = C_INT) :: CODE

    CODE = FZMQ_HAS(TRIM(CAPABILITY) // C_NULL_CHAR)
  END FUNCTION

  FUNCTION ZMQ_MSG_MORE(MESSAGE) RESULT(MORE)
    TYPE(ZMQ_MSG_T), INTENT(INOUT) :: MESSAGE
    LOGICAL :: MORE

    MORE = FZMQ_MSG_MORE(MESSAGE) == 1
  END FUNCTION

  FUNCTION ZMQ_RECV_API(SOCKET, BUF, LEN, FLAGS) RESULT(NBYTES)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    TYPE(C_PTR), INTENT(IN) :: BUF
    INTEGER(KIND = C_SIZE_T), INTENT(IN) :: LEN
    INTEGER(KIND = C_INT), INTENT(IN) :: FLAGS
    INTEGER(KIND = C_INT) :: NBYTES

    NBYTES = FZMQ_RECV(SOCKET, BUF, LEN, FLAGS)
  END FUNCTION

  FUNCTION ZMQ_RECV_STR(SOCKET, BUF, FLAGS) RESULT(NBYTES)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    CHARACTER(LEN = *), INTENT(INOUT) :: BUF
    INTEGER(KIND = C_INT) :: FLAGS
    INTEGER(KIND = C_INT) :: NBYTES

    INTEGER :: EOS
    TYPE(C_PTR) :: BUFFER_PTR
    INTEGER(KIND = C_SIZE_T) :: BUFFER_LEN
    CHARACTER(LEN = :), ALLOCATABLE, TARGET :: BUFFER

    BUFFER_LEN = LEN(BUF) + 1
    ALLOCATE(CHARACTER(LEN = BUFFER_LEN) :: BUFFER)
    BUFFER_PTR = C_LOC(BUFFER)
    NBYTES = FZMQ_RECV(SOCKET, BUFFER_PTR, BUFFER_LEN, FLAGS)
    EOS = 1
    DO
      IF (BUFFER(EOS:EOS) == C_NULL_CHAR .OR. EOS >= BUFFER_LEN) THEN
        EXIT
      END IF
      EOS = EOS + 1
    END DO
    EOS = EOS - 1
    BUF = BUFFER(1:EOS)
    DEALLOCATE(BUFFER)
  END FUNCTION

  FUNCTION ZMQ_SEND_API(SOCKET, BUF, LEN, FLAGS) RESULT(NBYTES)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    TYPE(C_PTR), INTENT(IN) :: BUF
    INTEGER(KIND = C_SIZE_T), INTENT(IN) :: LEN
    INTEGER(KIND = C_INT), INTENT(IN) :: FLAGS
    INTEGER(KIND = C_INT) :: NBYTES

    NBYTES = FZMQ_SEND(SOCKET, BUF, LEN, FLAGS)
  END FUNCTION

  FUNCTION ZMQ_SEND_STR(SOCKET, STR, FLAGS) RESULT(NBYTES)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    CHARACTER(LEN = *), INTENT(IN) :: STR
    INTEGER(KIND = C_INT), INTENT(IN) :: FLAGS
    INTEGER(KIND = C_INT) :: NBYTES

    TYPE(C_PTR) :: BUFFER_PTR
    INTEGER(KIND = C_SIZE_T) :: SIZE
    INTEGER(KIND = C_INT) :: BUFFER_LEN
    CHARACTER(LEN = :), ALLOCATABLE, TARGET :: BUFFER

    BUFFER_LEN = LEN(TRIM(STR)) + 1
    SIZE = INT(BUFFER_LEN, KIND = C_SIZE_T)
    ALLOCATE(CHARACTER(LEN = BUFFER_LEN) :: BUFFER)
    BUFFER = STR(1:BUFFER_LEN - 1) // C_NULL_CHAR
    BUFFER_PTR = C_LOC(BUFFER)

    NBYTES = FZMQ_SEND(SOCKET, BUFFER_PTR, SIZE, FLAGS)

    DEALLOCATE(BUFFER)
  END FUNCTION

  FUNCTION ZMQ_SEND_CONST_API(SOCKET, BUF, LEN, FLAGS) RESULT(NBYTES)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    TYPE(C_PTR), INTENT(IN) :: BUF
    INTEGER(KIND = C_SIZE_T), INTENT(IN) :: LEN
    INTEGER(KIND = C_INT), INTENT(IN) :: FLAGS
    INTEGER(KIND = C_INT) :: NBYTES

    NBYTES = FZMQ_SEND_CONST(SOCKET, BUF, LEN, FLAGS)
  END FUNCTION

  FUNCTION ZMQ_SEND_CONST_INT(SOCKET, VALUE, FLAGS) RESULT(NBYTES)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(IN), TARGET :: VALUE
    INTEGER(KIND = C_INT), INTENT(IN) :: FLAGS
    INTEGER(KIND = C_INT) :: NBYTES

    NBYTES = FZMQ_SEND_CONST(SOCKET, C_LOC(VALUE), C_SIZEOF(VALUE), FLAGS)
  END FUNCTION

  FUNCTION ZMQ_SEND_CONST_INT64(SOCKET, VALUE, FLAGS) RESULT(NBYTES)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    INTEGER(KIND = C_INT64_T), INTENT(IN), TARGET :: VALUE
    INTEGER(KIND = C_INT), INTENT(IN) :: FLAGS
    INTEGER(KIND = C_INT) :: NBYTES

    NBYTES = FZMQ_SEND_CONST(SOCKET, C_LOC(VALUE), C_SIZEOF(VALUE), FLAGS)
  END FUNCTION

  FUNCTION ZMQ_SEND_CONST_STR(SOCKET, STR, FLAGS) RESULT(NBYTES)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    CHARACTER(LEN = *), INTENT(IN), TARGET :: STR
    INTEGER(KIND = C_INT), INTENT(IN) :: FLAGS
    INTEGER(KIND = C_INT) :: NBYTES

    INTEGER(KIND = C_SIZE_T) :: SIZE

    SIZE = LEN(STR, KIND = C_SIZE_T)
    NBYTES = FZMQ_SEND_CONST(SOCKET, C_LOC(STR), SIZE, FLAGS)
  END FUNCTION

  FUNCTION ZMQ_SETSOCKOPT_API(SOCKET, OPTION_NAME, OPTION_VALUE, OPTION_LEN) &
      RESULT(CODE)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(IN) :: OPTION_NAME
    TYPE(C_PTR), INTENT(IN) :: OPTION_VALUE
    INTEGER(KIND = C_SIZE_T), INTENT(IN) :: OPTION_LEN
    INTEGER(KIND = C_INT) :: CODE

    CODE = FZMQ_SETSOCKOPT(SOCKET, OPTION_NAME, OPTION_VALUE, OPTION_LEN)
  END FUNCTION

  FUNCTION ZMQ_SETSOCKOPT_INT(SOCKET, OPTION_NAME, OPTION_VALUE) RESULT(CODE)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(IN) :: OPTION_NAME
    INTEGER(KIND = C_INT), INTENT(IN) :: OPTION_VALUE
    INTEGER(KIND = C_INT) :: CODE

    TYPE(C_PTR) :: OPTION_PTR
    INTEGER(KIND = C_SIZE_T) :: OPTION_LEN
    INTEGER(KIND = C_INT), TARGET :: OPTION_VAL

    OPTION_VAL = OPTION_VALUE
    OPTION_LEN = C_SIZEOF(OPTION_VAL)
    OPTION_PTR = C_LOC(OPTION_VAL)
    CODE = FZMQ_SETSOCKOPT(SOCKET, OPTION_NAME, OPTION_PTR, OPTION_LEN)
  END FUNCTION

  FUNCTION ZMQ_SETSOCKOPT_INT64(SOCKET, OPTION_NAME, OPTION_VALUE) RESULT(CODE)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(IN) :: OPTION_NAME
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: OPTION_VALUE
    INTEGER(KIND = C_INT) :: CODE

    TYPE(C_PTR) :: OPTION_PTR
    INTEGER(KIND = C_SIZE_T) :: OPTION_LEN
    INTEGER(KIND = C_INT64_T), TARGET :: OPTION_VAL

    OPTION_VAL = OPTION_VALUE
    OPTION_LEN = C_SIZEOF(OPTION_VAL)
    OPTION_PTR = C_LOC(OPTION_VAL)
    CODE = FZMQ_SETSOCKOPT(SOCKET, OPTION_NAME, OPTION_PTR, OPTION_LEN)
  END FUNCTION

  FUNCTION ZMQ_SETSOCKOPT_STR(SOCKET, OPTION_NAME, OPTION_VALUE) RESULT(CODE)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(IN) :: OPTION_NAME
    CHARACTER(LEN = *), INTENT(IN) :: OPTION_VALUE
    INTEGER(KIND = C_INT) :: CODE

    TYPE(C_PTR) :: OPTION_PTR
    INTEGER(KIND = C_SIZE_T) :: OPTION_LEN
    CHARACTER(LEN = :), ALLOCATABLE, TARGET :: OPTION_VAL

    OPTION_LEN = LEN(TRIM(OPTION_VALUE)) + 1
    ALLOCATE(CHARACTER(LEN = OPTION_LEN) :: OPTION_VAL)
    OPTION_VAL = OPTION_VALUE(1:OPTION_LEN - 1) // C_NULL_CHAR
    OPTION_PTR = C_LOC(OPTION_VAL)
    CODE = FZMQ_SETSOCKOPT(SOCKET, OPTION_NAME, OPTION_PTR, OPTION_LEN)
    DEALLOCATE(OPTION_VAL)
  END FUNCTION

  FUNCTION ZMQ_SOCKET_MONITOR(SOCKET, ENDPOINT, EVENTS) RESULT(CODE)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_CHAR, C_INT, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    CHARACTER(LEN = *), INTENT(IN) :: ENDPOINT
    INTEGER(KIND = C_INT), INTENT(IN) :: EVENTS
    INTEGER(KIND = C_INT) :: CODE

    CODE = FZMQ_SOCKET_MONITOR(SOCKET, TRIM(ENDPOINT) // C_NULL_CHAR, EVENTS)
  END FUNCTION

  FUNCTION ZMQ_STRERROR(ERRNUM) RESULT(MESSAGE)
    INTEGER(KIND = C_INT), INTENT(IN) :: ERRNUM
    CHARACTER(LEN = :), ALLOCATABLE :: MESSAGE

    INTEGER :: SIZE
    TYPE(C_PTR) :: MSG_C_PTR
    CHARACTER(KIND = C_CHAR, LEN = :), POINTER :: MSG_PTR

    SIZE = 1
    MSG_C_PTR = FZMQ_STRERROR(ERRNUM)
    CALL C_F_POINTER(MSG_C_PTR, MSG_PTR)
    DO
      IF (MSG_PTR(SIZE:SIZE) == C_NULL_CHAR .OR. SIZE >= 64) THEN
        EXIT
      END IF
      SIZE = SIZE + 1
    END DO
    SIZE = SIZE - 1
    ALLOCATE(CHARACTER(KIND = C_CHAR, LEN = SIZE) :: MESSAGE)
    MESSAGE = TRANSFER(MSG_PTR(1:SIZE), MESSAGE)
  END FUNCTION

  FUNCTION ZMQ_UNBIND(SOCKET, ENDPOINT) RESULT(CODE)
    TYPE(C_PTR), INTENT(IN) :: SOCKET
    CHARACTER(LEN = *), INTENT(IN) :: ENDPOINT
    INTEGER(KIND = C_INT) :: CODE

    CODE = FZMQ_UNBIND(SOCKET, TRIM(ENDPOINT) // C_NULL_CHAR)
  END FUNCTION

  FUNCTION ZMQ_Z85_DECODE(DEST, STRING) RESULT(SUCCESS)
    TYPE(C_PTR), INTENT(IN) :: DEST
    CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(IN) :: STRING
    LOGICAL :: SUCCESS

    TYPE(C_PTR) :: PTR

    PTR = FZMQ_Z85_DECODE(DEST, STRING)
    SUCCESS = C_ASSOCIATED(PTR)
  END FUNCTION

  FUNCTION ZMQ_Z85_ENCODE(DEST, DATA, SIZE) RESULT(SUCCESS)
    CHARACTER(KIND = C_CHAR), DIMENSION(*), INTENT(OUT) :: DEST
    TYPE(C_PTR), INTENT(IN) :: DATA
    INTEGER(KIND = C_SIZE_T), INTENT(IN) :: SIZE
    LOGICAL :: SUCCESS

    TYPE(C_PTR) :: PTR

    PTR = FZMQ_Z85_ENCODE(DEST, DATA, SIZE)
    SUCCESS = C_ASSOCIATED(PTR)
  END FUNCTION

END MODULE
