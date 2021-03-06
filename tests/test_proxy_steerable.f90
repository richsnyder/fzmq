PROGRAM TEST_PROXY_STEERABLE

USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_NULL_PTR
USE ZMQ
USE ASSERT
IMPLICIT NONE

TYPE(C_PTR) :: CONTEXT
TYPE(C_PTR) :: FSOCKET
TYPE(C_PTR) :: BSOCKET
TYPE(C_PTR) :: CSOCKET
INTEGER(KIND = C_INT) :: CODE
INTEGER(KIND = C_INT) :: VALUE
TYPE(ZMQ_POLLITEM_T), DIMENSION(0) :: ITEMS
CHARACTER(KIND = C_CHAR, LEN = 9), TARGET :: SIGNAL
LOGICAL :: SIGNALED

CALL OMP_SET_DYNAMIC(0)
CALL OMP_SET_NUM_THREADS(4)

CONTEXT = ZMQ_CTX_NEW()

!$OMP PARALLEL SHARED(CONTEXT), PRIVATE(FSOCKET,BSOCKET,CSOCKET,CODE,VALUE,ITEMS,SIGNAL,SIGNALED)
!$OMP SECTIONS

!$OMP SECTION

  FSOCKET = ZMQ_SOCKET(CONTEXT, ZMQ_PAIR)
  BSOCKET = ZMQ_SOCKET(CONTEXT, ZMQ_PAIR)
  CSOCKET = ZMQ_SOCKET(CONTEXT, ZMQ_PAIR)
  CODE = ZMQ_BIND(FSOCKET, 'inproc://fzmq-fe')
  CALL ASSERT_EQUALS(0, CODE)
  CODE = ZMQ_BIND(BSOCKET, 'inproc://fzmq-be')
  CALL ASSERT_EQUALS(0, CODE)
  CODE = ZMQ_BIND(CSOCKET, 'inproc://fzmq-ct')
  CALL ASSERT_EQUALS(0, CODE)

  CODE = ZMQ_PROXY_STEERABLE(FSOCKET, BSOCKET, C_NULL_PTR, CSOCKET)

  CODE = ZMQ_CLOSE(FSOCKET)
  CALL ASSERT_EQUALS(0, CODE)
  CODE = ZMQ_CLOSE(BSOCKET)
  CALL ASSERT_EQUALS(0, CODE)
  CODE = ZMQ_CLOSE(CSOCKET)
  CALL ASSERT_EQUALS(0, CODE)

!$OMP SECTION

  FSOCKET = ZMQ_SOCKET(CONTEXT, ZMQ_PAIR)
  CODE = ZMQ_CONNECT(FSOCKET, 'inproc://fzmq-fe')
  CALL ASSERT_EQUALS(0, CODE)

  CALL SEND(FSOCKET, 123)

  CODE = ZMQ_CLOSE(FSOCKET)
  CALL ASSERT_EQUALS(0, CODE)

!$OMP SECTION

  BSOCKET = ZMQ_SOCKET(CONTEXT, ZMQ_PAIR)
  CODE = ZMQ_CONNECT(BSOCKET, 'inproc://fzmq-be')
  CALL ASSERT_EQUALS(0, CODE)

  CALL RECV(BSOCKET, VALUE)
  CALL ASSERT_EQUALS(123, VALUE)

  CODE = ZMQ_CLOSE(BSOCKET)
  CALL ASSERT_EQUALS(0, CODE)

!$OMP SECTION

  CSOCKET = ZMQ_SOCKET(CONTEXT, ZMQ_PAIR)
  CODE = ZMQ_CONNECT(CSOCKET, 'inproc://fzmq-ct')
  CALL ASSERT_EQUALS(0, CODE)

  CODE = ZMQ_POLL(ITEMS, 0, 100_8)

  SIGNAL = 'TERMINATE'
  CODE = ZMQ_SEND(CSOCKET, C_LOC(SIGNAL), INT(LEN(SIGNAL), KIND = C_SIZE_T), 0)

  CODE = ZMQ_CLOSE(CSOCKET)
  CALL ASSERT_EQUALS(0, CODE)

!$OMP END SECTIONS NOWAIT
!$OMP END PARALLEL

CODE = ZMQ_CTX_TERM(CONTEXT)
CALL ASSERT_EQUALS(0, CODE)

CONTAINS

  SUBROUTINE SEND(SOCKET, VALUE)
    TYPE(C_PTR), INTENT(INOUT) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(IN) :: VALUE

    TYPE(C_PTR) :: DATA
    TYPE(ZMQ_MSG_T) :: MESSAGE
    INTEGER(KIND = C_INT) :: CODE
    INTEGER(KIND = C_INT) :: NBYTES
    INTEGER(KIND = C_SIZE_T) :: SIZE
    CHARACTER(KIND = C_CHAR, LEN = :), POINTER :: BUFFER
    CHARACTER(KIND = C_CHAR, LEN = :), POINTER :: RANGE

    SIZE = C_SIZEOF(VALUE)
    CODE = ZMQ_MSG_INIT_SIZE(MESSAGE, SIZE)
    CALL ASSERT_EQUALS(0, CODE)

    DATA = ZMQ_MSG_DATA(MESSAGE)
    CALL C_F_POINTER(DATA, BUFFER)
    RANGE => BUFFER(1:SIZE)
    RANGE = TRANSFER(VALUE, RANGE)

    NBYTES = ZMQ_MSG_SEND(MESSAGE, SOCKET, 0)
    CALL ASSERT_EQUALS(INT(SIZE, KIND = C_INT), NBYTES)
  END SUBROUTINE

  SUBROUTINE RECV(SOCKET, VALUE)
    TYPE(C_PTR), INTENT(INOUT) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(OUT) :: VALUE

    TYPE(C_PTR) :: DATA
    TYPE(ZMQ_MSG_T) :: MESSAGE
    INTEGER(KIND = C_INT) :: CODE
    INTEGER(KIND = C_INT) :: NBYTES
    INTEGER(KIND = C_SIZE_T) :: SIZE
    CHARACTER(KIND = C_CHAR, LEN = :), POINTER :: BUFFER
    CHARACTER(KIND = C_CHAR, LEN = :), POINTER :: RANGE

    SIZE = C_SIZEOF(VALUE)
    CODE = ZMQ_MSG_INIT(MESSAGE)
    CALL ASSERT_EQUALS(0, CODE)

    NBYTES = ZMQ_MSG_RECV(MESSAGE, SOCKET, 0)
    CALL ASSERT_EQUALS(INT(SIZE, KIND = C_INT), NBYTES)

    DATA = ZMQ_MSG_DATA(MESSAGE)
    CALL C_F_POINTER(DATA, BUFFER)
    RANGE => BUFFER(1:SIZE)
    VALUE = TRANSFER(RANGE, VALUE)

    CODE = ZMQ_MSG_CLOSE(MESSAGE)
    CALL ASSERT_EQUALS(0, CODE)
  END SUBROUTINE

END PROGRAM
