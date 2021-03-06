PROGRAM TEST_MSG_GET

USE ZMQ
USE ASSERT
IMPLICIT NONE

TYPE(C_PTR) :: CONTEXT
TYPE(C_PTR) :: SOCKET
INTEGER(KIND = C_INT) :: CODE
INTEGER(KIND = C_INT) :: VALUE
LOGICAL :: MORE

CALL OMP_SET_DYNAMIC(0)
CALL OMP_SET_NUM_THREADS(2)

CONTEXT = ZMQ_CTX_NEW()

!$OMP PARALLEL SHARED(CONTEXT), PRIVATE(SOCKET,CODE,VALUE,MORE)
!$OMP SECTIONS

!$OMP SECTION

  SOCKET = ZMQ_SOCKET(CONTEXT, ZMQ_PAIR)
  CODE = ZMQ_CONNECT(SOCKET, 'inproc://fzmq')
  CALL ASSERT_EQUALS(0, CODE)

  CALL SEND(SOCKET, 123, ZMQ_SNDMORE)
  CALL SEND(SOCKET, 456, 0)

  CODE = ZMQ_CLOSE(SOCKET)
  CALL ASSERT_EQUALS(0, CODE)

!$OMP SECTION

  SOCKET = ZMQ_SOCKET(CONTEXT, ZMQ_PAIR)
  CODE = ZMQ_BIND(SOCKET, 'inproc://fzmq')
  CALL ASSERT_EQUALS(0, CODE)

  MORE = RECV(SOCKET, VALUE)
  CALL ASSERT_EQUALS(123, VALUE)
  CALL ASSERT_TRUE(MORE)
  MORE = RECV(SOCKET, VALUE)
  CALL ASSERT_EQUALS(456, VALUE)
  CALL ASSERT_FALSE(MORE)

  CODE = ZMQ_CLOSE(SOCKET)
  CALL ASSERT_EQUALS(0, CODE)

!$OMP END SECTIONS
!$OMP END PARALLEL

CODE = ZMQ_CTX_TERM(CONTEXT)
CALL ASSERT_EQUALS(0, CODE)

CONTAINS

  SUBROUTINE SEND(SOCKET, VALUE, FLAGS)
    TYPE(C_PTR), INTENT(INOUT) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(IN) :: VALUE
    INTEGER(KIND = C_INT), INTENT(IN) :: FLAGS

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

    NBYTES = ZMQ_MSG_SEND(MESSAGE, SOCKET, FLAGS)
    CALL ASSERT_EQUALS(INT(SIZE, KIND = C_INT), NBYTES)
  END SUBROUTINE

  FUNCTION RECV(SOCKET, VALUE) RESULT(MORE)
    TYPE(C_PTR), INTENT(INOUT) :: SOCKET
    INTEGER(KIND = C_INT), INTENT(OUT) :: VALUE
    LOGICAL :: MORE

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

    CODE = ZMQ_MSG_GET(MESSAGE, ZMQ_MORE)
    MORE = CODE == 1

    CODE = ZMQ_MSG_CLOSE(MESSAGE)
    CALL ASSERT_EQUALS(0, CODE)
  END FUNCTION

END PROGRAM
