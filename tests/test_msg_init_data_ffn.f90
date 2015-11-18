PROGRAM TEST_MSG_INIT_DATA

USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_FUNLOC, C_NULL_PTR
USE ZMQ
USE ASSERT
IMPLICIT NONE

TYPE(C_PTR) :: CONTEXT
TYPE(C_PTR) :: SOCKET
INTEGER(KIND = C_INT) :: CODE
INTEGER(KIND = C_INT) :: RECV_VALUE
INTEGER(KIND = C_INT), TARGET, ALLOCATABLE :: SEND_VALUE

CALL OMP_SET_DYNAMIC(0)
CALL OMP_SET_NUM_THREADS(2)

CONTEXT = ZMQ_CTX_NEW()

!$OMP PARALLEL SHARED(CONTEXT,SEND_VALUE), PRIVATE(SOCKET,CODE,RECV_VALUE)
!$OMP SECTIONS

!$OMP SECTION

  SOCKET = ZMQ_SOCKET(CONTEXT, ZMQ_PAIR)
  CODE = ZMQ_CONNECT(SOCKET, 'inproc://fzmq')
  CALL ASSERT_EQUALS(0, CODE)

  ALLOCATE(INTEGER(KIND = C_INT) :: SEND_VALUE)
  SEND_VALUE = 123
  CALL SEND(SOCKET, SEND_VALUE)

  CODE = ZMQ_CLOSE(SOCKET)
  CALL ASSERT_EQUALS(0, CODE)

!$OMP SECTION

  SOCKET = ZMQ_SOCKET(CONTEXT, ZMQ_PAIR)
  CODE = ZMQ_BIND(SOCKET, 'inproc://fzmq')
  CALL ASSERT_EQUALS(0, CODE)

  CALL RECV(SOCKET, RECV_VALUE)
  CALL ASSERT_EQUALS(123, RECV_VALUE)

  CODE = ZMQ_CLOSE(SOCKET)
  CALL ASSERT_EQUALS(0, CODE)

!$OMP END SECTIONS
!$OMP END PARALLEL

CODE = ZMQ_CTX_TERM(CONTEXT)
CALL ASSERT_EQUALS(0, CODE)

CONTAINS

  SUBROUTINE FREE_FN(DATA, HINT)
    TYPE(C_PTR), VALUE, INTENT(IN) :: DATA
    TYPE(C_PTR), VALUE, INTENT(IN) :: HINT

    INTEGER(KIND = C_INT), POINTER :: VALUE

    CALL C_F_POINTER(DATA, VALUE)
    DEALLOCATE(VALUE)
  END SUBROUTINE

  SUBROUTINE SEND(SOCKET, VALUE)
    TYPE(C_PTR), INTENT(INOUT) :: SOCKET
    INTEGER(KIND = C_INT), POINTER, INTENT(IN) :: VALUE

    TYPE(C_PTR) :: DATA
    TYPE(C_FUNPTR) :: FFN
    TYPE(ZMQ_MSG_T) :: MESSAGE
    INTEGER(KIND = C_INT) :: CODE
    INTEGER(KIND = C_INT) :: NBYTES
    INTEGER(KIND = C_SIZE_T) :: SIZE

    DATA = C_LOC(VALUE)
    SIZE = C_SIZEOF(VALUE)
    FFN = C_FUNLOC(FREE_FN)
    CODE = ZMQ_MSG_INIT_DATA(MESSAGE, DATA, SIZE, FFN, C_NULL_PTR)
    CALL ASSERT_EQUALS(0, CODE)

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
