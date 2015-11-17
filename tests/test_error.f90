PROGRAM TEST_ERROR

USE, INTRINSIC :: ISO_C_BINDING
USE ZMQ
USE ASSERT
IMPLICIT NONE

TYPE(C_PTR) :: CONTEXT
INTEGER(KIND = C_INT) :: CODE
CHARACTER(LEN = 20) :: ERRMSG

CONTEXT = ZMQ_CTX_NEW()

CODE = ZMQ_CTX_SET(CONTEXT, 1234, 0)
CALL ASSERT_EQUALS(-1, CODE)

CODE = ZMQ_ERRNO()
CALL ASSERT_EQUALS(22, CODE)

ERRMSG = ZMQ_STRERROR(CODE)
CALL ASSERT_TRUE(ERRMSG == 'Invalid argument')

END PROGRAM
