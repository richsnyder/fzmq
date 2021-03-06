PROGRAM TEST_CONTEXT

USE ZMQ
USE ASSERT
IMPLICIT NONE

TYPE(C_PTR) :: CONTEXT
INTEGER(KIND = C_INT) :: CODE

CONTEXT = ZMQ_CTX_NEW()

CODE = ZMQ_CTX_SET(CONTEXT, ZMQ_MAX_SOCKETS, 512)
CALL ASSERT_EQUALS(0, CODE)

CODE = ZMQ_CTX_GET(CONTEXT, ZMQ_MAX_SOCKETS)
CALL ASSERT_EQUALS(512, CODE)

CODE = ZMQ_CTX_SHUTDOWN(CONTEXT)
CALL ASSERT_EQUALS(0, CODE)

CODE = ZMQ_CTX_TERM(CONTEXT)
CALL ASSERT_EQUALS(0, CODE)

END PROGRAM
