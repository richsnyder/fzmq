PROGRAM TEST_HAS

USE ZMQ
USE ASSERT
IMPLICIT NONE

INTEGER(KIND = C_INT) :: CODE

CODE = ZMQ_HAS('ipc')
CALL ASSERT_EQUALS(1, CODE)

CODE = ZMQ_HAS('fail')
CALL ASSERT_EQUALS(0, CODE)

END PROGRAM
