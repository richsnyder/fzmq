MODULE ASSERT

  USE, INTRINSIC :: ISO_C_BINDING,  ONLY : &
      C_INT32_T, C_INT64_T, &
      C_FLOAT, C_DOUBLE
  IMPLICIT NONE
  PRIVATE

  INTERFACE ASSERT_ARRAY_EQUALS
    MODULE PROCEDURE &
        ASSERT_ARRAY_EQUALS_I32, ASSERT_ARRAY_EQUALS_I32_MSG, &
        ASSERT_ARRAY_EQUALS_I64, ASSERT_ARRAY_EQUALS_I64_MSG, &
        ASSERT_ARRAY_EQUALS_R32, ASSERT_ARRAY_EQUALS_R32_MSG, &
        ASSERT_ARRAY_EQUALS_R64, ASSERT_ARRAY_EQUALS_R64_MSG
  END INTERFACE

  INTERFACE ASSERT_EQUALS
    MODULE PROCEDURE &
        ASSERT_EQUALS_I32, ASSERT_EQUALS_I32_MSG, &
        ASSERT_EQUALS_I64, ASSERT_EQUALS_I64_MSG, &
        ASSERT_EQUALS_R32, ASSERT_EQUALS_R32_MSG, &
        ASSERT_EQUALS_R64, ASSERT_EQUALS_R64_MSG
  END INTERFACE

  INTERFACE ASSERT_NOT_EQUALS
    MODULE PROCEDURE &
        ASSERT_NOT_EQUALS_I32, ASSERT_NOT_EQUALS_I32_MSG, &
        ASSERT_NOT_EQUALS_I64, ASSERT_NOT_EQUALS_I64_MSG, &
        ASSERT_NOT_EQUALS_R32, ASSERT_NOT_EQUALS_R32_MSG, &
        ASSERT_NOT_EQUALS_R64, ASSERT_NOT_EQUALS_R64_MSG
  END INTERFACE

  INTERFACE ASSERT_FALSE
    MODULE PROCEDURE ASSERT_FALSE_, ASSERT_FALSE_MSG
  END INTERFACE

  INTERFACE ASSERT_TRUE
    MODULE PROCEDURE ASSERT_TRUE_, ASSERT_TRUE_MSG
  END INTERFACE

  INTERFACE FAIL
    MODULE PROCEDURE FAIL_, FAIL_MSG
  END INTERFACE

  INTERFACE RAISE
    MODULE PROCEDURE RAISE_1, RAISE_2, RAISE_3
  END INTERFACE

  PUBLIC :: ASSERT_ARRAY_EQUALS
  PUBLIC :: ASSERT_EQUALS
  PUBLIC :: ASSERT_FALSE
  PUBLIC :: ASSERT_TRUE
  PUBLIC :: FAIL

CONTAINS

  SUBROUTINE ASSERT_ARRAY_EQUALS_I32(expected, actual)
    INTEGER(KIND = C_INT32_T), DIMENSION(:), INTENT(IN) :: expected
    INTEGER(KIND = C_INT32_T), DIMENSION(:), INTENT(IN) :: actual
    INTEGER :: pos
    IF (SIZE(expected) /= SIZE(actual)) THEN
      CALL RAISE('assert_array_equals', &
          TRIM(SIZE_ERRMSG(SIZE(expected), SIZE(actual))))
    END IF
    DO pos = 1, SIZE(expected)
      IF (expected(pos) /= actual(pos)) THEN
        CALL RAISE('assert_array_equals', &
                   TRIM(AT_ERRMSG(pos)) // '; ' // &
                   TRIM(EQ_ERRMSG_I32(expected(pos), actual(pos))))
      END IF
    END DO
  END SUBROUTINE

  SUBROUTINE ASSERT_ARRAY_EQUALS_I32_MSG(message, expected, actual)
    CHARACTER(LEN = *), INTENT(IN) :: message
    INTEGER(KIND = C_INT32_T), DIMENSION(:), INTENT(IN) :: expected
    INTEGER(KIND = C_INT32_T), DIMENSION(:), INTENT(IN) :: actual
    INTEGER :: pos
    IF (SIZE(expected) /= SIZE(actual)) THEN
      CALL RAISE('assert_array_equals', &
                 TRIM(SIZE_ERRMSG(SIZE(expected), SIZE(actual))))
    END IF
    DO pos = 1, SIZE(expected)
      IF (expected(pos) /= actual(pos)) THEN
        CALL RAISE('assert_array_equals', &
                   TRIM(AT_ERRMSG(pos)) // '; ' // &
                   TRIM(EQ_ERRMSG_I32(expected(pos), actual(pos))), &
                   message)
      END IF
    END DO
  END SUBROUTINE

  SUBROUTINE ASSERT_ARRAY_EQUALS_I64(expected, actual)
    INTEGER(KIND = C_INT64_T), DIMENSION(:), INTENT(IN) :: expected
    INTEGER(KIND = C_INT64_T), DIMENSION(:), INTENT(IN) :: actual
    INTEGER :: pos
    IF (SIZE(expected) /= SIZE(actual)) THEN
      CALL RAISE('assert_array_equals', &
                 TRIM(SIZE_ERRMSG(SIZE(expected), SIZE(actual))))
    END IF
    DO pos = 1, SIZE(expected)
      IF (expected(pos) /= actual(pos)) THEN
        CALL RAISE('assert_array_equals', &
                   TRIM(AT_ERRMSG(pos)) // '; ' // &
                   TRIM(EQ_ERRMSG_I64(expected(pos), actual(pos))))
      END IF
    END DO
  END SUBROUTINE

  SUBROUTINE ASSERT_ARRAY_EQUALS_I64_MSG(message, expected, actual)
    CHARACTER(LEN = *), INTENT(IN) :: message
    INTEGER(KIND = C_INT64_T), DIMENSION(:), INTENT(IN) :: expected
    INTEGER(KIND = C_INT64_T), DIMENSION(:), INTENT(IN) :: actual
    INTEGER :: pos
    IF (SIZE(expected) /= SIZE(actual)) THEN
      CALL RAISE('assert_array_equals', &
                 TRIM(SIZE_ERRMSG(SIZE(expected), SIZE(actual))))
    END IF
    DO pos = 1, SIZE(expected)
      IF (expected(pos) /= actual(pos)) THEN
        CALL RAISE('assert_array_equals', &
                   TRIM(AT_ERRMSG(pos)) // '; ' // &
                   TRIM(EQ_ERRMSG_I64(expected(pos), actual(pos))), &
                   message)
      END IF
    END DO
  END SUBROUTINE

  SUBROUTINE ASSERT_ARRAY_EQUALS_R32(expected, actual, delta)
    REAL(KIND = C_FLOAT), DIMENSION(:), INTENT(IN) :: expected
    REAL(KIND = C_FLOAT), DIMENSION(:), INTENT(IN) :: actual
    REAL(KIND = C_FLOAT), INTENT(IN) :: delta
    INTEGER :: pos
    IF (SIZE(expected) /= SIZE(actual)) THEN
      CALL RAISE('assert_array_equals', &
                 TRIM(SIZE_ERRMSG(SIZE(expected), SIZE(actual))))
    END IF
    DO pos = 1, SIZE(expected)
      IF (ABS(actual(pos) - expected(pos)) > ABS(delta)) THEN
        CALL RAISE('assert_array_equals', &
                   TRIM(AT_ERRMSG(pos)) // '; ' // &
                   TRIM(EQ_ERRMSG_R32(expected(pos), actual(pos))))
      END IF
    END DO
  END SUBROUTINE

  SUBROUTINE ASSERT_ARRAY_EQUALS_R32_MSG(message, expected, actual, delta)
    CHARACTER(LEN = *), INTENT(IN) :: message
    REAL(KIND = C_FLOAT), DIMENSION(:), INTENT(IN) :: expected
    REAL(KIND = C_FLOAT), DIMENSION(:), INTENT(IN) :: actual
    REAL(KIND = C_FLOAT), INTENT(IN) :: delta
    INTEGER :: pos
    IF (SIZE(expected) /= SIZE(actual)) THEN
      CALL RAISE('assert_array_equals', &
                 TRIM(SIZE_ERRMSG(SIZE(expected), SIZE(actual))))
    END IF
    DO pos = 1, SIZE(expected)
      IF (ABS(actual(pos) - expected(pos)) > ABS(delta)) THEN
        CALL RAISE('assert_array_equals', &
                   TRIM(AT_ERRMSG(pos)) // '; ' // &
                   TRIM(EQ_ERRMSG_R32(expected(pos), actual(pos))), &
                   message)
      END IF
    END DO
  END SUBROUTINE

  SUBROUTINE ASSERT_ARRAY_EQUALS_R64(expected, actual, delta)
    REAL(KIND = C_DOUBLE), DIMENSION(:), INTENT(IN) :: expected
    REAL(KIND = C_DOUBLE), DIMENSION(:), INTENT(IN) :: actual
    REAL(KIND = C_DOUBLE), INTENT(IN) :: delta
    INTEGER :: pos
    IF (SIZE(expected) /= SIZE(actual)) THEN
      CALL RAISE('assert_array_equals', &
                 TRIM(SIZE_ERRMSG(SIZE(expected), SIZE(actual))))
    END IF
    DO pos = 1, SIZE(expected)
      IF (ABS(actual(pos) - expected(pos)) > ABS(delta)) THEN
        CALL RAISE('assert_array_equals', &
                   TRIM(AT_ERRMSG(pos)) // '; ' // &
                   TRIM(EQ_ERRMSG_R64(expected(pos), actual(pos))))
      END IF
    END DO
  END SUBROUTINE

  SUBROUTINE ASSERT_ARRAY_EQUALS_R64_MSG(message, expected, actual, delta)
    CHARACTER(LEN = *), INTENT(IN) :: message
    REAL(KIND = C_DOUBLE), DIMENSION(:), INTENT(IN) :: expected
    REAL(KIND = C_DOUBLE), DIMENSION(:), INTENT(IN) :: actual
    REAL(KIND = C_DOUBLE), INTENT(IN) :: delta
    INTEGER :: pos
    IF (SIZE(expected) /= SIZE(actual)) THEN
      CALL RAISE('assert_array_equals', &
                 TRIM(SIZE_ERRMSG(SIZE(expected), SIZE(actual))))
    END IF
    DO pos = 1, SIZE(expected)
      IF (ABS(actual(pos) - expected(pos)) > ABS(delta)) THEN
        CALL RAISE('assert_array_equals', &
                   TRIM(AT_ERRMSG(pos)) // '; ' // &
                   TRIM(EQ_ERRMSG_R64(expected(pos), actual(pos))), &
                   message)
      END IF
    END DO
  END SUBROUTINE

  SUBROUTINE ASSERT_EQUALS_I32(expected, actual)
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: expected
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: actual
    IF (expected /= actual) THEN
      CALL RAISE('assert_equals', &
                 TRIM(EQ_ERRMSG_I32(expected, actual)))
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_EQUALS_I32_MSG(message, expected, actual)
    CHARACTER(LEN = *), INTENT(IN) :: message
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: expected
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: actual
    IF (expected /= actual) THEN
      CALL RAISE('assert_equals', &
                 TRIM(EQ_ERRMSG_I32(expected, actual)), &
                 message)
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_EQUALS_I64(expected, actual)
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: expected
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: actual
    IF (expected /= actual) THEN
      CALL RAISE('assert_equals', &
                 TRIM(EQ_ERRMSG_I64(expected, actual)))
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_EQUALS_I64_MSG(message, expected, actual)
    CHARACTER(LEN = *), INTENT(IN) :: message
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: expected
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: actual
    IF (expected /= actual) THEN
      CALL RAISE('assert_equals', &
                 TRIM(EQ_ERRMSG_I64(expected, actual)), &
                 message)
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_EQUALS_R32(expected, actual, delta)
    REAL(KIND = C_FLOAT), INTENT(IN) :: expected
    REAL(KIND = C_FLOAT), INTENT(IN) :: actual
    REAL(KIND = C_FLOAT), INTENT(IN) :: delta
    IF (ABS(actual - expected) > ABS(delta)) THEN
      CALL RAISE('assert_equals', &
                 TRIM(EQ_ERRMSG_R32(expected, actual)))
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_EQUALS_R32_MSG(message, expected, actual, delta)
    CHARACTER(LEN = *), INTENT(IN) :: message
    REAL(KIND = C_FLOAT), INTENT(IN) :: expected
    REAL(KIND = C_FLOAT), INTENT(IN) :: actual
    REAL(KIND = C_FLOAT), INTENT(IN) :: delta
    IF (ABS(actual - expected) > ABS(delta)) THEN
      CALL RAISE('assert_equals', &
                 TRIM(EQ_ERRMSG_R32(expected, actual)), &
                 message)
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_EQUALS_R64(expected, actual, delta)
    REAL(KIND = C_DOUBLE), INTENT(IN) :: expected
    REAL(KIND = C_DOUBLE), INTENT(IN) :: actual
    REAL(KIND = C_DOUBLE), INTENT(IN) :: delta
    IF (ABS(actual - expected) > ABS(delta)) THEN
      CALL RAISE('assert_equals', &
                 TRIM(EQ_ERRMSG_R64(expected, actual)))
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_EQUALS_R64_MSG(message, expected, actual, delta)
    CHARACTER(LEN = *), INTENT(IN) :: message
    REAL(KIND = C_DOUBLE), INTENT(IN) :: expected
    REAL(KIND = C_DOUBLE), INTENT(IN) :: actual
    REAL(KIND = C_DOUBLE), INTENT(IN) :: delta
    IF (ABS(actual - expected) > ABS(delta)) THEN
      CALL RAISE('assert_equals', &
                 TRIM(EQ_ERRMSG_R64(expected, actual)), &
                 message)
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_NOT_EQUALS_I32(expected, actual)
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: expected
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: actual
    IF (expected == actual) THEN
      CALL RAISE('assert_equals', &
                 TRIM(NEQ_ERRMSG_I32(actual)))
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_NOT_EQUALS_I32_MSG(message, expected, actual)
    CHARACTER(LEN = *), INTENT(IN) :: message
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: expected
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: actual
    IF (expected == actual) THEN
      CALL RAISE('assert_equals', &
                 TRIM(NEQ_ERRMSG_I32(actual)), &
                 message)
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_NOT_EQUALS_I64(expected, actual)
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: expected
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: actual
    IF (expected == actual) THEN
      CALL RAISE('assert_equals', &
                 TRIM(NEQ_ERRMSG_I64(actual)))
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_NOT_EQUALS_I64_MSG(message, expected, actual)
    CHARACTER(LEN = *), INTENT(IN) :: message
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: expected
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: actual
    IF (expected == actual) THEN
      CALL RAISE('assert_equals', &
                 TRIM(NEQ_ERRMSG_I64(actual)), &
                 message)
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_NOT_EQUALS_R32(expected, actual, delta)
    REAL(KIND = C_FLOAT), INTENT(IN) :: expected
    REAL(KIND = C_FLOAT), INTENT(IN) :: actual
    REAL(KIND = C_FLOAT), INTENT(IN) :: delta
    IF (ABS(actual - expected) < ABS(delta)) THEN
      CALL RAISE('assert_equals', &
                 TRIM(NEQ_ERRMSG_R32(actual)))
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_NOT_EQUALS_R32_MSG(message, expected, actual, delta)
    CHARACTER(LEN = *), INTENT(IN) :: message
    REAL(KIND = C_FLOAT), INTENT(IN) :: expected
    REAL(KIND = C_FLOAT), INTENT(IN) :: actual
    REAL(KIND = C_FLOAT), INTENT(IN) :: delta
    IF (ABS(actual - expected) < ABS(delta)) THEN
      CALL RAISE('assert_equals', &
                 TRIM(NEQ_ERRMSG_R32(actual)), &
                 message)
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_NOT_EQUALS_R64(expected, actual, delta)
    REAL(KIND = C_DOUBLE), INTENT(IN) :: expected
    REAL(KIND = C_DOUBLE), INTENT(IN) :: actual
    REAL(KIND = C_DOUBLE), INTENT(IN) :: delta
    IF (ABS(actual - expected) < ABS(delta)) THEN
      CALL RAISE('assert_equals', &
                 TRIM(NEQ_ERRMSG_R64(actual)))
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_NOT_EQUALS_R64_MSG(message, expected, actual, delta)
    CHARACTER(LEN = *), INTENT(IN) :: message
    REAL(KIND = C_DOUBLE), INTENT(IN) :: expected
    REAL(KIND = C_DOUBLE), INTENT(IN) :: actual
    REAL(KIND = C_DOUBLE), INTENT(IN) :: delta
    IF (ABS(actual - expected) < ABS(delta)) THEN
      CALL RAISE('assert_equals', &
                 TRIM(NEQ_ERRMSG_R64(actual)), &
                 message)
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_FALSE_(condition)
    LOGICAL, INTENT(IN) :: condition
    IF (condition) THEN
      CALL RAISE('assert_false')
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_FALSE_MSG(message, condition)
    CHARACTER(LEN = *), INTENT(IN) :: message
    LOGICAL, INTENT(IN) :: condition
    IF (.NOT. condition) THEN
      CALL RAISE('assert_false', message)
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_TRUE_(condition)
    LOGICAL, INTENT(IN) :: condition
    IF (.NOT. condition) THEN
      CALL RAISE('assert_true')
    END IF
  END SUBROUTINE

  SUBROUTINE ASSERT_TRUE_MSG(message, condition)
    CHARACTER(LEN = *), INTENT(IN) :: message
    LOGICAL, INTENT(IN) :: condition
    IF (.NOT. condition) THEN
      CALL RAISE('assert_true', message)
    END IF
  END SUBROUTINE

  SUBROUTINE FAIL_()
    CALL RAISE('fail')
  END SUBROUTINE

  SUBROUTINE FAIL_MSG(message)
    CHARACTER(LEN = *), INTENT(IN) :: message
    CALL RAISE('fail', message)
  END SUBROUTINE

  SUBROUTINE RAISE_1(method)
    CHARACTER(LEN = *), INTENT(IN) :: method
    WRITE(*, '(''FAIL('', A, '')'')') method
    CALL EXIT(-1)
  END SUBROUTINE

  SUBROUTINE RAISE_2(method, explain)
    CHARACTER(LEN = *), INTENT(IN) :: method
    CHARACTER(LEN = *), INTENT(IN) :: explain
    WRITE(*, '(''FAIL('', A, ''): '', A)') method, explain
    CALL EXIT(-1)
  END SUBROUTINE

  SUBROUTINE RAISE_3(method, explain, message)
    CHARACTER(LEN = *), INTENT(IN) :: method
    CHARACTER(LEN = *), INTENT(IN) :: explain
    CHARACTER(LEN = *), INTENT(IN) :: message
    WRITE(*, '(''FAIL('', A, ''): '', A, '' '', A)') method, message, explain
    CALL EXIT(-1)
  END SUBROUTINE

  FUNCTION STRINGIZE_I32(val) RESULT(str)
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: val
    CHARACTER(LEN = 12) :: str
    WRITE(str, '(I12)') val
    str = ADJUSTL(str)
  END FUNCTION

  FUNCTION STRINGIZE_I64(val) RESULT(str)
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: val
    CHARACTER(LEN = 20) :: str
    WRITE(str, '(I20)') val
    str = ADJUSTL(str)
  END FUNCTION

  FUNCTION STRINGIZE_R32(val) RESULT(str)
    REAL(KIND = C_FLOAT), INTENT(IN) :: val
    CHARACTER(LEN = 16) :: str
    WRITE(str, '(e16.9)') val
  END FUNCTION

  FUNCTION STRINGIZE_R64(val) RESULT(str)
    REAL(KIND = C_DOUBLE), INTENT(IN) :: val
    CHARACTER(LEN = 16) :: str
    WRITE(str, '(E16.9)') val
  END FUNCTION

  FUNCTION SIZE_ERRMSG(expected, actual) RESULT(message)
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: expected
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: actual
    CHARACTER(LEN = 59) :: fmt
    CHARACTER(LEN = 51) :: message
    fmt = '(''expected array length ['', A, ''] but was ['', A, '']'')'
    WRITE(message, fmt) TRIM(STRINGIZE_I32(expected)), &
                        TRIM(STRINGIZE_I32(actual))
  END FUNCTION

  FUNCTION AT_ERRMSG(pos) RESULT(message)
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: pos
    CHARACTER(LEN = 49) :: fmt
    CHARACTER(LEN = 47) :: message
    fmt = '(''arrays first differed at element ['', A,'']'')'
    WRITE(message, fmt) TRIM(STRINGIZE_I32(pos))
  END FUNCTION

  FUNCTION EQ_ERRMSG_I32(expected, actual) RESULT(message)
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: expected
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: actual
    CHARACTER(LEN = 46) :: fmt
    CHARACTER(LEN = 46) :: message
    fmt = '(''expected <'', A, ''> but was <'', A, ''>'')'
    WRITE(message, fmt) TRIM(STRINGIZE_I32(expected)), &
                        TRIM(STRINGIZE_I32(actual))
  END FUNCTION

  FUNCTION EQ_ERRMSG_I64(expected, actual) RESULT(message)
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: expected
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: actual
    CHARACTER(LEN = 46) :: fmt
    CHARACTER(LEN = 62) :: message
    fmt = '(''expected <'', A, ''> but was <'', A, ''>'')'
    WRITE(message, fmt) TRIM(STRINGIZE_I64(expected)), &
                        TRIM(STRINGIZE_I64(actual))
  END FUNCTION

  FUNCTION EQ_ERRMSG_R32(expected, actual) RESULT(message)
    REAL(KIND = C_FLOAT), INTENT(IN) :: expected
    REAL(KIND = C_FLOAT), INTENT(IN) :: actual
    CHARACTER(LEN = 46) :: fmt
    CHARACTER(LEN = 54) :: message
    fmt = '(''expected <'', A, ''> but was <'', A, ''>'')'
    WRITE(message, fmt) TRIM(STRINGIZE_R32(expected)), &
                        TRIM(STRINGIZE_R32(actual))
  END FUNCTION

  FUNCTION EQ_ERRMSG_R64(expected, actual) RESULT(message)
    REAL(KIND = C_DOUBLE), INTENT(IN) :: expected
    REAL(KIND = C_DOUBLE), INTENT(IN) :: actual
    CHARACTER(LEN = 46) :: fmt
    CHARACTER(LEN = 54) :: message
    fmt = '(''expected <'', A, ''> but was <'', A, ''>'')'
    WRITE(message, fmt) TRIM(STRINGIZE_R64(expected)), &
                        TRIM(STRINGIZE_R64(actual))
  END FUNCTION

  FUNCTION NEQ_ERRMSG_I32(actual) RESULT(message)
    INTEGER(KIND = C_INT32_T), INTENT(IN) :: actual
    CHARACTER(LEN = 21) :: message
    WRITE(message, '(''actual <'', A, ''>'')') TRIM(STRINGIZE_I32(actual))
  END FUNCTION

  FUNCTION NEQ_ERRMSG_I64(actual) RESULT(message)
    INTEGER(KIND = C_INT64_T), INTENT(IN) :: actual
    CHARACTER(LEN = 29) :: message
    WRITE(message, '(''actual <'', A, ''>'')') TRIM(STRINGIZE_I64(actual))
  END FUNCTION

  FUNCTION NEQ_ERRMSG_R32(actual) RESULT(message)
    REAL(KIND = C_FLOAT), INTENT(IN) :: actual
    CHARACTER(LEN = 25) :: message
    WRITE(message, '(''actual <'', A, ''>'')') TRIM(STRINGIZE_R32(actual))
  END FUNCTION

  FUNCTION NEQ_ERRMSG_R64(actual) RESULT(message)
    REAL(KIND = C_DOUBLE), INTENT(IN) :: actual
    CHARACTER(LEN = 25) :: message
    WRITE(message, '(''actual <'', A, ''>'')') TRIM(STRINGIZE_R64(actual))
  END FUNCTION

END MODULE ASSERT
