# DISTRIBUTION STATEMENT A. Approved for public release; distribution is
# unlimited.  Granted clearance per 88ABW-2015-5731.
#
# This file is declared a work of the U.S. Government and is not subject to
# copyright protection in the United States.

# Load and run select CMake files.

INCLUDE(CMakeParseArguments)

# Define functions.

FUNCTION(ADD_OPENMP_TEST A_NAME)

  # Parse the arguments to the function.
  CMAKE_PARSE_ARGUMENTS(A
    ""
    ""
    "SOURCE"
    ${ARGN}
  )

  IF(NOT A_SOURCE)
    STRING(REPLACE "-" "_" A_SOURCE "${A_NAME}.f90")
  ENDIF()

  ADD_EXECUTABLE(${A_NAME} ${A_SOURCE} assert.f90)
  SET_TARGET_PROPERTIES(${A_NAME} PROPERTIES COMPILE_FLAGS ${OpenMP_Fortran_FLAGS})
  SET_TARGET_PROPERTIES(${A_NAME} PROPERTIES LINK_FLAGS ${OpenMP_Fortran_FLAGS})
  TARGET_LINK_LIBRARIES(${A_NAME} ${EXTRA_LIBS})
  ADD_TEST(NAME ${A_NAME} COMMAND ${A_NAME})

ENDFUNCTION()

# Include project build directories and libraries.

INCLUDE_DIRECTORIES(BEFORE ${CMAKE_CURRENT_BINARY_DIR}/../src)
LIST(APPEND EXTRA_LIBS fzmq)

# Add unit tests.

ADD_OPENMP_TEST(test-version)
ADD_OPENMP_TEST(test-has)
ADD_OPENMP_TEST(test-z85)
ADD_OPENMP_TEST(test-message)
ADD_OPENMP_TEST(test-context)
ADD_OPENMP_TEST(test-error)
ADD_OPENMP_TEST(test-socket)
ADD_OPENMP_TEST(test-socket-options)
ADD_OPENMP_TEST(test-send-recv)
ADD_OPENMP_TEST(test-send-recv-string)
ADD_OPENMP_TEST(test-send-recv-message)
ADD_OPENMP_TEST(test-send-recv-multipart)
ADD_OPENMP_TEST(test-send-const)
ADD_OPENMP_TEST(test-msg-init-data)
ADD_OPENMP_TEST(test-msg-init-data-ffn)
ADD_OPENMP_TEST(test-msg-get)
ADD_OPENMP_TEST(test-poll)
ADD_OPENMP_TEST(test-socket-monitor)
ADD_OPENMP_TEST(test-proxy-steerable)
ADD_OPENMP_TEST(test-context-deprecated)
ADD_OPENMP_TEST(test-send-recv-deprecated)
