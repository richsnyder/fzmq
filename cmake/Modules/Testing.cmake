# Load and run select CMake files.

INCLUDE(CMakeParseArguments)

# Locate all of the required dependencies.

FIND_PACKAGE(OpenMP REQUIRED)

# Define functions.

FUNCTION(ADD_OPENMP_TEST A_NAME)

  # Parse the arguments to the function.
  CMAKE_PARSE_ARGUMENTS(A
    ""
    ""
    "SOURCE"
    ${ARGN}
  )

  ADD_EXECUTABLE(${A_NAME} ${A_SOURCE} assert.f90)
  SET_TARGET_PROPERTIES(${A_NAME} PROPERTIES COMPILE_FLAGS ${OpenMP_Fortran_FLAGS})
  SET_TARGET_PROPERTIES(${A_NAME} PROPERTIES LINK_FLAGS ${OpenMP_Fortran_FLAGS})
  TARGET_LINK_LIBRARIES(${A_NAME} ${EXTRA_LIBS})
  ADD_TEST(${A_NAME} ${A_NAME})

ENDFUNCTION()
