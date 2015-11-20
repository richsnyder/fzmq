# This file is declared a work of the U.S. Government and is not subject to
# copyright protection in the United States.

CMAKE_MINIMUM_REQUIRED(VERSION 3.0.0 FATAL_ERROR)

PROJECT(fzmq LANGUAGES C Fortran VERSION 0.7.1)

# Add custom modules to the module search path and mark them for installation.

LIST(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules)

# Locate all of the required dependencies.

FIND_PACKAGE(ZeroMQ 4.1.0 REQUIRED)
INCLUDE_DIRECTORIES(${ZeroMQ_INCLUDE_DIRS})
LIST(APPEND EXTRA_LIBS ${ZeroMQ_LIBRARIES})

# Add the project subdirectory.

ADD_SUBDIRECTORY(src)

# Determine whether or not to build the unit tests.

OPTION(BUILD_TESTS "Build unit tests." OFF)
IF (BUILD_TESTS)
  FIND_PACKAGE(OpenMP REQUIRED)
  ENABLE_TESTING()
  ADD_SUBDIRECTORY(tests)
ENDIF()

# Determine whether or not to build the documentation.

OPTION(BUILD_DOCS "Build documentation." ON)
IF (BUILD_DOCS)
  FIND_PACKAGE(Pandoc REQUIRED)
  ADD_SUBDIRECTORY(doc)
ENDIF()
