CMAKE_MINIMUM_REQUIRED(VERSION 3.0.0)

PROJECT(fzmq LANGUAGES Fortran VERSION 0.5.0)

# Add custom modules to the module search path and mark them for installation.

LIST(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules)

# Locate all of the required global dependencies.

FIND_PACKAGE(ZeroMQ REQUIRED)
INCLUDE_DIRECTORIES(${ZeroMQ_INCLUDE_DIRS})
LIST(APPEND EXTRA_LIBS ${ZeroMQ_LIBRARIES})

# Add the project subdirectories.

ADD_SUBDIRECTORY(src)

# Determine whether or not to build the unit tests.

OPTION(BUILD_TESTS "Build unit tests." ON)
IF (BUILD_TESTS)
  ENABLE_TESTING()
  ADD_SUBDIRECTORY(tests)
ENDIF()
