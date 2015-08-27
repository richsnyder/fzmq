CMAKE_MINIMUM_REQUIRED(VERSION 3.0.0)

PROJECT(fzmq LANGUAGES Fortran VERSION 0.3.0)

# Add custom modules to the module search path and mark them for installation.

LIST(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules)

# Locate all of the required dependencies.

FIND_PACKAGE(ZeroMQ REQUIRED)
INCLUDE_DIRECTORIES(${ZeroMQ_INCLUDE_DIRS})
LIST(APPEND EXTRA_LIBS ${ZeroMQ_LIBRARIES})

# Add the project subdirectories.

ADD_SUBDIRECTORY(src)
