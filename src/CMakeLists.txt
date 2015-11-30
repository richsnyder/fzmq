# DISTRIBUTION STATEMENT A. Approved for public release; distribution is
# unlimited.  Granted clearance per 88ABW-2015-5731.
#
# This file is declared a work of the U.S. Government and is not subject to
# copyright protection in the United States.

CONFIGURE_FILE(zmq_constants.c.in zmq_constants.c)
SET_SOURCE_FILES_PROPERTIES(${CMAKE_CURRENT_BINARY_DIR}/zmq_constants.c GENERATED)
ADD_LIBRARY(fzmq STATIC ${CMAKE_CURRENT_BINARY_DIR}/zmq_constants.c zmq.f90)
TARGET_LINK_LIBRARIES(fzmq ${EXTRA_LIBS})
INSTALL(TARGETS fzmq ARCHIVE DESTINATION lib)
INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/zmq.mod DESTINATION include)
