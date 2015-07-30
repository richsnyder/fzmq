# ==============================================================================
#
# Find the ZeroMQ high-level socket library
#
# ------------------------------------------------------------------------------
#
# Variables defined by this module include:
#   ZeroMQ_FOUND            If false, do not try to use ZeroMQ
#   ZeroMQ_INCLUDE_DIRS     Where to find the header files needed to use ZeroMQ
#   ZeroMQ_LIBRARIES        The libraries needed to use ZeroMQ
#   ZeroMQ_VERSION          The version of the ZeroMQ library
# Also defined, but not for general use, are:
#   ZeroMQ_INCLUDE_DIR      Where to find the ZeroMQ header files
#   ZeroMQ_LIBRARY          Where to find the ZeroMQ library
#
# ------------------------------------------------------------------------------
#
# Copyright (c) 2014, Richard Snyder
#
# Use, modification, and distribution is subject to the BSD License.
# See the accompanying file LICENSE.md for the full license.
# Terms can also be found at http://creativecommons.org/licenses/BSD/.
#
# ==============================================================================

FIND_PATH(ZeroMQ_INCLUDE_DIR zmq.h)
FIND_LIBRARY(ZeroMQ_LIBRARY NAMES zmq)

IF(ZeroMQ_INCLUDE_DIR)
  SET(zmq_header "${ZeroMQ_INCLUDE_DIR}/zmq.h")
  FILE(STRINGS "${zmq_header}" major REGEX "define +ZMQ_VERSION_MAJOR")
  FILE(STRINGS "${zmq_header}" minor REGEX "define +ZMQ_VERSION_MINOR")
  FILE(STRINGS "${zmq_header}" patch REGEX "define +ZMQ_VERSION_PATCH")
  STRING(REGEX REPLACE ".+([0-9]+)" "\\1" major ${major})
  STRING(REGEX REPLACE ".+([0-9]+)" "\\1" minor ${minor})
  STRING(REGEX REPLACE ".+([0-9]+)" "\\1" patch ${patch})
  STRING(STRIP "${major}" major)
  STRING(STRIP "${minor}" minor)
  STRING(STRIP "${patch}" patch)
  SET(ZeroMQ_VERSION "${major}.${minor}.${patch}")
ENDIF(ZeroMQ_INCLUDE_DIR)

INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(ZeroMQ
  FOUND_VAR ZeroMQ_FOUND
  REQUIRED_VARS ZeroMQ_LIBRARY ZeroMQ_INCLUDE_DIR
  VERSION_VAR ZeroMQ_VERSION)

IF(ZeroMQ_FOUND)
  SET(ZeroMQ_INCLUDE_DIRS ${ZeroMQ_INCLUDE_DIR})
  SET(ZeroMQ_LIBRARIES ${ZeroMQ_LIBRARY})
ENDIF(ZeroMQ_FOUND)

MARK_AS_ADVANCED(ZeroMQ_INCLUDE_DIR ZeroMQ_LIBRARY)
