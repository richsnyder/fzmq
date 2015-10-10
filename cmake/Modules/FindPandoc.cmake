# ==============================================================================
#
# Find the Pandoc markdown converter
#
# ------------------------------------------------------------------------------
#
# Variables defined by this module include:
#   Pandoc_FOUND                 TRUE if Pandoc and all components are found
#   Pandoc_EXECUTABLE            Full path to the Pandoc executable
#   Pandoc_citeproc_FOUND        TRUE if citeproc is found
#   Pandoc_citeproc_EXECUTABLE   Full path to the Pandoc citeproc executable
#   Pandoc_VERSION               The version of the Pandoc program
#
# ==============================================================================

INCLUDE(FindPackageHandleStandardArgs)

IF(Pandoc_FIND_QUIETLY)
  SET(_FIND_PACKAGE_ARGS ${_FIND_PACKAGE_ARGS} QUIET)
ENDIF()
IF(Pandoc_FIND_REQUIRED)
  SET(_FIND_PACKAGE_ARGS ${_FIND_PACKAGE_ARGS} REQUIRED)
ENDIF()

FIND_PACKAGE(LATEX ${_FIND_PACKAGE_ARGS})
FIND_PROGRAM(Pandoc_EXECUTABLE NAMES pandoc)
LIST(FIND Pandoc_FIND_COMPONENTS citeproc is_requested)
IF(is_requested GREATER -1)
  FIND_PROGRAM(Pandoc_citeproc_EXECUTABLE NAMES pandoc-citeproc)
ENDIF()

IF(Pandoc_EXECUTABLE)
  EXECUTE_PROCESS(COMMAND ${Pandoc_EXECUTABLE} "--version"
      ERROR_VARIABLE Pandoc_VERSION
      OUTPUT_VARIABLE Pandoc_VERSION
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  STRING(REGEX
      REPLACE "^pandoc ([.0-9]+).*" "\\1"
      Pandoc_VERSION
      "${Pandoc_VERSION}")
ENDIF()

SET(Pandoc_REQUIRED_VARS Pandoc_EXECUTABLE)
IF(Pandoc_FIND_REQUIRED_citeproc)
  LIST(APPEND Pandoc_REQUIRED_VARS Pandoc_citeproc_EXECUTABLE)
ENDIF()

FIND_PACKAGE_HANDLE_STANDARD_ARGS(
    Pandoc
    FOUND_VAR Pandoc_FOUND
    REQUIRED_VARS ${Pandoc_REQUIRED_VARS}
    VERSION_VAR Pandoc_VERSION)
