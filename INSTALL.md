FZMQ Installation Instructions
==============================

Introduction
------------

The generic process for setting up FZMQ on a Unix-like or Mac OS X system,
with default configuration options, is:

~~~
mkdir build
cd build
cmake /path/to/fzmq-x.y.z
make install
~~~

Detailed instructions are provided below.


Prerequisites
-------------

FZMQ relies on a handful of external packages that are not included in the FZMQ
distribution.  The following are required:

CMake
  ~ The CMake build system is used to manage compilation, testing, and
    installation.  It can be downloaded at <https://cmake.org/download/>.
    CMake versions 3.0 and above provide the necessary functionality.

C compiler
  ~ The C compiler must support the C99 standard.  The following compilers are
    known to work: gcc 5.2, Apple Clang 7.0.

Fortran compiler
  ~ FZMQ relies on features found in the Fortran 2003 standard.  The following
    compilers are known to work: gfortran 5.2.

libzmq
  ~ FZMQ interfaces with the native C library of ØMQ.  The library can be found
    at <http://zeromq.org/intro:get-the-software/>.  Version 4.1 or above is
    required.

Additionally, the FZMQ unit tests use OpenMP.  If you wish to compile and run
the unit tests, then your C and Fortran compilers must support OpenMP.

The pandoc document conversion tool is used to generate documentation.  If you
wish to build the documentation, then you will need pandoc installed on your
system.  It can be found at <http://pandoc.org/>.


Configuration
-------------

Before you can build the software artifacts from the source code, you need to
generate a set of build instructions that are suitable for your system.  FZMQ
uses the CMake program to make sure that all prerequisites are satisfied and to
see that the build process is properly configured.

FZMQ can be configured and built in a different location than the root
distribution directory.  This is called an *out-of-source* build and is
recommended because it keeps all intermediate files separate from the original
source.  To prepare for an out-of-source build, create a temporary directory
and run `cmake` from within this directory.

~~~
mkdir build
cd build
cmake /path/to/fzmq-x.y.z
~~~

The `cmake` program runs without user interaction.   Text- and graphics-based
user interfaces for CMake may be installed on your system.  These will be named
`ccmake` and `cmake-gui`, respectively, and may be used in place of `cmake`.


### Compilers

CMake will probe your system for common C and Fortran compilers and select the
first ones that it finds.  If it has trouble finding suitable compilers or if
you wish to override its choices, you can force the choice as follows:

~~~
CC=icc FC=ifort cmake /path/to/fzmq-x.y.z
~~~

### Installation prefix

By default, the FZMQ artifacts will be installed under `/usr/local`.  The
installation location can be changed by setting the `CMAKE_INSTALL_PREFIX`
CMake option.

~~~
cmake -DCMAKE_INSTALL_PREFIX=/path/to/install
~~~

### Unit tests

The FZMQ unit tests are not built by default.  These tests can be enabled as
follows:

~~~
cmake -DBUILD_TESTS=ON /path/to/fzmq-x.y.z
~~~

The unit tests require OpenMP, so enabling the tests requires C and Fortran
compilers that support OpenMP.

### Documentation

The FZMQ documentation is built by default.  You can disable generation of the
documentation by disabling the `BUILD_DOCS` option.

~~~
cmake -DBUILD_DOCS=OFF /path/to/fzmq-x.y.z
~~~


Building the library
--------------------

After the build has been configured with CMake, you can generate the FZMQ
artifacts by executing:

~~~
make
~~~


Testing the library
-------------------

If testing was enabled during the configuration, you can run the tests through
the `make` program.

~~~
make test
~~~


Installing the library
----------------------

The final phase of the build process is to install the artifacts.

~~~
make install
~~~


Linking with your application
-----------------------------

Once FZMQ has been installed, you can link your own programs against the
library like this:

~~~
gfortran -L /path/to/fzmq/lib -I /path/to/fzmq/include -o myprog myprog.f90 -lfzmq -lzmq
~~~
