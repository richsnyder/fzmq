FZMQ
====


Introduction
------------

ØMQ is a lightweight messaging kernel library that extends standard socket
interfaces.  ØMQ sockets provide an abstraction of asynchronous message queues,
multiple messaging patterns, message filtering, seamless access to multiple
transport protocols, and more.

This library provides a Fortran 2003 binding to the ØMQ kernel.  It interfaces
with the native C library, available separately at <http://www.zeromq.org>.
The Fortran API was designed to follow the C API as closely as practicable,
with departures targeted at better compatibility with Fortran.


Installation
------------

Installation instructions can be found in the `INSTALL.md` file.


Character strings
-----------------

Fortran character strings are not null terminated as they are in C.  The FZMQ
library handles null termination for you, so ordinary Fortran strings can be
passed as arguments.  However, this makes functions with string arguments run a
bit more slowly than their C counterparts.  If performance is an issue, these
functions have faster, alternate versions that leave the responsibility of null
termination to the user.  These functions begin with an `FZMQ` prefix, but are
otherwise identical to the default form.  For example, the following two
function calls are equivalent:

~~~
RC = ZMQ_BIND(SOCKET, 'tcp://*:5555')
RC = FZMQ_BIND(SOCKET, 'tcp://*:5555' // C_NULL_CHAR)
~~~


License
-------

FZMQ is, per 17 USC § 101, a joint work of iMatix Corporation and Contributors
and the United States Government.  Portions developed solely by U.S. Government
personnel are not subject to copyright protections in the United States.  These
portions are individually marked.  The joint work as a whole is subject to the
terms of the Mozilla Public License, v 2.0.  Terms of use can be found in
`LICENSE.md` or at <http://mozilla.org/MPL/2.0/>.

ØMQ is copyright (c) 2007-2015 iMatix Corporation and Contributors.  ØMQ is
free software licensed under the LGPL.  ØMQ, ZeroMQ, and 0MQ are trademarks of
iMatrix Corporation.  Terms of use can be found in `COPYING.LESSER` or at
<http://opensource.org/licenses/LGPL-3.0/>.


Acknowledgements
----------------

FZMQ documentation and some examples are adapted from libzmq.
