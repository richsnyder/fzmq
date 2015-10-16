% zmq_ctx_new


Name
----

zmq_ctx_new - create a new ØMQ context


Synopsis
--------

~~~{.synopsis}
FUNCTION zmq_ctx_new() RESULT(context)

  TYPE(C_PTR) :: context
~~~


Description
-----------

The *zmq_ctx_new()* function creates a new ØMQ _context_.

This function replaces the deprecated function [zmq_init][].

### Thread safety

A ØMQ _context_ is thread safe and may be shared among as many application
threads as necessary, without any additional locking required on the part of
the caller.


Return value
------------

The *zmq_ctx_new()* function shall return an opaque handle to the newly created
_context_ if successful.  Otherwise it shall return *C_NULL_PTR* and set
_errno_ to one of the values defined below.


Errors
------

No error values are defined for this function.


Example
-------

### Create a new context

~~~{.example}
TYPE(C_PTR) :: context

context = zmq_ctx_new()
~~~


See also
--------

[zmq_ctx_set][]
[zmq_ctx_get][]
[zmq_ctx_term][]
