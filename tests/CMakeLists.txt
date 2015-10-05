# Load and run select CMake files.

INCLUDE(Testing)

# Include project build directories and libraries.

INCLUDE_DIRECTORIES(BEFORE ${CMAKE_CURRENT_BINARY_DIR}/../src)
LIST(APPEND EXTRA_LIBS fzmq)

# Add unit tests.

ADD_OPENMP_TEST(test-version SOURCE test_version.f90)
ADD_OPENMP_TEST(test-has SOURCE test_has.f90)
ADD_OPENMP_TEST(test-z85 SOURCE test_z85.f90)
ADD_OPENMP_TEST(test-message SOURCE test_message.f90)
ADD_OPENMP_TEST(test-context SOURCE test_context.f90)
ADD_OPENMP_TEST(test-error SOURCE test_error.f90)
ADD_OPENMP_TEST(test-socket SOURCE test_socket.f90)
ADD_OPENMP_TEST(test-send-recv SOURCE test_send_recv.f90)
ADD_OPENMP_TEST(test-send-recv-string SOURCE test_send_recv_string.f90)
ADD_OPENMP_TEST(test-send-recv-message SOURCE test_send_recv_message.f90)
ADD_OPENMP_TEST(test-send-recv-multipart SOURCE test_send_recv_multipart.f90)
ADD_OPENMP_TEST(test-msg-get SOURCE test_msg_get.f90)
ADD_OPENMP_TEST(test-poll SOURCE test_poll.f90)
ADD_OPENMP_TEST(test-proxy-steerable SOURCE test_proxy_steerable.f90)
ADD_OPENMP_TEST(test-context-deprecated SOURCE test_context_deprecated.f90)
ADD_OPENMP_TEST(test-send-recv-deprecated SOURCE test_send_recv_deprecated.f90)
