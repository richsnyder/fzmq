# Locate all of the required dependencies.

FIND_PACKAGE(Pandoc REQUIRED)

# Define functions.

FUNCTION(ADD_PAGES)

  SET(links ${CMAKE_CURRENT_SOURCE_DIR}/links.md)
  CONFIGURE_FILE(template.in template.txt @ONLY)

  FOREACH(page ${ARGN})
    SET(cfg ${CMAKE_CURRENT_SOURCE_DIR}/${page}.md)
    SET(src ${CMAKE_CURRENT_BINARY_DIR}/${page}.md)
    SET(dst ${CMAKE_CURRENT_BINARY_DIR}/${page}.html)
    CONFIGURE_FILE(${cfg} ${src} @ONLY)
    ADD_CUSTOM_COMMAND(
      OUTPUT ${dst}
      MAIN_DEPENDENCY ${src}
      COMMAND ${Pandoc_EXECUTABLE}
      ARGS -s -t html5 --template=template.txt -o ${dst} ${src} ${links}
    )
    LIST(APPEND pages ${dst})
  ENDFOREACH()

  SET(src ${CMAKE_CURRENT_SOURCE_DIR}/style.css)
  SET(dst ${CMAKE_CURRENT_BINARY_DIR}/style.css)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${dst}
    MAIN_DEPENDENCY ${src}
    COMMAND ${CMAKE_COMMAND}
    ARGS -E copy ${src} ${dst}
  )
  LIST(APPEND pages ${dst})

  ADD_CUSTOM_TARGET(doc ALL DEPENDS ${pages})
  INSTALL(FILES ${pages} DESTINATION doc/html)

ENDFUNCTION()

# Add pages to the documentation target.

ADD_PAGES(index fzmq zmq_bind zmq_close zmq_connect zmq_ctx_destroy zmq_ctx_get
zmq_ctx_new zmq_ctx_set zmq_ctx_shutdown zmq_ctx_term zmq_curve
zmq_curve_keypair zmq_disconnect zmq_errno zmq_getsockopt zmq_gssapi zmq_has
zmq_init zmq_inproc zmq_ipc zmq_msg_close zmq_msg_copy zmq_msg_data zmq_msg_get
zmq_msg_gets zmq_msg_init zmq_msg_init_data zmq_msg_init_size zmq_msg_more
zmq_msg_move zmq_msg_recv zmq_msg_send zmq_msg_set zmq_msg_size zmq_null
zmq_pgm zmq_plain zmq_poll zmq_proxy zmq_proxy_steerable zmq_recv
zmq_recvmsg zmq_send zmq_send_const zmq_sendmsg zmq_setsockopt
zmq_socket zmq_socket_monitor zmq_strerror zmq_tcp zmq_tipc zmq_unbind
zmq_version zmq_z85_decode zmq_z85_encode)
