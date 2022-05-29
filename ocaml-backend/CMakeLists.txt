add_custom_target(
  ocaml-backend ALL
  COMMAND
  dune build && (test -f "${CMAKE_BINARY_DIR}/bin/enma-backend-ocaml"  || ln
    -s `dune exec -- which enma-backend`
    ${CMAKE_BINARY_DIR}/bin/enma-backend-ocaml)
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
