add_custom_target(
  haskell-backend ALL
  COMMAND stack build && cp `stack exec -- which backend-exe`
          ${CMAKE_BINARY_DIR}/bin/enma-backend-haskell
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
