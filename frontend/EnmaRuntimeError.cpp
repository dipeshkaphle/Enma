#include "EnmaRuntimeError.hpp"

EnmaRuntimeError::EnmaRuntimeError(Token tok, const char *err_msg)
    : runtime_error(err_msg), token(std::move(tok)) {}
