#pragma once

#include "Token.hpp"
#include <stdexcept>

struct EnmaRuntimeError : public std::runtime_error {
  Token token;
  explicit EnmaRuntimeError(Token tok, const char *err_msg);
};
