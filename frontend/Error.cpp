#include "Error.hpp"

#include <fmt/core.h>
#include <fmt/ostream.h>

#include <iostream>

using namespace std::string_literals;

bool Error::hadError = false;

void Error::error(int line, string_view message) {
  Error::report(line, "", message);
}

void Error::report(int line, string_view where, string_view message) {
  fmt::print(std::cerr, "[line {}] Error {} : {}\n", line, where, message);
  Error::hadError = true;
}

void Error::error(const Token &tok, const char *message) {
  if (tok.type == TokenType::ENDOFFILE) {
    report(tok.line, " at end", message);
  } else {
    report(tok.line, " at '"s + tok.lexeme + "'"s, message);
  }
}
