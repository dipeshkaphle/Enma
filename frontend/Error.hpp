#pragma once

#include "Token.hpp"

#include <string_view>

using std::string_view;

class Error {
public:
  static bool hadError;
  static void error(int line, string_view message);
  static void report(int line, string_view where, string_view message);
  static void error(const Token &tok, const char *message);
};
