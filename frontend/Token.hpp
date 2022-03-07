#pragma once

#include <any>
#include <string>
#include <variant>

#include "TokenTypes.hpp"

class Token {
private:
  [[nodiscard]] std::string literal_to_string() const;

public:
  // Our language has 5 literal types
  using literal_type =
      std::variant<std::monostate, int64_t, double, bool, char, std::string>;
  TokenType type;
  std::string lexeme;
  literal_type literal;
  int line;
  Token(TokenType type, std::string lexeme, literal_type literal, int line);

  [[nodiscard]] std::string to_string() const;
};
