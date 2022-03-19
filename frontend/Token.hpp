#pragma once

#include <any>
#include <string>
#include <variant>

#include "Common.hpp"
#include "TokenTypes.hpp"

class Token {
private:
  [[nodiscard]] std::string literal_to_string() const;

public:
  using literal_type = literal_type;

  TokenType type;
  std::string lexeme;
  literal_type literal;
  int line;
  Token(TokenType type, std::string lexeme, literal_type literal, int line);

  [[nodiscard]] std::string to_string() const;
};
