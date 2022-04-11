#include "Token.hpp"
#include "Common.hpp"

#include <fmt/format.h>

Token::Token(TokenType type, std::string lexeme, literal_type literal, int line)
    : type(type), lexeme(move(lexeme)), literal(move(literal)), line(line) {}

std::string Token::literal_to_string() const {
  using namespace std::string_literals;
  return ::literal_to_string(literal);
}

std::string Token::to_string() const {
  return fmt::format("Type: {}, Lexeme: {}, Literal: {}, Line : {}",
                     type_to_string(type), lexeme, literal_to_string(), line);
}
