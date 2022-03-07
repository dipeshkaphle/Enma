#include "Token.hpp"

#include <fmt/format.h>

// helper type for the visitor #4
template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
// explicit deduction guide (not needed as of C++20)
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

std::string Token::literal_to_string() const {
  using namespace std::string_literals;
  return std::visit(
      overloaded{[](int64_t x) { return std::to_string(x); },
                 [](const double &x) { return std::to_string(x); },
                 [](const char &c) { return std::string(1, c); },
                 [](const std::string &s) { return s; },
                 [](const bool &b) { return b ? "true"s : "false"s; },
                 [](auto) { return "_"s; }},
      literal);
}

Token::Token(TokenType type, std::string lexeme, literal_type literal, int line)
    : type(type), lexeme(move(lexeme)), literal(move(literal)), line(line) {}
std::string Token::to_string() const {
  return fmt::format("Type: {}, Lexeme: {}, Literal: {}, Line : {}",
                     type_to_string(type), lexeme, literal_to_string(), line);
}
