#pragma once

#include <functional>
#include <string>
#include <unordered_map>

// using namespace std;

enum class TokenType {
  // Single-character tokens.
  LEFT_PAREN = 0,
  RIGHT_PAREN,
  LEFT_BRACE,
  RIGHT_BRACE,
  COMMA,
  DOT,
  MINUS,
  PLUS,
  SEMICOLON,
  COLON,
  SLASH,
  STAR,
  // One or two character tokens.
  BANG,
  BANG_EQUAL,
  EQUAL,
  EQUAL_EQUAL,
  GREATER,
  GREATER_EQUAL,
  LESS,
  LESS_EQUAL,
  // Literals.
  IDENTIFIER,
  STRING,
  CHAR,
  INT,
  DOUBLE,
  // Keywords.
  AND,
  DATA,
  ELSE,
  FALSE,
  FN,
  // FOR,
  IF,
  NIL,
  OR,
  PRINT,
  PRINTLN,
  RETURN,
  TRUE,
  LET,
  WHILE,
  BREAK,
  CONTINUE,
  ENDOFFILE
};

// Thank you vim
inline std::string type_to_string(TokenType type) {
  static std::unordered_map<TokenType, std::string> to_str{
      {TokenType::LEFT_PAREN, "LEFT_PAREN"},
      {TokenType::RIGHT_PAREN, "RIGHT_PAREN"},
      {TokenType::LEFT_BRACE, "LEFT_BRACE"},
      {TokenType::RIGHT_BRACE, "RIGHT_BRACE"},
      {TokenType::COMMA, "COMMA"},
      {TokenType::DOT, "DOT"},
      {TokenType::MINUS, "MINUS"},
      {TokenType::PLUS, "PLUS"},
      {TokenType::SEMICOLON, "SEMICOLON"},
      {TokenType::COLON, "COLON"},
      {TokenType::SLASH, "SLASH"},
      {TokenType::STAR, "STAR"},
      {TokenType::BANG, "BANG"},
      {TokenType::BANG_EQUAL, "BANG_EQUAL"},
      {TokenType::EQUAL, "EQUAL"},
      {TokenType::EQUAL_EQUAL, "EQUAL_EQUAL"},
      {TokenType::GREATER, "GREATER"},
      {TokenType::GREATER_EQUAL, "GREATER_EQUAL"},
      {TokenType::LESS, "LESS"},
      {TokenType::LESS_EQUAL, "LESS_EQUAL"},
      {TokenType::IDENTIFIER, "IDENTIFIER"},
      {TokenType::STRING, "STRING"},
      {TokenType::CHAR, "CHAR"},
      {TokenType::INT, "INT"},
      {TokenType::DOUBLE, "DOUBLE"},
      {TokenType::AND, "AND"},
      {TokenType::DATA, "DATA"},
      {TokenType::ELSE, "ELSE"},
      {TokenType::FALSE, "FALSE"},
      {TokenType::FN, "FN"},
      // {TokenType::FOR, "FOR"},
      {TokenType::IF, "IF"},
      {TokenType::NIL, "NIL"},
      {TokenType::OR, "OR"},
      {TokenType::PRINT, "PRINT"},
      {TokenType::PRINTLN, "PRINTLN"},
      {TokenType::RETURN, "RETURN"},
      {TokenType::TRUE, "TRUE"},
      {TokenType::LET, "LET"},
      {TokenType::WHILE, "WHILE"},
      {TokenType::BREAK, "BREAK"},
      {TokenType::CONTINUE, "CONTINUE"},
      {TokenType::ENDOFFILE, "EOF"}};
  return to_str[type];
}
