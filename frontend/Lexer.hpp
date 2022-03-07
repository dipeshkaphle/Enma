#pragma once

#pragma once

#include <algorithm>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "Token.hpp"
#include "TokenTypes.hpp"

using namespace std;

class Lexer {
  string source;
  vector<Token> tokens;
  size_t start = 0;
  size_t cur = 0;
  size_t cur_line = 1;
  using tok = TokenType;
  static const unordered_map<string, TokenType> keywords;
  /*
   * tells us if we're still in bounds or not
   */
  bool is_at_end() { return cur >= source.size(); }

  /*
   * consumes a character
   */
  char advance() { return source[this->cur++]; }

  /*
   * sees the current character, doesnt consume
   */
  char peek();

  /*
   * one step lookahead
   */
  char peek_next();

  /*
   * consumes if the current is a match for expected character
   */
  bool match(char expected);

  /*
   * appends a found token to tokens list
   */
  void add_token(TokenType type,
                 const Token::literal_type &literal = std::monostate());

  [[nodiscard]] std::string parse_with_escapes(string_view s) const;

  /*
   * parses a string literal
   */
  void get_string(char closing = '"');

  void get_char();

  /*
   * parses a number literal
   */
  void get_number();

  void get_identifier();

  void multiline_comment();

public:
  explicit Lexer(string source) : source(std::move(source)) {}

  /*
   * scans all the tokens
   */
  [[nodiscard]] vector<Token> scan_tokens();

  /*
   * consumes a token and returns back to scan_tokens
   *
   */
  void scan_token();
};
