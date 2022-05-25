#pragma once

#pragma once

#include <algorithm>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "Token.hpp"
#include "TokenTypes.hpp"

using std::string;
using std::string_view;
using std::unordered_map;
using std::vector;

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
                 const literal_type &literal = std::monostate());

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
  static inline std::unordered_map<char, char> valid_escapes{
      {'\'', 0x27}, {'"', 0x22}, {'?', 0x3f}, {'\\', 0x5c},
      {'a', 0x07},  {'b', 0x08}, {'f', 0x0c}, {'n', 0x0a},
      {'r', 0x0d},  {'t', 0x09}, {'v', 0x0b}};
  explicit Lexer(string source);

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
