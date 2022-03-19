#pragma once

#include <memory>
#include <stdexcept>
#include <vector>

#include <tl/expected.hpp>

#include "Token.hpp"
namespace expr {
class Expr;
}

class Parser {
public:
  struct parse_error : public std::runtime_error {
    explicit parse_error(const char *err_msg) : runtime_error(err_msg) {}
  };

  using expr_or_err = tl::expected<std::unique_ptr<expr::Expr>, parse_error>;

private:
  static std::unordered_map<TokenType, int> prefix_binding_power;
  static std::unordered_map<TokenType, std::pair<int, int>> infix_binding_power;

  static const std::vector<TokenType> operators_tokens;
  static const std::vector<TokenType> name_or_literals_tokens;

  std::vector<Token> tokens;
  int cur{0};
  int loop_nesting_count{0};

  bool check(TokenType tk);

  Token &advance();

  bool is_at_end();

  Token &peek();

  Token &previous();

  bool match(const std::vector<TokenType> &tok);

  bool any_of_at_peek(const std::vector<TokenType> &tok);

  void synchronize();

  tl::expected<Token, parse_error> consume(TokenType tok, const char *err_msg);

  static parse_error error(const Token &tok, const char *err_msg);

  expr_or_err expression(int binding_power);

  expr_or_err prefix_expression();

public:
  explicit Parser(std::vector<Token> _toks);
};
