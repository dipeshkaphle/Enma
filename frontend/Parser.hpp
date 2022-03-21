#pragma once

#include <memory>
#include <stdexcept>
#include <vector>

#include <tl/expected.hpp>

#include "Token.hpp"
namespace expr {
class Expr;
}

struct InfixParselet;
struct PrefixParselet;

class Parser {
public:
  struct parse_error : public std::runtime_error {
    explicit parse_error(const char *err_msg) : runtime_error(err_msg) {}
  };

  using expr_or_err = tl::expected<std::unique_ptr<expr::Expr>, parse_error>;
  friend struct PrefixParselet;
  friend struct InfixParselet;
  friend struct GroupParselet;
  // static std::unordered_map<TokenType, int> prefix_binding_power;
  // static std::unordered_map<TokenType, std::pair<int, int>>
  // infix_binding_power;

  static const inline std::vector<TokenType> operators_tokens{
      TokenType::LEFT_PAREN, TokenType::DOT,
      TokenType::PLUS,       TokenType::MINUS,
      TokenType::SLASH,      TokenType::STAR,
      TokenType::AND,        TokenType::OR,
      TokenType::BANG,       TokenType::BANG_EQUAL,
      TokenType::EQUAL,      TokenType::EQUAL_EQUAL,
      TokenType::GREATER,    TokenType::GREATER_EQUAL,
      TokenType::LESS,       TokenType::LESS_EQUAL,

  };

  static const inline std::vector<TokenType> name_or_literals_tokens{
      TokenType::IDENTIFIER, TokenType::STRING, TokenType::CHAR, TokenType::INT,
      TokenType::DOUBLE,     TokenType::TRUE,   TokenType::FALSE

  };

  static const inline std::unordered_map<TokenType, int> prefix_binding_power{
      {TokenType::LEFT_PAREN, 0},
      {TokenType::BANG, 15},
      {TokenType::PLUS, 16},
      {TokenType::MINUS, 16},

  };

  // We model associativity with diff left and right binding power values
  // If lbp > rbp, means the operator is right associative, else left
  // associative
  static const inline std::unordered_map<TokenType, std::pair<int, int>>
      infix_binding_power{
          {TokenType::EQUAL, {2, 1}},
          {TokenType::OR, {3, 4}},
          {TokenType::AND, {5, 6}},
          {TokenType::EQUAL_EQUAL, {7, 8}},
          {TokenType::BANG_EQUAL, {7, 8}},
          {TokenType::LESS, {9, 10}},
          {TokenType::LESS_EQUAL, {9, 10}},
          {TokenType::GREATER, {9, 10}},
          {TokenType::GREATER_EQUAL, {9, 10}},
          {TokenType::PLUS, {11, 12}},
          {TokenType::MINUS, {11, 12}},
          {TokenType::SLASH, {13, 14}},
          {TokenType::STAR, {13, 14}},
          {TokenType::DOT, {18, 17}},
          {TokenType::LEFT_PAREN, {19, 20}},
      };

  // static const std::vector<TokenType> operators_tokens;
  // static const std::vector<TokenType> name_or_literals_tokens;

private:
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

  expr_or_err finish_call(std::unique_ptr<expr::Expr> callee);

  expr_or_err expression(int binding_power = 0);

  expr_or_err prefix_expression();

public:
  explicit Parser(std::vector<Token> _toks);
  expr_or_err parse();
};
struct PrefixParselet {
  virtual Parser::expr_or_err parse(Parser &parser, Token &tok);
};

struct InfixParselet {
  virtual Parser::expr_or_err parse(Parser &parser, Token &tok,
                                    std::unique_ptr<expr::Expr> left);
};

struct GroupParselet : public PrefixParselet {
  virtual Parser::expr_or_err parse(Parser &parser, Token &tok) override;
};
