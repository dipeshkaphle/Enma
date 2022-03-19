#include "Parser.hpp"
#include "Error.hpp"
#include "Expression.hpp"

#include <fmt/core.h>

#include <algorithm>
#include <ranges>
#include <type_traits>

#define UNIMPLEMENTED static_assert(false, "Unimplemented Code")

#define RETURN_IF_ERR(expr)                                                    \
  if (!((expr).has_value()))                                                   \
    return tl::unexpected((expr).error());

using namespace expr;

using expr_or_err = Parser::expr_or_err;

const std::vector<TokenType> Parser::operators_tokens{
    TokenType::LEFT_PAREN, TokenType::DOT,
    TokenType::PLUS,       TokenType::MINUS,
    TokenType::SLASH,      TokenType::STAR,
    TokenType::AND,        TokenType::OR,
    TokenType::BANG,       TokenType::BANG_EQUAL,
    TokenType::EQUAL,      TokenType::EQUAL_EQUAL,
    TokenType::GREATER,    TokenType::GREATER_EQUAL,
    TokenType::LESS,       TokenType::LESS_EQUAL,

};

static const std::vector<TokenType> name_or_literals_tokens{
    TokenType::IDENTIFIER, TokenType::STRING, TokenType::CHAR, TokenType::INT,
    TokenType::DOUBLE,     TokenType::TRUE,   TokenType::FALSE

};

std::unordered_map<TokenType, int> Parser::prefix_binding_power{
    {TokenType::LEFT_PAREN, 0},
    {TokenType::BANG, 15},
    {TokenType::PLUS, 16},
    {TokenType::MINUS, 16},

};
std::unordered_map<TokenType, std::pair<int, int>> Parser::infix_binding_power{
    {TokenType::LEFT_PAREN, {0, 0}},  {TokenType::EQUAL, {2, 1}},
    {TokenType::OR, {3, 4}},          {TokenType::AND, {5, 6}},
    {TokenType::EQUAL_EQUAL, {7, 8}}, {TokenType::BANG_EQUAL, {7, 8}},
    {TokenType::LESS, {9, 10}},       {TokenType::LESS_EQUAL, {9, 10}},
    {TokenType::GREATER, {9, 10}},    {TokenType::GREATER_EQUAL, {9, 10}},
    {TokenType::PLUS, {11, 12}},      {TokenType::MINUS, {11, 12}},
    {TokenType::SLASH, {13, 14}},     {TokenType::STAR, {13, 14}},
    {TokenType::DOT, {18, 17}},       {TokenType::COMMA, {20, 19}},
};

Parser::Parser(std::vector<Token> _toks) : tokens(move(_toks)), cur(0) {}

bool Parser::is_at_end() { return peek().type == TokenType::ENDOFFILE; }

Token &Parser::peek() { return tokens[cur]; }

Token &Parser::previous() { return tokens[cur - 1]; }

bool Parser::check(TokenType tk) {
  if (is_at_end()) {
    return false;
  }
  return peek().type == tk;
}

Token &Parser::advance() {
  if (!is_at_end()) {
    this->cur++;
  }
  return previous();
}

bool Parser::match(const std::vector<TokenType> &tok) {
  if (any_of_at_peek(tok)) {
    advance();
    return true;
  }
  return false;
}

bool Parser::any_of_at_peek(const std::vector<TokenType> &tok) {
  return std::ranges::any_of(tok, [&](auto &tk) { return this->check(tk); });
}

void Parser::synchronize() {
  advance();
  while (!this->is_at_end()) {
    if (previous().type == TokenType::SEMICOLON) {
      return;
    }
    switch (this->peek().type) {
    case TokenType::DATA:
    case TokenType::FN:
    case TokenType::LET:
    case TokenType::FOR:
    case TokenType::IF:
    case TokenType::WHILE:
    case TokenType::PRINT:
    case TokenType::RETURN:
      return;
    default:
      advance();
    }
  }
}

tl::expected<Token, Parser::parse_error> Parser::consume(TokenType tok,
                                                         const char *err_msg) {
  if (check(tok)) {
    return this->advance();
  }
  return tl::unexpected<Parser::parse_error>(Parser::error(peek(), err_msg));
}

Parser::parse_error Parser::error(const Token &tok, const char *err_msg) {
  Error::error(tok, err_msg);
  return parse_error(err_msg);
}

expr_or_err Parser::expression(int binding_power) {

  return prefix_expression().and_then(
      [&](std::unique_ptr<Expr> &&lhs) -> expr_or_err {
        while (true) {

          if (any_of_at_peek(Parser::operators_tokens)) {
            auto op = peek();
            if (infix_binding_power.contains(op.type)) {

              const auto [lbp, rbp] = infix_binding_power.at(op.type);
              if (lbp < binding_power) {
                break;
              }

              advance();
              auto maybe_final_expr = expression(rbp).and_then(
                  [&](std::unique_ptr<Expr> &&rhs) -> expr_or_err {
                    return std::make_unique<BinaryExpr>(
                        std::move(op), std::move(lhs), std::move(rhs));
                  });
              RETURN_IF_ERR(maybe_final_expr);
              lhs = std::move(maybe_final_expr.value());
              continue;
            }
            break;
          }
        }
        return lhs;
      });
}

expr_or_err Parser::prefix_expression() {

  if (any_of_at_peek(Parser::operators_tokens)) {
    if (prefix_binding_power.contains(peek().type)) {
      Token op = advance();
      // binding_power of LEFT_PAREN is 0
      auto rbp = prefix_binding_power.at(op.type);

      auto maybe_rhs = expression(rbp);
      RETURN_IF_ERR(maybe_rhs);

      if (op.type == TokenType::LEFT_PAREN) {
        return consume(TokenType::RIGHT_PAREN, "Expected Closing Parentheses")
            .map([&](auto &&) { return std::move(maybe_rhs.value()); });
      }
      return std::make_unique<PrefixExpr>(op, std::move(maybe_rhs.value()));
    }
    return tl::unexpected<parse_error>(
        Parser::error(peek(), "Unexpected token"));
  }

  if (match(Parser::name_or_literals_tokens)) {
    auto &prev_tok = previous();
    if (prev_tok.type == TokenType::IDENTIFIER) {
      return std::make_unique<VarExpr>(prev_tok);
    }
    return std::make_unique<LiteralExpr>(std::move(prev_tok.literal));
  }
  return tl::unexpected<parse_error>(Parser::error(peek(), "Unexpected token"));
}
