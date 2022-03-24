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

expr_or_err Parser::finish_call(std::unique_ptr<Expr> callee) {
  // by limiting the array to 255 size, we limit the maximum number of arguments
  // that can be passed to a function to 255
  std::array<std::unique_ptr<Expr>, 255> arguments;
  auto it = arguments.begin();
  if (!check(TokenType::RIGHT_PAREN)) {
    do {
      if (it == arguments.end()) {
        return tl::make_unexpected<parse_error>(
            this->error(peek(), "Cant have more than 255 arguments"));
      }
      auto expr = this->expression();
      RETURN_IF_ERR(expr);
      *it = move(expr.value());
      std::advance(it, 1);
    } while (match({TokenType::COMMA}));
  }
  auto paren = consume(TokenType::RIGHT_PAREN, "Expect ')' after arguments");
  RETURN_IF_ERR(paren);
  vector<std::unique_ptr<Expr>> vec(make_move_iterator(arguments.begin()),
                                    make_move_iterator(it));
  return make_unique<CallExpr>(move(callee), paren.value(), move(vec));
}

// TODO: Handle function calls, basically the LEFT PAREN OPERATOR for infix is
// to be handled properly
expr_or_err Parser::expression(int binding_power) {

  return prefix_expression().and_then([&](std::unique_ptr<Expr> &&lhs)
                                          -> expr_or_err {
    while (true) {

      if (any_of_at_peek(Parser::operators_tokens)) {
        auto op = peek();
        if (infix_binding_power.contains(op.type)) {

          const auto [lbp, rbp] = infix_binding_power.at(op.type);
          if (lbp < binding_power) {
            break;
          }

          advance();
          expr_or_err maybe_final_expr =
              tl::unexpected<parse_error>(parse_error(
                  "Something is wrong in the parser if you see this as error"));
          if (op.type == TokenType::LEFT_PAREN) {
            // CALL EXPR
            maybe_final_expr = finish_call(std::move(lhs));
          } else if (op.type == TokenType::IF) {
            // conditional expr  ,
            // <expr> if <cond> else <expr>

            maybe_final_expr = expression().and_then([&](auto &&condition) {
              //
              return consume(TokenType::ELSE, "Expected else after condition "
                                              "in conditional expression")
                  .and_then([&](auto &&) {
                    return expression().map([&](auto &&else_expr) {
                      return std::make_unique<ConditionalExpr>(
                          std::forward<decltype(condition)>(condition),
                          std::move(lhs),
                          std::forward<decltype(else_expr)>(else_expr));
                    });
                  });
            });
          } else {
            if (op.type == TokenType::EQUAL) {
              Expr *lhs_raw = lhs.get();
              auto *lhs_as_var_expr = dynamic_cast<VarExpr *>(lhs_raw);
              if (lhs_as_var_expr == nullptr) {
                return tl::unexpected<parse_error>(
                    parse_error("Invalid assignment, is not an lvalue"));
              }
            }
            maybe_final_expr = expression(rbp).and_then(
                [&](std::unique_ptr<Expr> &&rhs) -> expr_or_err {
                  return std::make_unique<BinaryExpr>(
                      std::move(op), std::move(lhs), std::move(rhs));
                });
          }
          RETURN_IF_ERR(maybe_final_expr);
          lhs = std::move(maybe_final_expr.value());
          continue;
        }
      }
      break;
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

expr_or_err Parser::parse() { return expression(); }
// Parser::expr_or_err PrefixParselet::parse(Parser &parser, Token &tok) {
// if (Parser::prefix_binding_power.contains(tok.type)) {
// Token op = parser.advance();
// auto rbp = Parser::prefix_binding_power.at(op.type);
// auto maybe_rhs = parser.expression(rbp);
// RETURN_IF_ERR(maybe_rhs);
// return std::make_unique<PrefixExpr>(op, std::move(maybe_rhs.value()));
// }
// return tl::unexpected<Parser::parse_error>(
// Parser::error(parser.peek(), "Unexpected token"));
// }

// Parser::expr_or_err InfixParselet::parse(Parser &parser, Token &tok,
// std::unique_ptr<expr::Expr> left) {
// //

// const auto [lbp, rbp] = Parser::infix_binding_power.at(tok.type);

// return parser.expression(rbp).and_then(
// [&](std::unique_ptr<Expr> &&rhs) -> expr_or_err {
// return std::make_unique<BinaryExpr>(std::move(tok), std::move(left),
// std::move(rhs));
// });
// }
