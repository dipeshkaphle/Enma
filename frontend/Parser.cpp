#include "Parser.hpp"
#include "Error.hpp"
#include "Expression.hpp"
#include "Statements.hpp"

#include <fmt/core.h>
#include <tl/to.hpp>

#include <algorithm>
#include <deque>
#include <iterator>
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
    // case TokenType::FOR:
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

expr_or_err Parser::parse_expr() { return expression(); }

Parser::stmt_or_err Parser::statement() {
  if (this->match({TokenType::DATA})) {
    return data_definition();
  }
  if (this->match({TokenType::IF})) {
    return if_statement();
  }
  if (this->match({TokenType::WHILE})) {
    return this->while_statement();
  }
  // if (this->match({TokenType::FOR})) {
  // return this->for_statement();
  // }
  if (this->match({TokenType::BREAK})) {
    return this->break_statement();
  }
  if (this->match({TokenType::CONTINUE})) {
    return this->continue_statement();
  }
  if (this->match({TokenType::PRINT})) {
    return this->print_statement(false);
  }
  if (this->match({TokenType::PRINTLN})) {
    return this->print_statement(true);
  }
  if (this->match({TokenType::RETURN})) {
    return this->return_statement();
  }
  if (this->match({TokenType::LEFT_BRACE})) {
    auto maybe_stmts = this->block();
    RETURN_IF_ERR(maybe_stmts);
    return std::make_unique<BlockStmt>(std::move(maybe_stmts.value()));
  }
  return this->expression_statement();
}
Parser::stmt_or_err Parser::break_statement() {
  if (this->loop_nesting_count > 0) {
    RETURN_IF_ERR(consume(TokenType::SEMICOLON, "Expected ; after a 'break'"));
    return std::make_unique<BreakStmt>();
  }
  return tl::make_unexpected<parse_error>(this->error(
      this->peek(),
      "Break statement is not inside any form of loop. This is not valid"));
}
Parser::stmt_or_err Parser::continue_statement() {
  if (this->loop_nesting_count > 0) {
    RETURN_IF_ERR(consume(TokenType::SEMICOLON, "Expected ; after a 'break'"));
    return std::make_unique<ContinueStmt>();
  }
  return tl::make_unexpected<parse_error>(this->error(
      this->peek(),
      "Continue statement is not inside any form of loop. This is not valid"));
}
Parser::stmt_or_err Parser::expression_statement() {
  auto expr = this->expression().and_then([&](auto &&exp) -> expr_or_err {
    return consume(TokenType::SEMICOLON, "Expect ; after expression")
        .and_then([&](auto &&) -> expr_or_err {
          return std::forward<decltype(exp)>(exp);
        });
  });
  return move(expr).and_then([&](expr_or_err &&exp) -> stmt_or_err {
    return std::make_unique<ExprStmt>(move(exp.value()));
  });
}
Parser::stmt_or_err Parser::print_statement(bool new_line) {

  auto expr = this->expression().and_then([&](auto &&exp) -> expr_or_err {
    return consume(TokenType::SEMICOLON, "Expect ; after expression")
        .and_then([&](auto &&) -> expr_or_err {
          return std::forward<decltype(exp)>(exp);
        });
  });
  return move(expr).and_then([&](expr_or_err &&exp) -> stmt_or_err {
    return std::make_unique<PrintStmt>(move(exp.value()), new_line);
  });
}
Parser::stmt_or_err Parser::return_statement() {
  auto stmt = std::make_unique<ReturnStmt>(this->previous());
  if (!check(TokenType::SEMICOLON)) {
    auto maybe_exp = this->expression();
    RETURN_IF_ERR(maybe_exp);
    stmt->value = make_optional(move(maybe_exp.value()));
  }
  return consume(TokenType::SEMICOLON, "Expect ; after return value")
      .map([&]([[maybe_unused]] auto &&_) { return move(stmt); });
}
Parser::stmt_or_err Parser::declaration() {
  if (match({TokenType::FN})) {
    return this->fn_declaration("Function");
  }
  if (match({TokenType::LET})) {
    return this->let_declaration();
  }
  return statement();
}
Parser::stmt_or_err Parser::fn_declaration(const std::string &type) {
  auto maybe_name = consume(TokenType::IDENTIFIER,
                            fmt::format("Expected {} name.", type).c_str());
  RETURN_IF_ERR(maybe_name);

  auto maybe_l_paren =
      consume(TokenType::LEFT_PAREN,
              fmt::format("Expect ( after {} name.", type).c_str());

  auto inside_parens = maybe_l_paren.and_then(
      [&]([[maybe_unused]] auto &_p)
          -> tl::expected<std::pair<vector<Token>, vector<Token>>,
                          parse_error> {
        // parses params
        vector<Token> params;
        vector<Token> param_types;
        if (!check(TokenType::RIGHT_PAREN)) {
          do {
            if (params.size() >= 255) {
              return tl::make_unexpected<parse_error>(
                  this->error(peek(), "Cant have more than 255 arguments"));
            }
            auto maybe_new_identifier =
                consume(TokenType::IDENTIFIER, "Expected parameter name");
            RETURN_IF_ERR(maybe_new_identifier);

            auto maybe_colon = consume(TokenType::COLON, "Expected :");
            RETURN_IF_ERR(maybe_colon);

            auto maybe_type =
                consume(TokenType::IDENTIFIER, "Expected type annotation");
            RETURN_IF_ERR(maybe_type);

            params.push_back(std::move(maybe_new_identifier.value()));
            param_types.push_back(std::move(maybe_type.value()));
          } while (match({TokenType::COMMA}));
        }
        auto maybe_rparen = consume(TokenType::RIGHT_PAREN, "Expected )");
        RETURN_IF_ERR(maybe_rparen);

        return std::pair{params, param_types};
      });

  RETURN_IF_ERR(inside_parens);

  auto [parameters, types] = std::move(inside_parens.value());

  auto body =
      consume(TokenType::LEFT_BRACE,
              fmt::format("Expect {} before {} body", '{', type).c_str())
          .and_then([&]([[maybe_unused]] const auto &_)
                        -> tl::expected<vector<stmt_ptr>, parse_error> {
            return block();
          });
  RETURN_IF_ERR(body);

  auto types_strings =
      types | std::views::transform([](auto &tok) { return tok.lexeme; }) |
      tl::to<std::vector<std::string>>();

  return std::make_unique<FnStmt>(maybe_name.value(), parameters, types_strings,
                                  std::move(body.value()));
}
Parser::stmt_or_err Parser::let_declaration() {
  /*
   * Wont allow empty declaration
   * let x; is not allowed
   * It must be let x = 2; (something like this)
   */
  auto maybe_id = consume(TokenType::IDENTIFIER,
                          "Expected identifier after let declaration");
  RETURN_IF_ERR((maybe_id));

  auto maybe_colon =
      consume(TokenType::COLON, "Expected colon after identifier");

  RETURN_IF_ERR(maybe_colon);

  auto maybe_type =
      consume(TokenType::IDENTIFIER, "Expected Type in let declartion");

  RETURN_IF_ERR(maybe_type);

  auto maybe_equal_to = consume(
      TokenType::EQUAL,
      "Cannot have empty declartion. It must be of form let <id> = <expr>;");
  RETURN_IF_ERR(maybe_equal_to);

  auto initializer_expr = this->expression();
  RETURN_IF_ERR(initializer_expr);

  auto maybe_semicolon = consume(TokenType::SEMICOLON,
                                 "Expected ; after the end of let declaration");
  RETURN_IF_ERR(maybe_semicolon);
  return std::make_unique<LetStmt>(maybe_id.value(), maybe_type.value(),
                                   std::move(initializer_expr.value()));
}
Parser::stmt_or_err Parser::if_statement() {
  auto maybe_leftparen = consume(TokenType::LEFT_PAREN, "Expect ( after if");
  RETURN_IF_ERR(maybe_leftparen);

  auto maybe_cond = this->expression();
  RETURN_IF_ERR(maybe_cond);

  auto maybe_right_paren =
      consume(TokenType::RIGHT_PAREN, "Expect ) after if condition");
  RETURN_IF_ERR(maybe_right_paren);

  auto maybe_then_branch = this->statement();
  RETURN_IF_ERR(maybe_then_branch);
  if (match({TokenType::ELSE})) {
    auto maybe_else_branch = this->statement();
    RETURN_IF_ERR(maybe_else_branch);
    return std::make_unique<IfStmt>(std::move(maybe_cond.value()),
                                    std::move(maybe_then_branch.value()),
                                    std::move(maybe_else_branch.value()));
  }
  return std::make_unique<IfStmt>(std::move(maybe_cond.value()),
                                  std::move(maybe_then_branch.value()));
}
Parser::stmt_or_err Parser::while_statement() {
  // in a loop
  this->loop_nesting_count++;
  auto maybe_leftparen = consume(TokenType::LEFT_PAREN, "Expect ( after while");
  RETURN_IF_ERR(maybe_leftparen);

  auto maybe_cond = this->expression();
  RETURN_IF_ERR(maybe_cond);
  auto maybe_right_paren =
      consume(TokenType::RIGHT_PAREN, "Expect ) after while condition");
  RETURN_IF_ERR(maybe_right_paren);
  auto maybe_body = this->statement();

  // going out of loop
  this->loop_nesting_count--;
  RETURN_IF_ERR(maybe_body);
  return std::make_unique<WhileStmt>(std::move(maybe_cond.value()),
                                     std::move(maybe_body.value()));
}

Parser::stmt_or_err Parser::data_definition() {
  return this
      ->consume(TokenType::IDENTIFIER,
                "Expected struct name after data keyword")
      .and_then([&](Token &&struct_name) -> stmt_or_err {
        auto eq = consume(TokenType::EQUAL, "Expected = after struct name");
        RETURN_IF_ERR(eq);

        auto lcurly = consume(TokenType::LEFT_BRACE,
                              "Expect { after = in data definition");
        RETURN_IF_ERR(lcurly);

        vector<Token> names;
        vector<Token> types;

        while (!check(TokenType::RIGHT_BRACE) && !this->is_at_end()) {

          auto maybe_name_and_type =
              consume(TokenType::IDENTIFIER, "field name expected")
                  .and_then([&](auto &&name) {
                    return consume(TokenType::COLON,
                                   "Expect : after name of the field")
                        .and_then([&](auto &&) {
                          return consume(TokenType::IDENTIFIER,
                                         "Expected type name")
                              .and_then(
                                  [&](auto &&type)
                                      -> tl::expected<std::pair<Token, Token>,
                                                      parse_error> {
                                    return std::pair<Token, Token>{name, type};
                                  });
                        });
                  });

          RETURN_IF_ERR(maybe_name_and_type);
          auto [name, type] = maybe_name_and_type.value();
          names.push_back(name);
          types.push_back(type);
        }
        return std::make_unique<DataDeclStmt>(struct_name, names, types);
      });
}
// Parser::stmt_or_err Parser::for_statement() {
// //
// }

tl::expected<std::vector<std::unique_ptr<Stmt>>, Parser::parse_error>
Parser::block() {
  std::deque<std::unique_ptr<Stmt>> stmts;
  while (!check(TokenType::RIGHT_BRACE) && !this->is_at_end()) {
    auto maybe_declr = this->declaration();
    RETURN_IF_ERR(maybe_declr);
    stmts.emplace_back(std::move(maybe_declr.value()));
  }
  return consume(TokenType::RIGHT_BRACE, "Expect } after block")
      .and_then(
          [&](const auto &) -> tl::expected<std::vector<std::unique_ptr<Stmt>>,
                                            Parser::parse_error> {
            return std::vector<std::unique_ptr<Stmt>>(
                make_move_iterator(stmts.begin()),
                make_move_iterator(stmts.end()));
          });
}

std::vector<Parser::stmt_or_err> Parser::parse() {
  vector<Parser::stmt_or_err> maybe_statements;
  while (!is_at_end()) {
    auto stmt = this->declaration();
    if (!stmt.has_value()) {
      this->synchronize();
    }
    maybe_statements.emplace_back(std::move(stmt));
  }
  return maybe_statements;
}
