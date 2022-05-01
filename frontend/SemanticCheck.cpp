#include "SemanticCheck.hpp"
#include "Common.hpp"
#include "Statements.hpp"

#include <fmt/format.h>
#include <tl/optional.hpp>
#include <tl/to.hpp>

#include <algorithm>
#include <iterator>
#include <ranges>

// TODO: DOESNT SEEM TO WORK

#define IF_IS_TYPE(VAR, TYPE, EXP)                                             \
  if (auto *VAR = dynamic_cast<TYPE *>(EXP); VAR != nullptr)

SemanticChecker::SemanticChecker(std::vector<std::unique_ptr<Stmt>> stmts)
    : stmts(std::move(stmts)) {}
void SemanticChecker::add_to_symtable(const std::string &scope,
                                      symbol_type entry) {
  this->sym_table[scope].emplace_back(entry);
}

bool SemanticChecker::has_symbol(vector<vector<symbol_type>> &symbols,
                                 const std::string &symbol_name) {
  for (auto &v : symbols | std::ranges::views::reverse) {
    for (auto &sym : v) {

      if (symbol_name ==
          std::visit(overloaded{[](FnStmt *fn) { return fn->name.lexeme; },
                                [](LetStmt *lt) { return lt->name.lexeme; },
                                [](auto) { return std::string(""); }},
                     sym)) {
        return true;
      }
    }
  }
  return false;
}

bool SemanticChecker::is_primitive_type(const std::string &symbol) {
  return std::ranges::any_of(
      std::array{"int", "double", "string", "char", "bool", "void"},
      [&](const char *tp) { return tp == symbol; });
}

tl::optional<SemanticChecker::semantic_error>
SemanticChecker::traverse_scopes_and_check(
    expr::Expr *expr, std::vector<std::vector<symbol_type>> &symbols) {

  if (auto *assign_expr = dynamic_cast<expr::AssignExpr *>(expr);
      assign_expr != nullptr) {
    // Its an assignment expression
    // Check the rhs and then check if lhs is a valid symbol
    return traverse_scopes_and_check(assign_expr->value.get(), symbols)
        .or_else([&]() -> tl::optional<SemanticChecker::semantic_error> {
          if (!has_symbol(symbols, assign_expr->name.lexeme)) {
            return tl::make_optional<SemanticChecker::semantic_error>(
                SemanticChecker::semantic_error(
                    fmt::format("Undeclared symbol {} in line {}",
                                assign_expr->name.lexeme,
                                assign_expr->name.line)
                        .c_str()));
          }
          if (this->get_type(assign_expr->name.lexeme, symbols) !=
              this->get_expr_type(assign_expr->value.get(), symbols)) {
            return SemanticChecker::semantic_error(
                fmt::format("Type of LHS != Type of RHS in line {}",
                            assign_expr->name.line)
                    .c_str());
          }
          return {};
        });
  }
  if (auto *var_expr = dynamic_cast<expr::VarExpr *>(expr);
      var_expr != nullptr) {
    // VarExpr: Check if the variable is a valid symbol
    if (!has_symbol(symbols, var_expr->name.lexeme)) {
      return tl::make_optional<SemanticChecker::semantic_error>(
          SemanticChecker::semantic_error(
              fmt::format("Undeclared symbol {} in line {}",
                          var_expr->name.lexeme, var_expr->name.line)
                  .c_str()));
    }
  }

  if (auto *cond_expr = dynamic_cast<expr::ConditionalExpr *>(expr);
      cond_expr != nullptr) {
    // ConditionalExpr: Check the condition, then the blocks
    return traverse_scopes_and_check(cond_expr->cond.get(), symbols)
        .or_else([&]() {
          return traverse_scopes_and_check(cond_expr->then_expr.get(), symbols);
        })
        .or_else([&]() {
          return traverse_scopes_and_check(cond_expr->else_expr.get(), symbols);
        })
        .or_else([&]() -> tl::optional<SemanticChecker::semantic_error> {
          if (get_expr_type(cond_expr->else_expr.get(), symbols) !=
              get_expr_type(cond_expr->then_expr.get(), symbols)) {
            return SemanticChecker::semantic_error(
                fmt::format("Type of else branch != Type of then branch in "
                            "ConditionalExpr")
                    .c_str());
          }
          return {};
        });
  }

  if (auto *bin_expr = dynamic_cast<expr::BinaryExpr *>(expr);
      bin_expr != nullptr) {
    // BinaryExpr: Check lhs and rhs
    return traverse_scopes_and_check(bin_expr->left.get(), symbols)
        .or_else([&]() {
          return traverse_scopes_and_check(bin_expr->right.get(), symbols);
        })
        .or_else([&]() -> tl::optional<SemanticChecker::semantic_error> {
          if (get_expr_type(bin_expr->left.get(), symbols) !=
              get_expr_type(bin_expr->right.get(), symbols)) {
            return SemanticChecker::semantic_error(
                fmt::format("Type of lhs != Type of rhs in "
                            "BinaryExpr in line {}",
                            bin_expr->op.line)
                    .c_str());
          }
          return {};
        });
  }

  // TODO
  if (auto *attr_access = dynamic_cast<expr::AttrAccessExpr *>(expr);
      attr_access != nullptr) {
    if (auto *var = dynamic_cast<expr::VarExpr *>(attr_access);
        var != nullptr) {
    }
    // auto [type_name, remaining_scope] = get_type_of(attr_access->left,
    // std::span<std::vector<symbol_type>> symbols)
  }

  if (auto *prefix_expr = dynamic_cast<expr::PrefixExpr *>(expr);
      prefix_expr != nullptr) {
    // PrefixExpr: Check the expression
    return traverse_scopes_and_check(prefix_expr->right.get(), symbols)
        .or_else([&]() -> tl::optional<SemanticChecker::semantic_error> {
          auto type = get_expr_type(prefix_expr->right.get(), symbols);
          tl::optional<Type> INT = "int";
          tl::optional<Type> DOUBLE = "double";
          tl::optional<Type> BOOL = "bool";
          if (!(type == INT || type == DOUBLE || type == BOOL)) {
            return SemanticChecker::semantic_error(
                fmt::format("Unary expr should be one of int, double or bool "
                            "but its not. Line {}",
                            prefix_expr->op.line)
                    .c_str());
          }
          return {};
        });
  }

  if (auto *call_expr = dynamic_cast<expr::CallExpr *>(expr);
      call_expr != nullptr) {
    // CallExpr: Check that its a valid symbol and check all the params
    if (auto *var = dynamic_cast<expr::VarExpr *>(call_expr->callee.get());
        var != nullptr) {
      if (!has_symbol(symbols, var->name.lexeme)) {
        return tl::make_optional<SemanticChecker::semantic_error>(
            SemanticChecker::semantic_error(
                fmt::format("Undeclared symbol {} in line {}", var->name.lexeme,
                            var->name.line)
                    .c_str()));
      }
    }

    for (auto &exp : call_expr->arguments) {
      auto res = traverse_scopes_and_check(exp.get(), symbols);
      if (res.has_value()) {
        return res;
      }
    }
  }
  return {};
}

tl::optional<SemanticChecker::semantic_error>
SemanticChecker::traverse_scopes_and_check(
    Stmt *stmt, vector<vector<symbol_type>> &symbols) {

  tl::optional<SemanticChecker::semantic_error> maybe_err;
  if (auto *fn = dynamic_cast<FnStmt *>(stmt); fn != nullptr) {

    // Put the function in table
    // Put the params in table
    // For every statement in body, check if they are valid and declared symbols

    symbols.back().push_back(fn);

    std::deque<std::unique_ptr<LetStmt>> params_as_let_stmt;
    std::ranges::transform(
        fn->params, std::back_inserter(params_as_let_stmt),
        [&, i = 0](const Token &tok) mutable -> std::unique_ptr<LetStmt> {
          return std::make_unique<LetStmt>(
              tok,
              Token(TokenType::IDENTIFIER, fn->param_types[i], std::monostate(),
                    tok.line),
              std::make_unique<expr::LiteralExpr>(std::monostate()));
        });

    symbols.emplace_back();

    std::ranges::for_each(params_as_let_stmt,
                          [&](std::unique_ptr<LetStmt> &param) {
                            symbols.back().push_back(param.get());
                          });

    for (stmt_ptr &body_stmt : fn->body) {
      maybe_err = traverse_scopes_and_check(body_stmt.get(), symbols);
      if (maybe_err.has_value()) {
        break;
      }
    }
    symbols.pop_back();
    return maybe_err;
  }

  if (auto *let_stmt = dynamic_cast<LetStmt *>(stmt); let_stmt != nullptr) {
    // Check if rhs is valid
    return traverse_scopes_and_check(let_stmt->initializer_expr.get(), symbols)
        .or_else([&]() {
          if (!let_stmt->type.has_value()) {
            auto val = get_expr_type(let_stmt->initializer_expr.get(), symbols);
            if (val.has_value()) {
              let_stmt->type =
                  Token(TokenType::IDENTIFIER,
                        std::visit(
                            overloaded{
                                [](type_t &tp) { return tp; },
                                [](std::pair<std::vector<type_t>, type_t> &fn) {
                                  return fn.second;
                                }},
                            val.value()),
                        std::monostate(), let_stmt->name.line);
            }
          }
        })
        .or_else([&]() {
          return get_expr_type(let_stmt->initializer_expr.get(), symbols)
              .and_then([&](Type &&rhs_type)
                            -> tl::optional<SemanticChecker::semantic_error> {
                // LetStmt only supports normal types, not possible to do
                // function types as of now, so we'll typecheck and if failure
                // return error
                if (let_stmt->type.has_value()) {
                  if (std::visit(
                          overloaded{[&](type_t &type) {
                                       return type ==
                                              let_stmt->type.value().lexeme;
                                     },
                                     [&](auto) { return false; }},
                          rhs_type)) {
                    return {};
                  }
                }
                return SemanticChecker::semantic_error(
                    fmt::format("Type mismatch at line {} in let stmt ",
                                let_stmt->name.line)
                        .c_str());
              });
        })
        .or_else([&]() { symbols.back().push_back(let_stmt); });
  }

  if (auto *block_stmt = dynamic_cast<BlockStmt *>(stmt);
      block_stmt != nullptr) {
    // Create new frame
    symbols.emplace_back();

    for (auto &blck_entry : block_stmt->statements) {
      maybe_err = traverse_scopes_and_check(blck_entry.get(), symbols);
      if (maybe_err.has_value()) {
        break;
      }
    }
    // Pop the frame
    symbols.pop_back();
  }

  if (auto *if_stmt = dynamic_cast<IfStmt *>(stmt); if_stmt != nullptr) {
    // Check if condition is valid, then check if then is valid and check if
    // else is valid
    return traverse_scopes_and_check(if_stmt->condition.get(), symbols)
        .or_else([&]() {
          return traverse_scopes_and_check(if_stmt->then_branch.get(), symbols);
        })
        .or_else([&]() -> tl::optional<SemanticChecker::semantic_error> {
          if (if_stmt->else_branch.has_value()) {
            return traverse_scopes_and_check(if_stmt->else_branch.value().get(),
                                             symbols);
          }
          return {};
        });
  }

  if (auto *while_stmt = dynamic_cast<WhileStmt *>(stmt);
      while_stmt != nullptr) {
    return traverse_scopes_and_check(while_stmt->condition.get(), symbols)
        .or_else([&]() {
          return traverse_scopes_and_check(while_stmt->body.get(), symbols);
        });
  }

  if (auto *data_decl_stmt = dynamic_cast<DataDeclStmt *>(stmt);
      data_decl_stmt != nullptr) {
    // TODO
    // symbols.back().push_back(data_decl_stmt);
  }

  if (auto *expr_stmt = dynamic_cast<ExprStmt *>(stmt); expr_stmt != nullptr) {
    return traverse_scopes_and_check(expr_stmt->expr.get(), symbols);
  }
  return {};
}

tl::optional<Type>
SemanticChecker::get_type(const std::string &symbol,
                          std::span<std::vector<symbol_type>> symbols) {
  return get_symbol(symbol, symbols).and_then([](symbol_type &&sym) {
    return std::visit(
        overloaded{[](FnStmt *fn) -> tl::optional<Type> {
                     return std::pair{fn->param_types, fn->return_type};
                   },
                   [](LetStmt *let) {
                     return let->type.has_value()
                                ? tl::optional<Type>(let->type.value().lexeme)
                                : tl::nullopt;
                   },
                   [](auto) -> tl::optional<Type> { return {}; }},
        sym);
  });
}

tl::optional<symbol_type>
SemanticChecker::get_symbol(const std::string &symbol,
                            std::span<std::vector<symbol_type>> symbols) {
  for (auto &spn : symbols | std::views::reverse) {
    for (symbol_type &sym : spn | std::views::reverse) {
      auto symbol_name =
          std::visit(overloaded{[&](FnStmt *fn) { return fn->name.lexeme; },
                                [&](LetStmt *let) { return let->name.lexeme; },
                                [&](DataDeclStmt *data) {
                                  return data->struct_name.lexeme;
                                }},
                     sym);
      if (symbol_name == symbol) {
        return sym;
      }
    }
  }
  return {};
}

tl::optional<Type>
SemanticChecker::get_expr_type(expr::Expr *exp,
                               std::span<std::vector<symbol_type>> symbols) {
  IF_IS_TYPE(bin_exp, expr::BinaryExpr, exp) {
    return get_expr_type(bin_exp->left.get(), symbols);
  }

  IF_IS_TYPE(unary_expr, expr::PrefixExpr, exp) {
    return get_expr_type(unary_expr->right.get(), symbols);
  }
  IF_IS_TYPE(var_expr, expr::VarExpr, exp) {
    return this->get_type(var_expr->name.lexeme, symbols);
  }
  IF_IS_TYPE(assign_expr, expr::AssignExpr, exp) {
    return this->get_type(assign_expr->name.lexeme, symbols);
  }
  IF_IS_TYPE(lit_expr, expr::LiteralExpr, exp) {
    return std::visit(
        overloaded{[&](int64_t) { return "int"; },
                   [](double) { return "double"; }, [](bool) { return "bool"; },
                   [](std::string) { return "string"; },
                   [](char) { return "char"; }, [](auto) { return "void"; }},
        lit_expr->value);
  }
  IF_IS_TYPE(cond_expr, expr::ConditionalExpr, exp) {
    return get_expr_type(cond_expr->else_expr.get(), symbols);
  }
  IF_IS_TYPE(call_expr, expr::CallExpr, exp) {
    IF_IS_TYPE(var_expr, expr::VarExpr, call_expr->callee.get()) {
      this->get_symbol(var_expr->name.lexeme, symbols)
          .and_then([](symbol_type &&sym) {
            return std::visit(
                overloaded{[&](FnStmt *fn) -> tl::optional<Type> {
                             return std::pair{fn->param_types, fn->return_type};
                           },
                           [&](auto) -> tl::optional<Type> { return {}; }},
                sym);
          });
    }
    return {};
  }
  return {};
}

vector<SemanticChecker::semantic_error>
SemanticChecker::infer_type_type_check_and_undeclared_symbols_check() {

  std::vector<SemanticChecker::semantic_error> errors;
  std::vector<std::vector<symbol_type>> symbols(1, std::vector<symbol_type>());
  std::ranges::for_each(this->stmts, [&](std::unique_ptr<Stmt> &stmt) {
    auto res = traverse_scopes_and_check(stmt.get(), symbols);
    if (res.has_value()) {
      errors.push_back(res.value());
    }
  });
  return errors;
}
