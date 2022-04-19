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

tl::optional<SemanticChecker::semantic_error>
SemanticChecker::traverse_scopes_and_check_undeclared_variables(
    expr::Expr *expr, vector<vector<symbol_type>> &symbols) {
  if (auto *assign_expr = dynamic_cast<expr::AssignExpr *>(expr);
      assign_expr != nullptr) {
    return traverse_scopes_and_check_undeclared_variables(
               assign_expr->value.get(), symbols)
        .or_else([&]() -> tl::optional<SemanticChecker::semantic_error> {
          if (!has_symbol(symbols, assign_expr->name.lexeme)) {
            return tl::make_optional<SemanticChecker::semantic_error>(
                SemanticChecker::semantic_error(
                    fmt::format("Undeclared symbol {}",
                                assign_expr->name.lexeme)
                        .c_str()));
          }
          return {};
        });
  }
  if (auto *var_expr = dynamic_cast<expr::VarExpr *>(expr);
      var_expr != nullptr) {
    if (!has_symbol(symbols, var_expr->name.lexeme)) {
      return tl::make_optional<SemanticChecker::semantic_error>(
          SemanticChecker::semantic_error(
              fmt::format("Undeclared symbol {}", var_expr->name.lexeme)
                  .c_str()));
    }
  }
  if (auto *cond_expr = dynamic_cast<expr::ConditionalExpr *>(expr);
      cond_expr != nullptr) {

    return traverse_scopes_and_check_undeclared_variables(cond_expr->cond.get(),
                                                          symbols)
        .or_else([&]() {
          return traverse_scopes_and_check_undeclared_variables(
              cond_expr->then_expr.get(), symbols);
        })
        .or_else([&]() {
          return traverse_scopes_and_check_undeclared_variables(
              cond_expr->else_expr.get(), symbols);
        });
  }

  if (auto *bin_expr = dynamic_cast<expr::BinaryExpr *>(expr);
      bin_expr != nullptr) {
    return traverse_scopes_and_check_undeclared_variables(bin_expr->left.get(),
                                                          symbols)
        .or_else([&]() {
          return traverse_scopes_and_check_undeclared_variables(
              bin_expr->right.get(), symbols);
        });
  }

  if (auto *prefix_expr = dynamic_cast<expr::PrefixExpr *>(expr);
      prefix_expr != nullptr) {
    return traverse_scopes_and_check_undeclared_variables(
        prefix_expr->right.get(), symbols);
  }

  if (auto *call_expr = dynamic_cast<expr::CallExpr *>(expr);
      call_expr != nullptr) {
    if (auto *var = dynamic_cast<expr::VarExpr *>(call_expr->callee.get());
        var != nullptr) {
      if (!has_symbol(symbols, var->name.lexeme)) {
        return tl::make_optional<SemanticChecker::semantic_error>(
            SemanticChecker::semantic_error(
                fmt::format("Undeclared symbol {}", var->name.lexeme).c_str()));
      }
    }

    for (auto &exp : call_expr->arguments) {
      auto res =
          traverse_scopes_and_check_undeclared_variables(exp.get(), symbols);
      if (res.has_value()) {
        return res;
      }
    }
  }
  return {};
}

tl::optional<SemanticChecker::semantic_error>
SemanticChecker::traverse_scopes_and_check_undeclared_variables(
    Stmt *stmt, vector<vector<symbol_type>> &symbols) {

  // TODO

  tl::optional<SemanticChecker::semantic_error> maybe_err;
  if (auto *fn = dynamic_cast<FnStmt *>(stmt); fn != nullptr) {

    // Put the function in table
    // Put the params in table
    // For every statement in body, check if they are valid and declared symbols

    symbols.back().push_back(fn);

    std::deque<std::unique_ptr<LetStmt>> params_as_let_stmt;
    std::ranges::transform(
        fn->params, std::back_inserter(params_as_let_stmt),
        [&](const Token &tok) -> std::unique_ptr<LetStmt> {
          return std::make_unique<LetStmt>(
              tok, std::nullopt,
              std::make_unique<expr::LiteralExpr>(std::monostate()));
        });

    symbols.emplace_back();

    std::ranges::for_each(params_as_let_stmt,
                          [&](std::unique_ptr<LetStmt> &param) {
                            symbols.back().push_back(param.get());
                          });

    for (stmt_ptr &body_stmt : fn->body) {
      maybe_err = traverse_scopes_and_check_undeclared_variables(
          body_stmt.get(), symbols);
      if (maybe_err.has_value()) {
        break;
      }
    }
    symbols.pop_back();
    return maybe_err;
  }

  if (auto *let_stmt = dynamic_cast<LetStmt *>(stmt); let_stmt != nullptr) {
    // Check if rhs is valid
    return traverse_scopes_and_check_undeclared_variables(
               let_stmt->initializer_expr.get(), symbols)
        .or_else([&]() { symbols.back().push_back(let_stmt); });
  }

  if (auto *block_stmt = dynamic_cast<BlockStmt *>(stmt);
      block_stmt != nullptr) {
    // Create new frame
    symbols.emplace_back();

    for (auto &blck_entry : block_stmt->statements) {
      maybe_err = traverse_scopes_and_check_undeclared_variables(
          blck_entry.get(), symbols);
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
    return traverse_scopes_and_check_undeclared_variables(
               if_stmt->condition.get(), symbols)
        .or_else([&]() {
          return traverse_scopes_and_check_undeclared_variables(
              if_stmt->then_branch.get(), symbols);
        })
        .or_else([&]() -> tl::optional<SemanticChecker::semantic_error> {
          if (if_stmt->else_branch.has_value()) {
            return traverse_scopes_and_check_undeclared_variables(
                if_stmt->else_branch.value().get(), symbols);
          }
          return {};
        });
  }

  if (auto *while_stmt = dynamic_cast<WhileStmt *>(stmt);
      while_stmt != nullptr) {
    return traverse_scopes_and_check_undeclared_variables(
               while_stmt->condition.get(), symbols)
        .or_else([&]() {
          return traverse_scopes_and_check_undeclared_variables(
              while_stmt->body.get(), symbols);
        });
  }

  if (auto *expr_stmt = dynamic_cast<ExprStmt *>(stmt); expr_stmt != nullptr) {
    return traverse_scopes_and_check_undeclared_variables(expr_stmt->expr.get(),
                                                          symbols);
  }
  return {};
}

vector<SemanticChecker::semantic_error>
SemanticChecker::undeclared_symbols_check() {

  std::vector<SemanticChecker::semantic_error> errors;
  std::vector<std::vector<symbol_type>> symbols(1,std::vector<symbol_type>());
  std::ranges::for_each(this->stmts, [&](std::unique_ptr<Stmt> &stmt) {
    auto res =
        traverse_scopes_and_check_undeclared_variables(stmt.get(), symbols);
    if (res.has_value()) {
      errors.push_back(res.value());
    }
  });
  return errors;
}

vector<SemanticChecker::semantic_error> infer_type_and_do_type_error_check() {
  return {};
}
