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
// void SemanticChecker::add_to_symtable(const std::string &scope,
// symbol_type entry) {
// this->sym_table[scope].emplace_back(entry);
// }

bool SemanticChecker::is_primitive_type(const std::string &symbol) {
  return std::ranges::any_of(
      std::array{"int", "double", "string", "char", "bool", "void"},
      [&](const char *tp) { return tp == symbol; });
}

tl::optional<SemanticChecker::semantic_error>
SemanticChecker::traverse_scopes_and_check(expr::Expr *expr, SymTable &table) {

  if (auto *assign_expr = dynamic_cast<expr::AssignExpr *>(expr);
      assign_expr != nullptr) {
    // Its an assignment expression
    // Check the rhs and then check if lhs is a valid symbol
    return traverse_scopes_and_check(assign_expr->value.get(), table)
        .or_else([&]() -> tl::optional<SemanticChecker::semantic_error> {
          if (!table.has_symbol(assign_expr->name.lexeme)) {
            return tl::make_optional<SemanticChecker::semantic_error>(
                SemanticChecker::semantic_error(
                    fmt::format("Undeclared symbol {} in line {}",
                                assign_expr->name.lexeme,
                                assign_expr->name.line)
                        .c_str()));
          }
          if (table.get_type(assign_expr->name.lexeme) !=
              table.get_expr_type(assign_expr->value.get())) {
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
    if (!table.has_symbol(var_expr->name.lexeme)) {
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
    return traverse_scopes_and_check(cond_expr->cond.get(), table)
        .or_else([&]() {
          return traverse_scopes_and_check(cond_expr->then_expr.get(), table);
        })
        .or_else([&]() {
          return traverse_scopes_and_check(cond_expr->else_expr.get(), table);
        })
        .or_else([&]() -> tl::optional<SemanticChecker::semantic_error> {
          if (table.get_expr_type(cond_expr->else_expr.get()) !=
              table.get_expr_type(cond_expr->then_expr.get())) {
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
    return traverse_scopes_and_check(bin_expr->left.get(), table)
        .or_else([&]() {
          return traverse_scopes_and_check(bin_expr->right.get(), table);
        })
        .or_else([&]() -> tl::optional<SemanticChecker::semantic_error> {
          if (table.get_expr_type(bin_expr->left.get()) !=
              table.get_expr_type(bin_expr->right.get())) {
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
    // std::span<std::vector<symbol_type>> table)
  }

  if (auto *prefix_expr = dynamic_cast<expr::PrefixExpr *>(expr);
      prefix_expr != nullptr) {
    // PrefixExpr: Check the expression
    return traverse_scopes_and_check(prefix_expr->right.get(), table)
        .or_else([&]() -> tl::optional<SemanticChecker::semantic_error> {
          auto type = table.get_expr_type(prefix_expr->right.get());
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
      if (!table.has_symbol(var->name.lexeme)) {
        return tl::make_optional<SemanticChecker::semantic_error>(
            SemanticChecker::semantic_error(
                fmt::format("Undeclared symbol {} in line {}", var->name.lexeme,
                            var->name.line)
                    .c_str()));
      }
    }

    for (auto &exp : call_expr->arguments) {
      auto res = traverse_scopes_and_check(exp.get(), table);
      if (res.has_value()) {
        return res;
      }
    }
  }
  return {};
}

tl::optional<SemanticChecker::semantic_error>
SemanticChecker::traverse_scopes_and_check(Stmt *stmt, SymTable &table) {

  tl::optional<SemanticChecker::semantic_error> maybe_err;
  if (auto *fn = dynamic_cast<FnStmt *>(stmt); fn != nullptr) {

    // Put the function in table
    // Put the params in table
    // For every statement in body, check if they are valid and declared table

    table.back().emplace_back(fn);

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

    table.push_frame();

    std::ranges::for_each(params_as_let_stmt,
                          [&](std::unique_ptr<LetStmt> &param) {
                            table.back().emplace_back(param.get());
                          });

    for (stmt_ptr &body_stmt : fn->body) {
      maybe_err = traverse_scopes_and_check(body_stmt.get(), table);
      if (maybe_err.has_value()) {
        break;
      }
    }
    table.pop_frame();
    return maybe_err;
  }

  if (auto *let_stmt = dynamic_cast<LetStmt *>(stmt); let_stmt != nullptr) {
    // Check if rhs is valid
    return traverse_scopes_and_check(let_stmt->initializer_expr.get(), table)
        .or_else([&]() {
          if (!let_stmt->type.has_value()) {
            auto val = table.get_expr_type(let_stmt->initializer_expr.get());
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
          return table.get_expr_type(let_stmt->initializer_expr.get())
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
        .or_else([&]() { table.back().emplace_back(let_stmt); });
  }

  if (auto *block_stmt = dynamic_cast<BlockStmt *>(stmt);
      block_stmt != nullptr) {
    // Create new frame
    table.push_frame();

    for (auto &blck_entry : block_stmt->statements) {
      maybe_err = traverse_scopes_and_check(blck_entry.get(), table);
      if (maybe_err.has_value()) {
        break;
      }
    }
    // Pop the frame
    table.pop_frame();
  }

  if (auto *if_stmt = dynamic_cast<IfStmt *>(stmt); if_stmt != nullptr) {
    // Check if condition is valid, then check if then is valid and check if
    // else is valid
    return traverse_scopes_and_check(if_stmt->condition.get(), table)
        .or_else([&]() {
          return traverse_scopes_and_check(if_stmt->then_branch.get(), table);
        })
        .or_else([&]() -> tl::optional<SemanticChecker::semantic_error> {
          if (if_stmt->else_branch.has_value()) {
            return traverse_scopes_and_check(if_stmt->else_branch.value().get(),
                                             table);
          }
          return {};
        });
  }

  if (auto *while_stmt = dynamic_cast<WhileStmt *>(stmt);
      while_stmt != nullptr) {
    return traverse_scopes_and_check(while_stmt->condition.get(), table)
        .or_else([&]() {
          return traverse_scopes_and_check(while_stmt->body.get(), table);
        });
  }

  if (auto *data_decl_stmt = dynamic_cast<DataDeclStmt *>(stmt);
      data_decl_stmt != nullptr) {
    // TODO
    // table.back().push_back(data_decl_stmt);
  }

  if (auto *print_stmt = dynamic_cast<PrintStmt *>(stmt);
      print_stmt != nullptr) {
    return traverse_scopes_and_check(print_stmt->expr.get(), table);
  }

  if (auto *ret_stmt = dynamic_cast<ReturnStmt *>(stmt); ret_stmt != nullptr) {
    if (ret_stmt->value.has_value()) {
      return traverse_scopes_and_check(ret_stmt->value.value().get(), table);
    }
  }

  if (auto *expr_stmt = dynamic_cast<ExprStmt *>(stmt); expr_stmt != nullptr) {
    return traverse_scopes_and_check(expr_stmt->expr.get(), table);
  }
  return {};
}

vector<SemanticChecker::semantic_error>
SemanticChecker::infer_type_type_check_and_undeclared_symbols_check() {

  std::vector<SemanticChecker::semantic_error> errors;
  // std::vector<std::vector<symbol_type>> table(1, std::vector<symbol_type>());
  SymTable table;
  std::ranges::for_each(this->stmts, [&](std::unique_ptr<Stmt> &stmt) {
    auto res = traverse_scopes_and_check(stmt.get(), table);
    if (res.has_value()) {
      errors.push_back(res.value());
    }
  });
  return errors;
}
