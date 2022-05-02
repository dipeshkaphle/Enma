#pragma once

#include "Expression.hpp"
#include "Statements.hpp"
#include "SymbolTable.hpp"

#include <tl/optional.hpp>

#include <deque>
#include <span>
#include <unordered_map>

class SemanticChecker {
public:
  struct semantic_error : public std::runtime_error {
    explicit semantic_error(const char *err_msg) : runtime_error(err_msg) {}
  };

private:
  tl::optional<semantic_error> traverse_scopes_and_check(Expr *expr,
                                                         SymTable &table);

  tl::optional<semantic_error> traverse_scopes_and_check(Stmt *stmt,
                                                         SymTable &table);

  bool is_primitive_type(const std::string &symbol);

public:
  std::vector<std::unique_ptr<Stmt>> stmts;
  SemanticChecker(std::vector<std::unique_ptr<Stmt>>);

  vector<semantic_error> infer_type_type_check_and_undeclared_symbols_check();

  // vector<type_error> infer_type_and_do_type_error_check();
};
