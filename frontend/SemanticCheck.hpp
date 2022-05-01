#pragma once

#include "Expression.hpp"
#include "Statements.hpp"

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
  void add_to_symtable(const std::string &scope, symbol_type entry);
  // void traverse_scopes_and_check_undeclared_variables(Stmt *stmt,
  // const std::string &scope,
  // size_t stmt_no);

  tl::optional<semantic_error>
  traverse_scopes_and_check(Expr *expr, vector<vector<symbol_type>> &symbols);

  tl::optional<semantic_error>
  traverse_scopes_and_check(Stmt *stmt, vector<vector<symbol_type>> &symbols);

  bool has_symbol(vector<vector<symbol_type>> &symbols,
                  const std::string &symbol_name);

  tl::optional<Type> get_type(const std::string &symbol,
                              std::span<std::vector<symbol_type>> symbols);
  tl::optional<Type> get_expr_type(expr::Expr *exp,
                                   std::span<std::vector<symbol_type>> symbols);

  tl::optional<symbol_type>
  get_symbol(const std::string &symbol,
             std::span<std::vector<symbol_type>> symbols);

  bool is_primitive_type(const std::string &symbol);

public:
  static inline std::unordered_map<std::string, std::deque<symbol_type>>
      sym_table;
  std::vector<std::unique_ptr<Stmt>> stmts;
  SemanticChecker(std::vector<std::unique_ptr<Stmt>>);

  vector<semantic_error> infer_type_type_check_and_undeclared_symbols_check();

  // vector<type_error> infer_type_and_do_type_error_check();
};
