#pragma once

#include "Common.hpp"
#include "Expression.hpp"

#include <tl/optional.hpp>

#include <span>
#include <vector>

struct SymTable {
  std::vector<std::vector<symbol_type>> symbols;
  SymTable();

  void push_frame();
  void pop_frame();
  std::vector<symbol_type> &back();
  bool has_symbol(const std::string &symbol_name);
  tl::optional<Type> get_type(const std::string &symbol);
  tl::optional<Type> get_expr_type(expr::Expr *exp);
  tl::optional<symbol_type> get_symbol(const std::string &symbol);
};
