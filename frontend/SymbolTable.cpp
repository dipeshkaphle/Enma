#include "SymbolTable.hpp"
#include "Statements.hpp"

#include <iterator>
#include <ranges>

#define IF_IS_TYPE(VAR, TYPE, EXP)                                             \
  if (auto *VAR = dynamic_cast<TYPE *>(EXP); VAR != nullptr)

SymTable::SymTable() : symbols(1) {}

void SymTable::push_frame() { symbols.emplace_back(); }
void SymTable::pop_frame() { symbols.pop_back(); }
std::vector<symbol_type> &SymTable::back() { return symbols.back(); }
bool SymTable::has_symbol(const std::string &symbol_name) {
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
tl::optional<Type> SymTable::get_type(const std::string &symbol) {
  return get_symbol(symbol).and_then([](symbol_type &&sym) {
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
tl::optional<Type> SymTable::get_expr_type(expr::Expr *exp) {
  IF_IS_TYPE(bin_exp, expr::BinaryExpr, exp) {
    return get_expr_type(bin_exp->left.get());
  }

  IF_IS_TYPE(unary_expr, expr::PrefixExpr, exp) {
    return get_expr_type(unary_expr->right.get());
  }
  IF_IS_TYPE(var_expr, expr::VarExpr, exp) {
    return this->get_type(var_expr->name.lexeme);
  }
  IF_IS_TYPE(assign_expr, expr::AssignExpr, exp) {
    return this->get_type(assign_expr->name.lexeme);
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
    return get_expr_type(cond_expr->else_expr.get());
  }
  IF_IS_TYPE(call_expr, expr::CallExpr, exp) {
    IF_IS_TYPE(var_expr, expr::VarExpr, call_expr->callee.get()) {
      this->get_symbol(var_expr->name.lexeme).and_then([](symbol_type &&sym) {
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
tl::optional<symbol_type> SymTable::get_symbol(const std::string &symbol) {
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
