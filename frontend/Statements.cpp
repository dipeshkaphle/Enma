#include "Statements.hpp"
#include "Common.hpp"

#include <fmt/format.h>
#include <tl/to.hpp>

#include <iterator>

Stmt::~Stmt() = default;

BlockStmt::BlockStmt(std::vector<stmt_ptr> stmts)
    : statements(std::move(stmts)) {}
BlockStmt::BlockStmt(BlockStmt &&stmt) noexcept
    : statements(std::move(stmt.statements)) {}
string BlockStmt::to_sexp() const {
  return fmt::format(
      "(Block [{}])",
      fmt::join(

          this->statements | std::views::transform([&](auto &stmt) {
            return stmt.to_sexp();
          }) | tl::to<std::vector<std::string>>(),
          "\n"));
}

BreakStmt::BreakStmt() = default;
string BreakStmt::to_sexp() const { return fmt::format("(break)"); }

ContinueStmt::ContinueStmt() = default;
string ContinueStmt::to_sexp() const { return fmt::format("(continue)"); }

DataDeclStmt::DataDeclStmt(Token struct_name, std::vector<Token> names,
                           std::vector<Token> types)
    : struct_name(std::move(struct_name)), names(std::move(names)),
      types(std::move(types)) {}
string DataDeclStmt::to_sexp() const {
  return fmt::format(
      "(DataDefn {} [{}])", this->struct_name.lexeme,
      fmt::join(this->names | std::views::transform([&](auto &tok) {
                  return tok.lexeme;
                }) | tl::to<std::vector<std::string>>,
                ","));
}

ExprStmt::ExprStmt(std::unique_ptr<Expr> expr) : expr(std::move(expr)) {}
string ExprStmt::to_sexp() const { return fmt::format("{}", expr->to_sexp()); }

FnStmt::FnStmt(Token fn_name, std::vector<Token> params,
               std::vector<std::string> param_types,
               std::vector<stmt_ptr> fn_body)
    : name(std::move(fn_name)), params(std::move(params)),
      param_types(std::move(param_types)), body(std::move(fn_body)) {}
string FnStmt::to_sexp() const { return fmt::format(""); }

IfStmt::IfStmt(std::unique_ptr<Expr> condition,
               std::unique_ptr<Stmt> then_branch,
               std::optional<std::unique_ptr<Stmt>> else_branch)
    : condition(std::move(condition)), then_branch(std::move(then_branch)),
      else_branch(std::move(else_branch)) {}
string IfStmt::to_sexp() const { return fmt::format(""); }

LetStmt::LetStmt(Token name, Token type, std::unique_ptr<Expr> expr)
    : name(std::move(name)), type(std::move(type)),
      initializer_expr(std::move(expr)) {}
string LetStmt::to_sexp() const { return fmt::format(""); }

PrintStmt::PrintStmt(std::unique_ptr<Expr> expr, bool new_line)
    : expr(std::move(expr)), has_newline(new_line) {}
string PrintStmt::to_sexp() const { return fmt::format(""); }

ReturnStmt::ReturnStmt(Token keyword, std::optional<std::unique_ptr<Expr>> val)
    : keyword(std::move(keyword)), value(std::move(val)) {}
string ReturnStmt::to_sexp() const { return fmt::format(""); }

WhileStmt::WhileStmt(std::unique_ptr<Expr> condition,
                     std::unique_ptr<Stmt> body,
                     std::optional<std::unique_ptr<Stmt>> change_fn)
    : condition(std::move(condition)), body(std::move(body)),
      change_fn(std::move(change_fn)) {}
[[nodiscard]] string WhileStmt::to_sexp() const { return fmt::format(""); }
