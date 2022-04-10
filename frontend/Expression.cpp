#include "Expression.hpp"
#include "Common.hpp"

#include <fmt/format.h>
#include <tl/to.hpp>

#include <iterator>
#include <ranges>

using namespace expr;

Expr::~Expr() = default;

BinaryExpr::BinaryExpr(Token op, std::unique_ptr<Expr> left,
                       std::unique_ptr<Expr> right)
    : op(std::move(op)), left(std::move(left)), right(std::move(right)) {}
string BinaryExpr::to_sexp() const {
  return fmt::format("({} {} {})", op.lexeme, left->to_sexp(),
                     right->to_sexp());
}

PrefixExpr::PrefixExpr(Token op, std::unique_ptr<Expr> right)
    : op(std::move(op)), right(std::move(right)) {}

string PrefixExpr::to_sexp() const {
  return fmt::format("({} {})", op.lexeme, this->right->to_sexp());
}

VarExpr::VarExpr(Token name) : name(std::move(name)) {}

string VarExpr::to_sexp() const { return fmt::format("{}", this->name.lexeme); }

LiteralExpr::LiteralExpr(literal_type val) : value(std::move(val)) {}

string LiteralExpr::to_sexp() const {
  return fmt::format("{}", literal_to_string(this->value));
}

CallExpr::CallExpr(std::unique_ptr<Expr> callee, Token paren,
                   std::vector<std::unique_ptr<Expr>> args)
    : callee(std::move(callee)), paren(std::move(paren)),
      arguments(std::move(args)) {}
string CallExpr::to_sexp() const {
  return fmt::format("(call {} [{}])", callee->to_sexp(),
                     fmt::join(arguments | std::views::transform([](auto &x) {
                                 return x->to_sexp();
                               }) | tl::to<std::vector<std::string>>(),
                               ", "));
}

AssignExpr::AssignExpr(Token name, std::unique_ptr<Expr> val)
    : name(std::move(name)), value(std::move(val)) {}
string AssignExpr::to_sexp() const {
  return fmt::format("(= {} {})", name.lexeme, value->to_sexp());
}

ConditionalExpr::ConditionalExpr(std::unique_ptr<Expr> cond,
                                 std::unique_ptr<Expr> then_expr,
                                 std::unique_ptr<Expr> else_expr)
    : cond(std::move(cond)), then_expr(std::move(then_expr)),
      else_expr(std::move(else_expr)) {}

string ConditionalExpr::to_sexp() const {
  return fmt::format("(if {} {} {})", cond->to_sexp(), then_expr->to_sexp(),
                     else_expr->to_sexp());
}
