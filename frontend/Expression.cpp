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
string BinaryExpr::to_string() const {
  return fmt::format("({} {} {})", op.lexeme, left->to_string(),
                     right->to_string());
}

PrefixExpr::PrefixExpr(Token op, std::unique_ptr<Expr> right)
    : op(std::move(op)), right(std::move(right)) {}

string PrefixExpr::to_string() const {
  return fmt::format("({} {})", op.lexeme, this->right->to_string());
}

VarExpr::VarExpr(Token name) : name(std::move(name)) {}

string VarExpr::to_string() const {
  return fmt::format("{}", this->name.lexeme);
}

LiteralExpr::LiteralExpr(literal_type val) : value(std::move(val)) {}

string LiteralExpr::to_string() const {
  return fmt::format("{}", literal_to_string(this->value));
}

CallExpr::CallExpr(std::unique_ptr<Expr> callee, Token paren,
                   std::vector<std::unique_ptr<Expr>> args)
    : callee(std::move(callee)), paren(std::move(paren)),
      arguments(std::move(args)) {}
string CallExpr::to_string() const {
  return fmt::format("(call {} [{}])", callee->to_string(),
                     fmt::join(arguments | std::views::transform([](auto &x) {
                                 return x->to_string();
                               }) | tl::to<std::vector<std::string>>(),
                               ", "));
}
