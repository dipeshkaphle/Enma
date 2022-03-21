#include "Expression.hpp"
#include "Common.hpp"

#include <fmt/format.h>

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
