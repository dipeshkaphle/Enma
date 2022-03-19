#include "Expression.hpp"

using namespace expr;

Expr::~Expr() = default;

BinaryExpr::BinaryExpr(Token op, std::unique_ptr<Expr> left,
                       std::unique_ptr<Expr> right)
    : op(std::move(op)), left(std::move(left)), right(std::move(right)) {}

PrefixExpr::PrefixExpr(Token op, std::unique_ptr<Expr> right)
    : op(std::move(op)), right(std::move(right)) {}

VarExpr::VarExpr(Token name) : name(std::move(name)) {}

LiteralExpr::LiteralExpr(literal_type val) : value(std::move(val)) {}
