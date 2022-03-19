#pragma once

#include <memory>

#include "Common.hpp"
#include "Lexer.hpp"

namespace expr {
using literal_type = literal_type;

class Expr {
public:
  virtual ~Expr();
};

struct BinaryExpr : public Expr {
  Token op;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;
  BinaryExpr(Token op, std::unique_ptr<Expr> left, std::unique_ptr<Expr> right);
} __attribute__((aligned(128))) __attribute__((packed));

struct PrefixExpr : public Expr {
  Token op;
  std::unique_ptr<Expr> right;
  PrefixExpr(Token op, std::unique_ptr<Expr> right);
} __attribute__((aligned(128))) __attribute__((packed));

struct VarExpr : public Expr {
  Token name;
  VarExpr(Token name);
} __attribute__((aligned(128))) __attribute__((packed));

struct LiteralExpr : public Expr {
  literal_type value;

	LiteralExpr(literal_type val);
};

} // namespace expr
