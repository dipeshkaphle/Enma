#pragma once

#include "Common.hpp"
#include "Lexer.hpp"

#include <memory>

namespace expr {
using literal_type = literal_type;

class Expr {
public:
  virtual ~Expr();
  [[nodiscard]] virtual string to_string() const = 0;
};

struct BinaryExpr : public Expr {
  Token op;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;
  BinaryExpr(Token op, std::unique_ptr<Expr> left, std::unique_ptr<Expr> right);
  [[nodiscard]] string to_string() const override;
} __attribute__((aligned(128))) __attribute__((packed));

struct PrefixExpr : public Expr {
  Token op;
  std::unique_ptr<Expr> right;
  PrefixExpr(Token op, std::unique_ptr<Expr> right);
  [[nodiscard]] string to_string() const override;

} __attribute__((aligned(128))) __attribute__((packed));

struct VarExpr : public Expr {
  Token name;
  VarExpr(Token name);

  [[nodiscard]] string to_string() const override;
} __attribute__((aligned(128))) __attribute__((packed));

struct LiteralExpr : public Expr {
  literal_type value;

  LiteralExpr(literal_type val);

  [[nodiscard]] string to_string() const override;
};

} // namespace expr
