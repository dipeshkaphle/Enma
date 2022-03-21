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
};

struct PrefixExpr : public Expr {
  Token op;
  std::unique_ptr<Expr> right;
  PrefixExpr(Token op, std::unique_ptr<Expr> right);
  [[nodiscard]] string to_string() const override;
};

struct VarExpr : public Expr {
  Token name;
  VarExpr(Token name);

  [[nodiscard]] string to_string() const override;
};

struct LiteralExpr : public Expr {
  literal_type value;

  LiteralExpr(literal_type val);

  [[nodiscard]] string to_string() const override;
};

struct CallExpr : public Expr {
  std::unique_ptr<Expr> callee;
  Token paren;
  std::vector<std::unique_ptr<Expr>> arguments;
  CallExpr(std::unique_ptr<Expr> callee, Token paren,
           std::vector<std::unique_ptr<Expr>> args);
  [[nodiscard]] string to_string() const override;
};

struct AssignExpr : public Expr {
  Token name;
  std::unique_ptr<Expr> value;
  AssignExpr(Token name, std::unique_ptr<Expr> val);
  [[nodiscard]] string to_string() const override;
};

} // namespace expr
