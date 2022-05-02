#pragma once

#include "Common.hpp"
#include "Instuctions.hpp"
#include "Lexer.hpp"

#include <memory>

namespace expr {
using literal_type = literal_type;

template <typename T> class ExprVisitor {
  //
};

class Expr : public ExprVisitor<visit_type> {
public:
  virtual ~Expr();
  [[nodiscard]] virtual string to_sexp() const = 0;
  [[nodiscard]] virtual std::vector<std::string> gen_intermediate() = 0;
  [[nodiscard]] virtual std::vector<Instr> compile_to_bytecode() = 0;
};

struct BinaryExpr : public Expr {
  Token op;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;
  BinaryExpr(Token op, std::unique_ptr<Expr> left, std::unique_ptr<Expr> right);
  [[nodiscard]] string to_sexp() const override;
  std::vector<std::string> gen_intermediate() override;
  std::vector<Instr> compile_to_bytecode() override;
};

struct PrefixExpr : public Expr {
  Token op;
  std::unique_ptr<Expr> right;
  PrefixExpr(Token op, std::unique_ptr<Expr> right);
  [[nodiscard]] string to_sexp() const override;
  std::vector<std::string> gen_intermediate() override;
  std::vector<Instr> compile_to_bytecode() override;
};

struct AttrAccessExpr : public Expr {
  Token op;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;
  AttrAccessExpr(Token op, std::unique_ptr<Expr> left,
                 std::unique_ptr<Expr> right);
  [[nodiscard]] string to_sexp() const override;
  std::vector<std::string> gen_intermediate() override;
  std::vector<Instr> compile_to_bytecode() override;
};

struct VarExpr : public Expr {
  Token name;
  VarExpr(Token name);

  [[nodiscard]] string to_sexp() const override;
  std::vector<std::string> gen_intermediate() override;
  std::vector<Instr> compile_to_bytecode() override;
};

struct LiteralExpr : public Expr {
  literal_type value;

  LiteralExpr(literal_type val);

  [[nodiscard]] string to_sexp() const override;
  std::vector<std::string> gen_intermediate() override;
  std::vector<Instr> compile_to_bytecode() override;
};

struct CallExpr : public Expr {
  std::unique_ptr<Expr> callee;
  Token paren;
  std::vector<std::unique_ptr<Expr>> arguments;
  CallExpr(std::unique_ptr<Expr> callee, Token paren,
           std::vector<std::unique_ptr<Expr>> args);
  [[nodiscard]] string to_sexp() const override;
  std::vector<std::string> gen_intermediate() override;
  std::vector<Instr> compile_to_bytecode() override;
};

struct AssignExpr : public Expr {
  Token name;
  std::unique_ptr<Expr> value;
  AssignExpr(Token name, std::unique_ptr<Expr> val);
  [[nodiscard]] string to_sexp() const override;
  std::vector<std::string> gen_intermediate() override;
  std::vector<Instr> compile_to_bytecode() override;
};

struct ConditionalExpr : public Expr {
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Expr> then_expr;
  std::unique_ptr<Expr> else_expr;
  ConditionalExpr(std::unique_ptr<Expr> cond, std::unique_ptr<Expr> then_expr,
                  std::unique_ptr<Expr> else_expr);

  [[nodiscard]] string to_sexp() const override;
  std::vector<std::string> gen_intermediate() override;
  std::vector<Instr> compile_to_bytecode() override;
};

} // namespace expr
