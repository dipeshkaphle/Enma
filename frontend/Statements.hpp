#pragma once

#include "Common.hpp"
#include "Expression.hpp"

#include <memory>
#include <optional>
#include <vector>

using Expr = expr::Expr;

class Stmt {
public:
  virtual ~Stmt();
  [[nodiscard]] virtual string to_sexp() const = 0;
};

using stmt_ptr = std::unique_ptr<Stmt>;

struct BlockStmt : public Stmt {
  //

  std::vector<stmt_ptr> statements;
  BlockStmt(std::vector<stmt_ptr> stmts);
  BlockStmt(BlockStmt &&stmt) noexcept;
  [[nodiscard]] string to_sexp() const override;
};
struct BreakStmt : public Stmt {
  BreakStmt();
  [[nodiscard]] string to_sexp() const override;
};
struct ContinueStmt : public Stmt {
  ContinueStmt();
  [[nodiscard]] string to_sexp() const override;
};

struct DataDeclStmt : public Stmt {
  Token struct_name;
  std::vector<Token> names;
  std::vector<Token> types;
  DataDeclStmt(Token struct_name, std::vector<Token> names,
               std::vector<Token> types);
  [[nodiscard]] string to_sexp() const override;
};

struct ExprStmt : public Stmt {
  std::unique_ptr<Expr> expr;
  ExprStmt(std::unique_ptr<Expr> expr);
  [[nodiscard]] string to_sexp() const override;
};
struct FnStmt : public Stmt {
  Token name;
  std::vector<Token> params;
  std::vector<std::string> param_types;
  // can't store a block because of the possibility of early returns
  std::vector<stmt_ptr> body;

  FnStmt(Token fn_name, std::vector<Token> params,
         std::vector<std::string> param_types, std::vector<stmt_ptr> fn_body);
  [[nodiscard]] string to_sexp() const override;
};
struct IfStmt : public Stmt {
  std::unique_ptr<Expr> condition;
  std::unique_ptr<Stmt> then_branch;
  std::optional<std::unique_ptr<Stmt>> else_branch;

  IfStmt(std::unique_ptr<Expr> condition, std::unique_ptr<Stmt> then_branch,
         std::optional<std::unique_ptr<Stmt>> else_branch = std::nullopt);
  [[nodiscard]] string to_sexp() const override;
};
struct LetStmt : public Stmt {
  Token name;
  Token type;
  std::unique_ptr<Expr> initializer_expr;
  LetStmt(Token name, Token type, std::unique_ptr<Expr> expr);
  [[nodiscard]] string to_sexp() const override;
};
struct PrintStmt : public Stmt {
  std::unique_ptr<Expr> expr;
  bool has_newline;
  PrintStmt(std::unique_ptr<Expr> expr, bool new_line = false);
  [[nodiscard]] string to_sexp() const override;
};
struct ReturnStmt : public Stmt {
  Token keyword;
  std::optional<std::unique_ptr<Expr>> value;
  ReturnStmt(Token keyword,
             std::optional<std::unique_ptr<Expr>> val = std::nullopt);
  [[nodiscard]] string to_sexp() const override;
};
struct WhileStmt : public Stmt {
  std::unique_ptr<Expr> condition;
  std::unique_ptr<Stmt> body;
  std::optional<std::unique_ptr<Stmt>> change_fn;

  WhileStmt(std::unique_ptr<Expr> condition, std::unique_ptr<Stmt> body,
            std::optional<std::unique_ptr<Stmt>> change_fn = std::nullopt);
  [[nodiscard]] string to_sexp() const override;
};
