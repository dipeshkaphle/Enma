#include "Statements.hpp"
#include "Common.hpp"

#include <fmt/format.h>
#include <tl/to.hpp>
// #include <tl/zip_transform.hpp>

#include <iterator>

Stmt::~Stmt() = default;

BlockStmt::BlockStmt(std::vector<stmt_ptr> stmts)
    : statements(std::move(stmts)) {}
BlockStmt::BlockStmt(BlockStmt &&stmt) noexcept
    : statements(std::move(stmt.statements)) {}
string BlockStmt::to_sexp() const {
  return fmt::format(
      "(Block [ {} ])",
      fmt::join(this->statements | std::views::transform([&](auto &stmt) {
                  return stmt->to_sexp();
                }) | tl::to<std::vector<std::string>>(),
                "; "));
}
vector<std::string> BlockStmt::gen_intermediate() {
  static int block_cnt = 0;
  std::vector<std::string> instrs;
  // auto blck = block_cnt;
  instrs.emplace_back(fmt::format(".BLOCK{}:", block_cnt++));
  for (auto &stmt : this->statements) {
    auto temp = stmt->gen_intermediate();
    instrs.insert(instrs.end(), temp.begin(), temp.end());
  }
  return instrs;
}
vector<std::string> BlockStmt::transpile_to_cpp() {
  auto all_stmts =
      this->statements | std::ranges::views::transform([](auto &s) {
        return fmt::format("{}", fmt::join(s->transpile_to_cpp(), "\n"));
      }) |
      tl::to<std::vector<std::string>>();
  return {fmt::format("{{\n  {} \n}}\n", fmt::join(all_stmts, "\n"))};
  // std::vector<std::string> output;
  // std::ranges::for_each(all_stmts, [&](vector<std::string> &vec) {
  // std::ranges::copy(vec, std::back_inserter(output));
  // });
  // return output;
}

BreakStmt::BreakStmt() = default;
string BreakStmt::to_sexp() const { return fmt::format("(break)"); }
vector<std::string> BreakStmt::gen_intermediate() { return {"  break"}; }
vector<std::string> BreakStmt::transpile_to_cpp() { return {"break;"}; }

ContinueStmt::ContinueStmt() = default;
string ContinueStmt::to_sexp() const { return fmt::format("(continue)"); }
vector<std::string> ContinueStmt::gen_intermediate() { return {"  continue"}; }
vector<std::string> ContinueStmt::transpile_to_cpp() { return {"continue;"}; }

DataDeclStmt::DataDeclStmt(Token struct_name, std::vector<Token> names,
                           std::vector<Token> types)
    : struct_name(std::move(struct_name)), names(std::move(names)),
      types(std::move(types)) {}
string DataDeclStmt::to_sexp() const {
  // auto v = tl::views::zip_transform(
  // [&](const auto &name, const auto &type) {
  // return fmt::format("{}: {}", name.lexeme, type.lexeme);
  // },
  // this->names, this->types);

  std::vector<std::string> param_and_types;
  std::transform(this->names.begin(), this->names.end(), this->types.begin(),
                 std::back_inserter(param_and_types),
                 [](const auto &name, const auto &type) {
                   return fmt::format("{}: {}", name.lexeme, type.lexeme);
                 });

  return fmt::format("(DataDefn {} [{}])", this->struct_name.lexeme,
                     fmt::join(param_and_types, ","));
}
vector<std::string> DataDeclStmt::gen_intermediate() { return {}; }
vector<std::string> DataDeclStmt::transpile_to_cpp() { return {}; }

ExprStmt::ExprStmt(std::unique_ptr<Expr> expr) : expr(std::move(expr)) {}
string ExprStmt::to_sexp() const { return fmt::format("{}", expr->to_sexp()); }
vector<std::string> ExprStmt::gen_intermediate() {
  return this->expr->gen_intermediate();
}
vector<std::string> ExprStmt::transpile_to_cpp() {
  return {fmt::format("{};", this->expr->transpile_to_cpp())};
}

FnStmt::FnStmt(Token fn_name, std::vector<Token> params,
               std::vector<type_t> param_types, type_t return_type,
               std::vector<stmt_ptr> fn_body)
    : name(std::move(fn_name)), params(std::move(params)),
      param_types(std::move(param_types)), return_type(std::move(return_type)),
      body(std::move(fn_body)) {}
string FnStmt::to_sexp() const {
  // auto v = tl::views::zip_transform(
  // [&](const auto &name, const auto &type) {
  // return fmt::format("{}: {}", name.lexeme, type);
  // },
  // this->params, this->param_types);

  std::vector<std::string> param_and_types;
  std::transform(this->params.begin(), this->params.end(),
                 this->param_types.begin(), std::back_inserter(param_and_types),
                 [](const auto &name, const auto &type) {
                   return fmt::format("{}: {}", name.lexeme, type);
                 });
  return fmt::format(
      "(Fn {} [{}] returns {} [ {} ] )", name.lexeme,
      fmt::join(param_and_types, ", "), return_type,
      fmt::join(this->body | std::views::transform([](const auto &stmt) {
                  return stmt->to_sexp();
                }) | tl::to<std::vector<std::string>>(),
                "; "));
}
vector<std::string> FnStmt::gen_intermediate() {
  vector<std::string> instrs;
  instrs.emplace_back(this->name.lexeme + ":");
  for (auto &s : this->body) {
    auto tmp = s->gen_intermediate();
    instrs.insert(instrs.end(), tmp.begin(), tmp.end());
  }
  return instrs;
}
vector<std::string> FnStmt::transpile_to_cpp() {

  // return {};

  auto parameters = fmt::format(
      "{}", fmt::join(this->params |
                          std::views::transform([&, i = 0](auto &tok) mutable {
                            return this->param_types[i++] + " " + tok.lexeme;
                          }) |
                          tl::to<std::vector<std::string>>(),
                      ","));
  auto fn_body = fmt::format(
      "{}", fmt::join(this->body | std::views::transform([](auto &s) {
                        return fmt::format(
                            "{}", fmt::join(s->transpile_to_cpp(), "\n"));
                      }) | tl::to<std::vector<std::string>>(),
                      "\n"));

  auto type = fmt::format("std::function<{}({})>", this->return_type,
                          fmt::join(param_types, ","));

  return {

      fmt::format("{} {} = [=, &{}]({})", type, this->name.lexeme,
                  this->name.lexeme, parameters),
      fmt::format("{{\n{}\n}};\n", fn_body)

  };
}

IfStmt::IfStmt(std::unique_ptr<Expr> condition,
               std::unique_ptr<Stmt> then_branch,
               std::optional<std::unique_ptr<Stmt>> else_branch)
    : condition(std::move(condition)), then_branch(std::move(then_branch)),
      else_branch(std::move(else_branch)) {}
string IfStmt::to_sexp() const {
  return fmt::format("(If {} then {} else {}))", condition->to_sexp(),
                     then_branch->to_sexp(),
                     else_branch.has_value() ? else_branch.value()->to_sexp()
                                             : std::string("{}"));
}
vector<std::string> IfStmt::gen_intermediate() {
  static int cnt = 0;
  std::vector<std::string> instrs;
  auto cond_code = this->condition->gen_intermediate();
  instrs.insert(instrs.end(), cond_code.begin(), cond_code.end());
  instrs.emplace_back(fmt::format(".IF{}:", cnt++));
  auto then_code = this->then_branch->gen_intermediate();
  auto else_code = this->else_branch.has_value()
                       ? this->else_branch.value()->gen_intermediate()
                       : std::vector<std::string>(0);

  instrs.emplace_back(fmt::format(
      "  Jz <IP+{}> // if false go to after then block", then_code.size() + 1));
  instrs.insert(instrs.end(), then_code.begin(), then_code.end());
  if (!else_code.empty()) {
    instrs.emplace_back(fmt::format("  Jmp <IP+{}>", else_code.size() + 1));
  }
  instrs.insert(instrs.end(), else_code.begin(), else_code.end());
  // instrs.emplace_back("  Jmp <IP+1>");
  return instrs;
}
vector<std::string> IfStmt::transpile_to_cpp() {
  return {fmt::format("if ({}) ", this->condition->transpile_to_cpp()),
          fmt::format("{}\n",
                      fmt::join(this->then_branch->transpile_to_cpp(), "\n")),
          fmt::format(
              "{}", else_branch.has_value()
                        ? fmt::format(
                              "else {}",
                              fmt::join(else_branch.value()->transpile_to_cpp(),
                                        "\n"))
                        : "")};
}

LetStmt::LetStmt(Token name, std::optional<Token> type,
                 std::unique_ptr<Expr> expr)
    : name(std::move(name)), type(std::move(type)),
      initializer_expr(std::move(expr)) {}
string LetStmt::to_sexp() const {
  return fmt::format("(Let {}{} {})", name.lexeme,
                     type.has_value() ? ":" + type.value().lexeme : "",
                     initializer_expr->to_sexp());
}
vector<std::string> LetStmt::gen_intermediate() {
  std::vector<std::string> instrs;
  // instrs.emplace_back("// pushing rhs to stack");
  auto rhs = this->initializer_expr->gen_intermediate();
  instrs.insert(instrs.end(), rhs.begin(), rhs.end());
  instrs.push_back(fmt::format("  Load {}", this->name.lexeme));
  return instrs;
}
vector<std::string> LetStmt::transpile_to_cpp() {
  return {fmt::format("{} {} = {};", this->type.value().lexeme,
                      this->name.lexeme,
                      this->initializer_expr->transpile_to_cpp())};
}

PrintStmt::PrintStmt(std::unique_ptr<Expr> expr, bool new_line)
    : expr(std::move(expr)), has_newline(new_line) {}
string PrintStmt::to_sexp() const {
  return fmt::format("(Print {})", expr->to_sexp());
}
vector<std::string> PrintStmt::gen_intermediate() {
  vector<std::string> instrs;
  // instrs.emplace_back("// Push the expression to print");
  auto exp_code = this->expr->gen_intermediate();
  instrs.insert(instrs.end(), instrs.begin(), instrs.end());
  instrs.emplace_back("  Write");
  if (has_newline) {
    instrs.emplace_back("  Push '\\n'");
    instrs.emplace_back("  Write");
  }
  return instrs;
}
vector<std::string> PrintStmt::transpile_to_cpp() {
  if (has_newline) {
    return {fmt::format("cout << {} <<endl ;", this->expr->transpile_to_cpp())};
  }
  return {fmt::format("cout<< {};", this->expr->transpile_to_cpp())};
}

ReturnStmt::ReturnStmt(Token keyword, std::optional<std::unique_ptr<Expr>> val)
    : keyword(std::move(keyword)), value(std::move(val)) {}
string ReturnStmt::to_sexp() const {
  return fmt::format("(Return {})", value.has_value() ? value.value()->to_sexp()
                                                      : std::string(""));
}
vector<std::string> ReturnStmt::gen_intermediate() { return {"  Ret"}; }
vector<std::string> ReturnStmt::transpile_to_cpp() {
  return {fmt::format(
      "return {};",
      this->value.has_value() ? this->value.value()->transpile_to_cpp() : "")};
}

WhileStmt::WhileStmt(std::unique_ptr<Expr> condition,
                     std::unique_ptr<Stmt> body,
                     std::optional<std::unique_ptr<Stmt>> change_fn)
    : condition(std::move(condition)), body(std::move(body)),
      change_fn(std::move(change_fn)) {}
[[nodiscard]] string WhileStmt::to_sexp() const {
  return fmt::format("(While cond {} do {})", condition->to_sexp(),
                     body->to_sexp());
}
vector<std::string> WhileStmt::gen_intermediate() {
  static int cnt = 0;
  vector<std::string> instrs;
  auto label = fmt::format("WHILE{}", cnt++);
  instrs.emplace_back(fmt::format(".{}:", label));
  // instrs.emplace_back("// Pushing condition");

  auto cond_code = this->condition->gen_intermediate();
  auto block = this->body->gen_intermediate();

  instrs.insert(instrs.end(), cond_code.begin(), cond_code.end());

  auto jnz = fmt::format("  Jnz <IP+2> // if true go to start of block");
  auto jmp = fmt::format("  Jmp <IP+{}> // if false go to end of block",
                         block.size() + 2);

  instrs.emplace_back(jnz);
  instrs.emplace_back(jmp);
  std::transform(block.begin(), block.end(), block.begin(),
                 [&, i = 0](string &code) mutable {
                   i++;
                   if (code == "  break") {
                     return fmt::format(
                         "  Jmp <IP+{}> // break by going to end of loop",
                         block.size() + 2 - i);
                   }
                   if (code == "  continue") {
                     return fmt::format("  Jmp {}", label);
                   }
                   return code;
                 });
  instrs.insert(instrs.end(), block.begin(), block.end());
  instrs.emplace_back(fmt::format("  Jmp {} // jump back to loop", label));
  return instrs;
}
vector<std::string> WhileStmt::transpile_to_cpp() {
  return {fmt::format("while({})", this->condition->transpile_to_cpp()),
          fmt::format("{}", fmt::join(this->body->transpile_to_cpp(), "\n"))};
}
