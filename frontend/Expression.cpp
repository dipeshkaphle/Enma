#include "Expression.hpp"
#include "Common.hpp"
#include "SymbolTable.hpp"

#include <fmt/format.h>
#include <tl/to.hpp>

#include <iterator>
#include <ranges>

using namespace expr;
using symbol_table = std::vector<std::vector<symbol_type>>;

std::string literal_as_literal_string(const literal_type &lit) {
  return std::visit(
      overloaded{[](const std::string &x) { return fmt::format("\"{}\"", x); },
                 [](const char x) { return fmt::format("\'{}\'", x); },
                 [](const auto &x) { return literal_to_string(x); }},
      lit);
}

Expr::~Expr() = default;

BinaryExpr::BinaryExpr(Token op, std::unique_ptr<Expr> left,
                       std::unique_ptr<Expr> right)
    : op(std::move(op)), left(std::move(left)), right(std::move(right)) {}
string BinaryExpr::to_sexp() const {
  return fmt::format("({} {} {})", op.lexeme, left->to_sexp(),
                     right->to_sexp());
}
std::vector<std::string> BinaryExpr::gen_intermediate() {
  vector<std::string> instrs;
  auto lhs = this->left->gen_intermediate();
  auto rhs = this->right->gen_intermediate();
  // instrs.emplace_back("// Codegen for lhs");
  instrs.insert(instrs.end(), begin(lhs), end(lhs));
  // instrs.emplace_back("// Codegen for rhs");
  instrs.insert(instrs.end(), begin(rhs), end(rhs));
  instrs.emplace_back(
      fmt::format("  Op {}  // Pops top 2, performs op and pushes result",
                  this->op.lexeme));
  return instrs;
}
std::string BinaryExpr::transpile_to_cpp() {
  return this->left->transpile_to_cpp() + " " + this->op.lexeme + " " +
         this->right->transpile_to_cpp();
}
std::vector<Instr> BinaryExpr::gen_bytecode(SymTable &symtable) {
  std::vector<Instr> instrs;
  auto lhs = this->left->gen_bytecode(symtable);
  auto rhs = this->right->gen_bytecode(symtable);
  auto op_type = symtable.get_expr_type(this->left.get()).value();
  instrs.insert(instrs.end(), lhs.begin(), lhs.end());
  instrs.insert(instrs.end(), rhs.begin(), rhs.end());
  instrs.emplace_back(BinOp{.op = this->op.lexeme});
  return instrs;
}

PrefixExpr::PrefixExpr(Token op, std::unique_ptr<Expr> right)
    : op(std::move(op)), right(std::move(right)) {}

string PrefixExpr::to_sexp() const {
  return fmt::format("({} {})", op.lexeme, this->right->to_sexp());
}
std::vector<std::string> PrefixExpr::gen_intermediate() {
  auto rhs = this->right->gen_intermediate();
  vector<std::string> instrs;
  // instrs.emplace_back("// Pushing the operand to stack");
  instrs.insert(instrs.end(), begin(rhs), end(rhs));
  instrs.emplace_back(fmt::format(
      "  PrefixOp {}  //Pops one from stack, does op and pushes it back",
      this->op.lexeme));
  return instrs;
}
std::string PrefixExpr::transpile_to_cpp() {
  return "(" + this->op.lexeme + this->right->transpile_to_cpp() + ")";
}
std::vector<Instr> PrefixExpr::gen_bytecode(SymTable &symtable) {
  std::vector<Instr> instrs;
  auto rhs = this->right->gen_bytecode(symtable);
  instrs.insert(instrs.end(), rhs.begin(), rhs.end());
  instrs.emplace_back(PrefixOp{.op = this->op.lexeme});
  return instrs;
}

AttrAccessExpr::AttrAccessExpr(Token op, std::unique_ptr<Expr> left,
                               std::unique_ptr<Expr> right)
    : op(op), left(std::move(left)), right(std::move(right)) {}
string AttrAccessExpr::to_sexp() const {
  return fmt::format("(. {} {})", left->to_sexp(), right->to_sexp());
}
// TODO
std::vector<std::string> AttrAccessExpr::gen_intermediate() { return {}; }
std::string AttrAccessExpr::transpile_to_cpp() { return ""; }
std::vector<Instr>
AttrAccessExpr::gen_bytecode([[maybe_unused]] SymTable &symtable) {
  return {};
}

VarExpr::VarExpr(Token name) : name(std::move(name)) {}

string VarExpr::to_sexp() const { return fmt::format("{}", this->name.lexeme); }
std::vector<std::string> VarExpr::gen_intermediate() {
  return {fmt::format("  Push {}  //Push variable", this->name.lexeme)};
}
std::string VarExpr::transpile_to_cpp() { return this->name.lexeme; }
std::vector<Instr> VarExpr::gen_bytecode(SymTable &symtable) {
  return {Ref{.offset = symtable.get_symbol_offset(this->name.lexeme).value()}};
}

LiteralExpr::LiteralExpr(literal_type val) : value(std::move(val)) {}

string LiteralExpr::to_sexp() const {
  return fmt::format("{}", literal_to_string(this->value));
}
std::vector<std::string> LiteralExpr::gen_intermediate() {

  return {fmt::format("  PushI {} //Push intermediate",
                      literal_as_literal_string(this->value))};
}
std::string LiteralExpr::transpile_to_cpp() {
  return literal_as_literal_string(this->value);
}
std::vector<Instr>
LiteralExpr::gen_bytecode([[maybe_unused]] SymTable &symtable) {

  // using literal_type =
  // std::variant<std::monostate, int64_t, double, bool, char, std::string>;
  auto instr = std::visit(
      overloaded{
          [&](int64_t &x) -> Instr { return Instr(Push<int64_t>{.val = x}); },
          [&](double &x) -> Instr { return Instr(Push<double>{.val = x}); },
          [&](bool x) -> Instr { return Instr(Push<bool>{.val = x}); },
          [&](char x) -> Instr { return Instr(Push<char>{.val = x}); },
          [&](std::string &x) -> Instr {
            return Instr(Push<std::string>{.val = std::move(x)});
          },
          [&](auto) {
            throw std::runtime_error("Shouldn't be here");
            return Instr(Push<int64_t>{.val = -1});
          }},
      this->value);
  return {instr};
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
std::vector<std::string> CallExpr::gen_intermediate() {
  vector<std::string> instrs;
  // instrs.emplace_back("// Filling up parameters in stack");
  for (auto &s : this->arguments) {
    auto tmp = s->gen_intermediate();
    instrs.insert(instrs.end(), tmp.begin(), tmp.end());
  }
  // instrs.emplace_back("//Calling function");
  if (auto *var = dynamic_cast<VarExpr *>(this->callee.get()); var != nullptr) {
    instrs.emplace_back(fmt::format("  Call {}", var->name.lexeme));
  } else {
    instrs.emplace_back(fmt::format("  Call <lambda>"));
  }
  return instrs;
}
std::string CallExpr::transpile_to_cpp() {
  return fmt::format(
      "{}({})", this->callee->transpile_to_cpp(),
      fmt::join(this->arguments | std::views::transform([](auto &s) {
                  return s->transpile_to_cpp();
                }),
                ","));
}
std::vector<Instr> CallExpr::gen_bytecode(SymTable &symtable) {
  // symtable.back().push
  std::vector<Instr> instrs;
  for (auto &arg : this->arguments) {
    auto arg_bytecode = arg->gen_bytecode(symtable);
    instrs.insert(instrs.end(), arg_bytecode.begin(), arg_bytecode.end());
  }
  // TODO : lambda expression if they're added
  auto fn = dynamic_cast<VarExpr *>(this->callee.get());
  instrs.emplace_back(Call{.label = fmt::format("\"{}\"", fn->name.lexeme)});
  return instrs;
}

AssignExpr::AssignExpr(Token name, std::unique_ptr<Expr> val)
    : name(std::move(name)), value(std::move(val)) {}
string AssignExpr::to_sexp() const {
  return fmt::format("(= {} {})", name.lexeme, value->to_sexp());
}
std::vector<std::string> AssignExpr::gen_intermediate() {
  vector<std::string> instrs;
  // instrs.emplace_back("// Pushing the value to stack");
  auto tmp = this->value->gen_intermediate();
  instrs.insert(instrs.end(), tmp.begin(), tmp.end());
  // instrs.emplace_back("// Load to the variable");
  instrs.push_back(fmt::format("  Load {}", this->name.lexeme));
  return instrs;
}
std::string AssignExpr::transpile_to_cpp() {
  return this->name.lexeme + " = " + this->value->transpile_to_cpp();
}
std::vector<Instr> AssignExpr::gen_bytecode(SymTable &symtable) {
  std::vector<Instr> instrs;
  auto rhs = this->value->gen_bytecode(symtable);
  instrs.insert(instrs.end(), rhs.begin(), rhs.end());
  instrs.emplace_back(
      Load{.offset = symtable.get_symbol_offset(this->name.lexeme).value()});
  return instrs;
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
std::vector<std::string> ConditionalExpr::gen_intermediate() {
  vector<std::string> instrs;
  // instrs.emplace_back("// Push the condition to stack");
  auto cond_code = cond->gen_intermediate();
  instrs.insert(instrs.end(), cond_code.begin(), cond_code.end());
  // instrs.emplace_back("// Push Then Branch to stack");
  auto then_code = this->then_expr->gen_intermediate();
  instrs.insert(instrs.end(), then_code.begin(), then_code.end());
  // instrs.emplace_back("// Push Else Branch to stack");
  auto else_code = this->else_expr->gen_intermediate();
  instrs.insert(instrs.end(), else_code.begin(), else_code.end());
  instrs.emplace_back("  IF // Pops 3 things from stack, checks cond and "
                      "pushes one of the branches based on cond");
  return instrs;
}
std::string ConditionalExpr::transpile_to_cpp() {
  return fmt::format("if ({}) {{ {} }} else {{ {} }} ",
                     cond->transpile_to_cpp(), then_expr->transpile_to_cpp(),
                     else_expr->transpile_to_cpp());
}
std::vector<Instr> ConditionalExpr::gen_bytecode(SymTable &symtable) {
  std::vector<Instr> instrs;
  auto cond_code = cond->gen_bytecode(symtable);
  instrs.insert(instrs.end(), cond_code.begin(), cond_code.end());
  auto then_code = this->then_expr->gen_bytecode(symtable);
  auto else_code = this->else_expr->gen_bytecode(symtable);
  instrs.emplace_back(Jnz{.offset = (int64_t)else_code.size() + 1});
  instrs.insert(instrs.end(), else_code.begin(), else_code.end());
  instrs.emplace_back(Jmp{.offset = (int64_t)then_code.size()});
  instrs.insert(instrs.end(), then_code.begin(), then_code.end());
  return instrs;
}
