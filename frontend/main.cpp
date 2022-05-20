#include "Error.hpp"
#include "Lexer.hpp"
#include "Parser.hpp"
#include "SemanticCheck.hpp"
#include "Statements.hpp"

#include <cstdlib>
#include <deque>
#include <fmt/core.h>
#include <fmt/ostream.h>
#include <fstream>
#include <iostream>
#include <iterator>

using std::string;
using std::string_view;
using namespace std;

void run(const string &source, [[maybe_unused]] bool is_repl = false) {
  Lexer scanner(source);
  std::vector<Token> tokens = scanner.scan_tokens();
  if (Error::hadError) {
    return;
  }

#ifdef DEBUG
  fmt::print("=======================================\n");
  fmt::print("All the Tokens\n");
  fmt::print("=======================================\n");
  fmt::print("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n");
  std::ranges::for_each(tokens,
                        [](auto &x) { fmt::print("{}\n", x.to_string()); });
  fmt::print("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n");
  fmt::print("=======================================\n");
#endif

  Parser parser(tokens);
  auto stmnts = parser.parse();
  if (Error::hadError) {
    return;
  }

#ifdef DEBUG
  fmt::print("=======================================\n");
  fmt::print("S-Expression\n");
  fmt::print("=======================================\n");
  std::ranges::for_each(stmnts, [](auto &stmnt) {
    fmt::print("{}\n\n", stmnt.value()->to_sexp());
  });
#endif

  std::deque<stmt_ptr> stmts_without_err;

  std::transform(std::make_move_iterator(stmnts.begin()),
                 std::make_move_iterator(stmnts.end()),
                 std::back_inserter(stmts_without_err),
                 [&](auto &&maybe_stmt) -> stmt_ptr {
                   return std::move(maybe_stmt.value());
                 });

  SemanticChecker checker(
      std::vector<stmt_ptr>(std::make_move_iterator(stmts_without_err.begin()),
                            std::make_move_iterator(stmts_without_err.end())));
  auto errs = checker.infer_type_type_check_and_undeclared_symbols_check();
  bool has_error = false;

  for (auto &err : errs) {
    if (!has_error) {
      fmt::print("=======================================\n");
      fmt::print("Semantic Errors \n");
      fmt::print("=======================================\n");
    }
    fmt::print(std::cerr, "{}\n", err.what());
    has_error = true;
  }
  if (has_error) {
    return;
  }
  // auto all_statements = std::move(checker.stmts);
  // std::vector<std::string> instrs;
  // for (auto &stmt : all_statements) {
  // auto tmp = stmt->gen_intermediate();
  // instrs.insert(instrs.end(), tmp.begin(), tmp.end());
  // }
  // fmt::print("=======================================\n");
  // fmt::print("Intermediate Code\n");
  // fmt::print("=======================================\n");
  // for (auto &s : instrs)
  // fmt::print("{}\n", s);

  auto all_statements = std::move(checker.stmts);
  std::vector<Instr> instrs;
  SymTable symtable;
  for (auto &stmt : all_statements) {
    auto codegen = stmt->gen_bytecode(symtable);
    instrs.insert(instrs.end(), codegen.begin(), codegen.end());
  }
  ofstream bytecode_out("out.bytecode");
  for (auto &instr : instrs) {
    fmt::print(bytecode_out, "{}\n", to_string(instr));
  }

  ofstream f("out.cpp");

  fmt::print(f, "#include <string>\n");
  fmt::print(f, "#include <iostream>\n");
  fmt::print(f, "#include <functional>\n");
  fmt::print(f, "using namespace std;\n");
  fmt::print(f, "int main(){{\n");
  for (auto &s : all_statements) {
    fmt::print(f, "{}\n", fmt::join(s->transpile_to_cpp(), "\n"));
  }
  fmt::print(f, "}}\n");
  f.close();

  system("clang-format -i out.cpp");
}

void runFile(const char *filename) {
  std::ifstream inp(filename);

  if (inp.is_open()) {
    string all_code((std::istreambuf_iterator<char>(inp)),
                    (std::istreambuf_iterator<char>()));
    try {
      run(all_code, false);
    } catch (std::exception &e) {
      fmt::print(std::cerr, "Exception: {}", e.what());
    }
    if (Error::hadError) {
      exit(20);
    }
  } else {
    throw std::runtime_error("Couldnt open the file " + string(filename));
  }
}

void runPrompt() {
  Error::hadError = false;
  string line;
  while (true) {
    fmt::print(">>> ");
    // cout << ">>> ";
    // cout.flush();
    std::getline(std::cin, line);
    if (line.empty()) {
      break;
    }
    try {
      run(line, true);
    } catch (std::exception &e) {
      cerr << "Exception: " << e.what() << '\n';
    }
    Error::hadError = false;
  }
}

int main(int argc, char **argv) {

  // system("pwd");
  // runFile("../../examples/fib.enma");
  if (argc > 2) {
    fmt::print("Usage: enma-frontend [script]");
    exit(255);
  } else if (argc == 2) {
    runFile(argv[1]);
  } else {
    runPrompt();
  }
}
