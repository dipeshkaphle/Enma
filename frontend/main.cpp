#include "Error.hpp"
#include "Lexer.hpp"
#include "Parser.hpp"
#include "Statements.hpp"

#include <fmt/core.h>
#include <fmt/ostream.h>
#include <fstream>
#include <iostream>

using std::string;
using std::string_view;
using namespace std;

void run(const string &source, [[maybe_unused]] bool is_repl = false) {
  Lexer scanner(source);
  std::vector<Token> tokens = scanner.scan_tokens();
  if (Error::hadError) {
    return;
  }

  // #ifdef DEBUG
  fmt::print("=======================================\n");
  fmt::print("All the Tokens\n");
  fmt::print("=======================================\n");
  fmt::print("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n");
  std::ranges::for_each(tokens,
                        [](auto &x) { fmt::print("{}\n", x.to_string()); });
  fmt::print("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n");
  fmt::print("=======================================\n");
  // #endif

  Parser parser(tokens);
  auto stmnts = parser.parse();
  std::ranges::for_each(stmnts, [](auto &stmnt) {
    if(stmnt.has_value())
      fmt::print("{}\n\n",stmnt.value()->to_sexp());
    else
      throw stmnt.error();
  });
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

  if (argc > 2) {
    fmt::print("Usage: enma-frontend [script]");
    exit(255);
  } else if (argc == 2) {
    runFile(argv[1]);
  } else {
    runPrompt();
  }
}
