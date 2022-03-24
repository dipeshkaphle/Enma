#include "frontend/Expression.hpp"
#include "frontend/Lexer.hpp"
#include "frontend/Parser.hpp"

#include <catch2/catch.hpp>
#include <fmt/core.h>

#include <string>

using namespace std::string_literals;
#define PARSE(s) Parser(Lexer(s).scan_tokens()).parse()
#define PARSE_AND_SEXP(s) PARSE(s).value()->to_string()

TEST_CASE("Pratt Parser Test") {
  REQUIRE(PARSE_AND_SEXP("1+2") == "(+ 1 2)");
  REQUIRE(PARSE_AND_SEXP("1*2") == "(* 1 2)");
  REQUIRE(PARSE_AND_SEXP("1+2*3+10") == "(+ (+ 1 (* 2 3)) 10)");
  REQUIRE(PARSE_AND_SEXP("2+-3*-10") == "(+ 2 (* (- 3) (- 10)))");
  REQUIRE(PARSE_AND_SEXP("f.g.h") == "(. f (. g h))");
  REQUIRE(PARSE_AND_SEXP("-f.g") == "(- (. f g))");
  REQUIRE(PARSE_AND_SEXP("1+2+3+4") == "(+ (+ (+ 1 2) 3) 4)");
  REQUIRE(PARSE_AND_SEXP("(((0)))") == "0");
  REQUIRE(PARSE_AND_SEXP("1+(2+(3+4))") == "(+ 1 (+ 2 (+ 3 4)))");
  REQUIRE(PARSE_AND_SEXP("f(g(1), 2)") == "(call f [(call g [1]), 2])");
  REQUIRE(PARSE_AND_SEXP("f(1) + g(1,2)") ==
          "(+ (call f [1]) (call g [1, 2]))");
  REQUIRE(PARSE_AND_SEXP("1<2 ==  2<=3") == "(== (< 1 2) (<= 2 3))");
  REQUIRE(PARSE_AND_SEXP("x=y=1") == "(= x (= y 1))");
  REQUIRE(!PARSE("1=2").has_value());
  REQUIRE(PARSE_AND_SEXP("2 if (1==2) else 3") == "(if (== 1 2) 2 3)");
  REQUIRE(PARSE_AND_SEXP("2 if 1==2 else 3") == "(if (== 1 2) 2 3)");
}
