#include "frontend/Lexer.hpp"

#include <catch2/catch.hpp>
#include <fmt/core.h>
#include <iterator>
#include <ranges>

TEST_CASE("Lexer") {
  std::string source = R"(
data Position = {
	x: int;
	y: int;
};
	)";
  Lexer lexer(source);

  const auto &all_toks = lexer.scan_tokens();
  using tok = TokenType;

  REQUIRE(ranges::equal(
      all_toks | views::transform(
                     [](const Token &tok) -> TokenType { return tok.type; }),
      std::vector<TokenType>{
          tok::DATA, tok::IDENTIFIER, tok::EQUAL, tok::LEFT_BRACE,
          tok::IDENTIFIER, tok::COLON, tok::IDENTIFIER, tok::SEMICOLON,
          tok::IDENTIFIER, tok::COLON, tok::IDENTIFIER, tok::SEMICOLON,
          tok::RIGHT_BRACE, tok::SEMICOLON, tok::ENDOFFILE}));
}
