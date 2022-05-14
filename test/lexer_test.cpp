#include "frontend/Lexer.hpp"

#include <catch2/catch.hpp>
#include <fmt/core.h>
#include <iterator>
#include <ranges>

#include <tl/to.hpp>

TEST_CASE("Lexer") {
  std::string source = R"(
	let x = 'c';
data Position = {
	x': int; // x' is also identifier
	y: int;
};
	)";
  Lexer lexer(source);

  const auto &all_toks = lexer.scan_tokens();
  using tok = TokenType;

  auto all_types = all_toks |
                   std::views::transform([](const Token &token) -> TokenType {
                     return token.type;
                   }) |
                   tl::to<std::vector<TokenType>>();

  REQUIRE(all_types == std::vector<TokenType>{
                           tok::LET,        tok::IDENTIFIER, tok::EQUAL,
                           tok::CHAR,       tok::SEMICOLON,  tok::DATA,
                           tok::IDENTIFIER, tok::EQUAL,      tok::LEFT_BRACE,
                           tok::IDENTIFIER, tok::COLON,      tok::IDENTIFIER,
                           tok::SEMICOLON,  tok::IDENTIFIER, tok::COLON,
                           tok::IDENTIFIER, tok::SEMICOLON,  tok::RIGHT_BRACE,
                           tok::SEMICOLON,  tok::ENDOFFILE});
}
