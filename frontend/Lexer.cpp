#include "Lexer.hpp"
#include "Error.hpp"

#include <fmt/ostream.h>

#include <iostream>

using tok = TokenType;

const unordered_map<string, TokenType> Lexer::keywords = {
    {"and", tok::AND},
    {"or", tok::OR},
    {"data", tok::DATA},
    {"else", tok::ELSE},
    {"false", tok::FALSE},
    {"true", tok::TRUE},
    {"for", tok::FOR},
    {"fn", tok::FN},
    {"if", tok::IF},
    {"nil", tok::NIL},
    {"println", tok::PRINTLN},
    {"print", tok::PRINT},
    {"return", tok::RETURN},
    {"let", tok::LET},
    {"while", tok::WHILE},
    {"break", tok::BREAK},
    {"continue", tok::CONTINUE}};

Lexer::Lexer(string source) : source(std::move(source)) {}

char Lexer::peek() {
  if (is_at_end()) {
    return '\0';
  }
  return source[this->cur];
}

char Lexer::peek_next() {
  if (this->cur + 1 >= source.size()) {
    return '\0';
  }
  return source[this->cur + 1];
}

bool Lexer::match(char expected) {
  if (is_at_end()) {
    return false;
  }
  if (source[cur] != expected) {
    return false;
  }
  this->cur++;
  return true;
}

void Lexer::add_token(TokenType type, const literal_type &literal) {
  string text = source.substr(start, cur - start);
  tokens.emplace_back(Token(type, text, literal, cur_line));
}

std::string Lexer::parse_with_escapes(string_view s) const {
  // https://en.cppreference.com/w/cpp/language/escape
  std::unordered_map<char, char> valid_escapes{
      {'\'', 0x27}, {'"', 0x22}, {'?', 0x3f}, {'\\', 0x5c},
      {'a', 0x07},  {'b', 0x08}, {'f', 0x0c}, {'n', 0x0a},
      {'r', 0x0d},  {'t', 0x09}, {'v', 0x0b}};

  auto it = s.begin();
  string result;
  bool escape = false;
  while (it != s.end()) {
    if (!escape && *it == '\\') {
      escape = true;
    } else {
      if (escape) {
        char c = *it;
        if (valid_escapes.contains(c)) {
          result.push_back(valid_escapes[c]);
        } else {
          result.push_back(c);
        }
        escape = false;
      } else {
        result.push_back(*it);
      }
    }
    it++;
  }
  return result;
}

void Lexer::get_string(char closing) {
  while (!is_at_end() && peek() != closing) {
    if (peek() == '\n') {
      cur_line++;
    }
    if (peek() == '\\') {
      advance();
    }
    advance();
  }
  if (is_at_end()) {
    Error::error(cur_line, "Unterminated string");
    return;
  }

  /*
   * was a valid string , so we advance further
   */
  advance();

  /*
   * source: "abc"x
   * start : 0
   * cur : 5
   * we want 1 to  3
   * so we do substr(start+1, (cur-2)-(start+1) + 1)
   */
  string sv = source;
  string str = parse_with_escapes(
      sv.substr(start + 1, (this->cur - 2) - (this->start + 1) + 1));
  add_token(TokenType::STRING, str);
}

void Lexer::get_char() {
  Lexer::get_string();
  auto last_tok = tokens.back();
  tokens.pop_back();
  if (std::holds_alternative<std::string>(last_tok.literal)) {
    add_token(TokenType::CHAR, std::get<std::string>(last_tok.literal)[0]);
  } else {
    // Can't get here
    fmt::print(std::cerr, "Shouldnt be here!!!!");
    exit(69);
  }
  // add_token(TokenType::CHAR, );
}

void Lexer::get_number() {
  /*
   * consume all the digits
   */
  while (isdigit(peek()) != 0) {
    advance();
  }

  /*
   * means we've encountered a double
   */
  bool is_double = false;
  if (peek() == '.' && (isdigit(peek_next()) != 0)) {
    is_double = true;
    advance();
    while (isdigit(peek()) != 0) {
      advance();
    }
  }

  /*
   * source: 123.56+123
   * start : 0
   * cur: 6
   * wanted : 123.56
   * source.substr(0,cur-1 - start +1 )
   */
  string number = source.substr(start, (cur - 1) - start + 1);
  add_token(is_double ? TokenType::DOUBLE : TokenType::INT,
            is_double ? literal_type(std::stod(number))
                      : literal_type(std::stoll(number)));
}

void Lexer::get_identifier() {
  while ((isalpha(peek()) != 0) || (isdigit(peek()) != 0) || peek() == '_') {
    advance();
  }

  /*
   * abc = 2;
   * start =0
   * cur= 3
   *
   */
  string ident = source.substr(start, this->cur - this->start);
  literal_type lit = ident;
  TokenType type = tok::IDENTIFIER;
  if (keywords.contains(ident)) {
    type = keywords.at(ident);
    if (type == tok::TRUE || type == tok::FALSE) {
      lit = type == tok::TRUE;
    }
  }
  add_token(type, lit);
}

void Lexer::multiline_comment() {
  vector<string> st;
  st.emplace_back("/*");
  while (!st.empty()) {
    if (is_at_end()) {
      Error::error(cur_line, "Unclosed multi line comment");
      return;
    }
    if (peek() == '/') {
      advance();
      if (!is_at_end() && peek() == '*') {
        // another multi line comment starts
        advance();
        st.emplace_back("/*");

      } else if (!is_at_end()) {
        advance();
      }

    } else if (peek() == '*') {
      advance();
      if (!is_at_end() && peek() == '/') {
        // a multi line comment closes
        advance();
        st.pop_back();
      }
    } else if (peek() == '\n') {
      advance();
      this->cur_line++;
    } else {
      advance();
    }
  }
}

vector<Token> Lexer::scan_tokens() {
  while (!is_at_end()) {
    start = cur;
    scan_token();
  }
  tokens.emplace_back(Token(TokenType::ENDOFFILE, "", "", cur_line));
  return tokens;
}

void Lexer::scan_token() {
  char c = this->advance();
  using tok = TokenType;
  switch (c) {
  case ' ':
  case '\r':
  case '\t':
    // Ignore whitespace.
    break;

  case '\n':
    this->cur_line++;
    break;
  case '(':
    add_token(tok::LEFT_PAREN);
    break;
  case ')':
    add_token(tok::RIGHT_PAREN);
    break;
  case '{':
    add_token(tok::LEFT_BRACE);
    break;
  case '}':
    add_token(tok::RIGHT_BRACE);
    break;
  case ',':
    add_token(tok::COMMA);
    break;
  case '.':
    add_token(tok::DOT);
    break;
  case '-':
    add_token(tok::MINUS);
    break;
  case '+':
    add_token(tok::PLUS);
    break;
  case ':':
    add_token(tok::COLON);
    break;
  case ';':
    add_token(tok::SEMICOLON);
    break;
  case '*':
    if (!is_at_end() && peek() == '/') {
      advance(), Error::error(cur_line, "Invalid token */");
      break;
    }
    add_token(tok::STAR);
    break;
  case '!':
    add_token(match('=') ? tok::BANG_EQUAL : tok::BANG);
    break;
  case '=':
    add_token(match('=') ? tok::EQUAL_EQUAL : tok::EQUAL);
    break;
  case '<':
    add_token(match('=') ? tok::LESS_EQUAL : tok::LESS);
    break;
  case '>':
    add_token(match('=') ? tok::GREATER_EQUAL : tok::GREATER);
    break;
  case '/': // multi line nested comments
    if (match('/')) {
      // A comment goes until the end of the line.
      while (!is_at_end() && peek() != '\n') {
        advance();
      }
    } else if (match('*')) {
      multiline_comment();
    } else {
      add_token(tok::SLASH);
    }
    break;
  case '"':
    get_string();
    break;
  case '\'':
    get_char();
    break;
  default:
    if (isdigit(c) != 0) {
      get_number();
    } else if ((isalpha(c) != 0) || c == '_') {
      get_identifier();
    } else {
      Error::error(cur_line, "Unexpected character.");
    }
    break;
  }
}
