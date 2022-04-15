#include "Environment.hpp"

#include <fmt/format.h>
#include <tl/expected.hpp>

#include <ranges>

value_or_err Environment::get(const Token &name) {
  for (map_type &table : this->sym_table | std::ranges::views::reverse) {
    if (table.contains(name.lexeme)) {
      return table.at(name.lexeme);
    }
  }
  return tl::make_unexpected<EnmaRuntimeError>(EnmaRuntimeError(
      name, fmt::format("Undefined variable: {}. ", name.lexeme).c_str()));
}

void Environment::define(const std::string &name, value_type val) {
  this->sym_table.back()[name] = std::move(val);
}

value_or_err Environment::assign(const Token &name, value_type val) {
  for (auto &table : this->sym_table | std::ranges::views::reverse) {
    if (table.contains(name.lexeme)) {
      table[name.lexeme] = std::move(val);
      return table[name.lexeme];
    }
  }
  return tl::make_unexpected<EnmaRuntimeError>(EnmaRuntimeError(
      name, fmt::format("Undefined variable: {}. ", name.lexeme).c_str()));
}

void Environment::push_frame() { this->sym_table.emplace_back(); }
void Environment::pop_frame() { this->sym_table.pop_back(); }
