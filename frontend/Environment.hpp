#pragma once

#include "Common.hpp"
#include "EnmaRuntimeError.hpp"
#include "Token.hpp"

#include <tl/expected.hpp>

#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

using namespace tl;

using value_type = visit_type;

using value_or_err =
    tl::expected<std::reference_wrapper<value_type>, EnmaRuntimeError>;

using map_type = std::unordered_map<std::string, value_type>;

class Environment {
private:
  std::vector<map_type> sym_table;

public:
  Environment() : sym_table(1, map_type()) {}
  [[nodiscard]] value_or_err get(const Token &name);
  void define(const std::string &name, value_type val);
  void push_frame();
  void pop_frame();
  value_or_err assign(const Token &name, value_type val);
};
