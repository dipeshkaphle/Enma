#include "Common.hpp"

std::string literal_to_string(const literal_type &lit) {
  using namespace std::string_literals;
  return std::visit(
      overloaded{[](int64_t x) { return std::to_string(x); },
                 [](const double &x) { return std::to_string(x); },
                 [](const char &c) { return std::string(1, c); },
                 [](const std::string &s) { return s; },
                 [](const bool &b) { return b ? "true"s : "false"s; },
                 [](auto) { return "_"s; }},
      lit);
  //
}
