#pragma once

#include <string>
#include <variant>

// helper type for the visitor #4
template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
// explicit deduction guide (not needed as of C++20)
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

// Our language has 5 literal types
using literal_type =
    std::variant<std::monostate, int64_t, double, bool, char, std::string>;

std::string literal_to_string(const literal_type &lit);

using visit_type =
    std::variant<std::monostate, int64_t, double, bool, char, std::string>;
