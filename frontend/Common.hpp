#pragma once
#include <string>
#include <variant>

// Our language has 5 literal types
using literal_type =
    std::variant<std::monostate, int64_t, double, bool, char, std::string>;
