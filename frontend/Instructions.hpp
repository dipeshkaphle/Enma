#pragma once

#include "Common.hpp"
#include "fmt/core.h"

#include <variant>
#include <vector>

template <typename T> struct Push { T val; };
struct Load {
  int64_t offset;
};
struct Ref {
  int64_t offset;
};
template <typename T> struct Add { using type = T; };
template <typename T> struct Sub { using type = T; };
template <typename T> struct Mul { using type = T; };
template <typename T> struct Div { using type = T; };
template <typename T> struct Eq { using type = T; };
template <typename T> struct Neq { using type = T; };
template <typename T> struct Gt { using type = T; };
template <typename T> struct Ge { using type = T; };
template <typename T> struct Lt { using type = T; };
template <typename T> struct Le { using type = T; };
struct Not {};
struct And {};
struct Or {};
struct Print {};
struct Ret {
  bool pop;
};
struct Jnz {
  int64_t offset;
};
struct Jmp {
  int64_t offset;
};
struct Lbl {
  std::string label_name;
};
struct Call {
  std::string label;
  int arg_cnt;
};
struct BinOp {
  std::string op;
};
struct PrefixOp {
  std::string op;
};
struct Continue {};
struct Break {};

using Instr = std::variant<Push<int64_t>, Push<double>, Push<bool>, Push<char>,
                           Push<std::string>, Load, Ref, BinOp, PrefixOp, Print,
                           Ret, Call, Jmp, Jnz, Lbl, Break, Continue>;

std::string to_string(Instr &instr);
