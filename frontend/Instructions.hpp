#pragma once

#include "Common.hpp"

#include <variant>
#include <vector>

template <typename T> struct Push { T val; };
template <typename T> struct Load {
  T val;
  std::vector<literal_type>::iterator dest;
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
struct Ret {};
struct Call {};
struct Jnz {};
struct Jmp {};
struct Lbl {
	std::string label_name;
};


using Instr = std::variant<
 Push<int64_t>,
 Push<double>,
 Push<bool>,
 Push<char>,
 Push<std::string>, 
 Load<int64_t>, 
 Load<double>,
 Load<bool>, 
 Load<char>, 
 Load<std::string>,
 Add<int64_t>, 
 Add<double>,
 Add<std::string>, 
 Add<char>, 
 Sub<int64_t>, 
 Sub<double>,
 Sub<char>, 
 Mul<int64_t>, 
 Mul<double>,
 Div<int64_t>, 
 Div<double>,
 Eq<int64_t>, 
 Eq<double>,
 Eq<bool>, 
 Eq<char>, 
 Eq<std::string>,
 Neq<int64_t>, 
 Neq<double>,
 Neq<bool>, 
 Neq<char>, 
 Neq<std::string>,
 Gt<int64_t>,
 Gt<double>,
 Gt<std::string>,
 Gt<char>,
 Ge<int64_t>,
 Ge<double>,
 Ge<std::string>,
 Ge<char>,
 Lt<int64_t>,
 Lt<double>,
 Lt<std::string>,
 Lt<char>,
 Le<int64_t>,
 Le<double>,
 Le<std::string>,
 Le<char>,
 Not,
 And,
 Or,
 Print,
 Ret,
 Call,
 Jmp,
 Jnz,
 Lbl
 >;
