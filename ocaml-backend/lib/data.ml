type data = 
  | I of int64
  | D of float 
  | B of bool
  | C of char
  | S of string
  | Fn of string
  | Err of string
  [@@deriving show]

let string_of_data = function
  | I i -> Int64.to_string i
  | D d -> Float.to_string d
  | B b -> Bool.to_string b
  | C c -> String.make 1 c
  | S s -> s
  | Fn s -> String.concat " " ["Fn<";s ;">"] 
  | Err s ->  s

