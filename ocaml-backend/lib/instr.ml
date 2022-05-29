type instr =
  | PushI of int64
  | PushD of float
  | PushB of bool
  | PushC of char
  | PushS of string
  | PushFn of string
  | Load of int64
  | Ref of int64
  | BinOp of string
  | PrefixOp  of string
  | Print
  | Ret
  | Call of string * int
  | Jmp of int64
  | Jnz of int64
  | Label of string
  | Nop
  [@@deriving show] ;;


let split_at_first_space s =  let l = String.split_on_char ' ' s in (List.hd l, String.concat " " ( List.tl l ))

let parse_instr ln =
  match split_at_first_space ln  with
    | ("PushI", v) -> PushI (Int64.of_string v)
    | ("PushD", v)  -> PushD (Float.of_string v)
    | ("PushB", v) ->  PushB (if v="True" then true else false)
    | ("PushC", v)  -> let s = (Scanf.unescaped (String.sub v 1 (( String.length v ) - 2))) in PushC (s.[0])
    | ("PushS", v) -> PushS (Scanf.unescaped (String.sub v 1 ((String.length v) - 2)))
    | ("PushFn", v) -> PushFn (Scanf.unescaped (String.sub v 1 ((String.length v) - 2)))
    | ("Load",v ) -> Load (Int64.of_string  v)
    | ("Ref",v) -> Ref (Int64.of_string v)
    | ("BinOp",v) -> BinOp (Scanf.unescaped (String.sub v 1 ((String.length v) - 2)))
    | ("PrefixOp",v) -> PrefixOp (Scanf.unescaped (String.sub v 1 ((String.length v) - 2)))
    | ("Print",_)  -> Print
    | ("Ret",_) -> Ret
    | ("Call", v) -> 
        let x = String.split_on_char ' ' v in
        let label = List.hd x in
        let args_cnt = x |> List.tl |> List.hd in
        let trimmed_label = String.sub label 1 ((String.length label) -  2) in
        Call (trimmed_label, int_of_string args_cnt)
    | ("Jmp", v) -> Jmp (Int64.of_string v)
    | ("Jnz",v) -> Jnz (Int64.of_string  v)
    | ("Label",v) -> Label (Scanf.unescaped (String.sub v 1 ((String.length v) - 2)))
    | (_,_) ->  Nop

