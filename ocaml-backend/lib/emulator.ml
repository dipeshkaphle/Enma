open Data;;
open Instr;;
open Vector;;
type out_and_state = data option * State.machine_state


(* type instr = *)
  (* | PushI of int64 *)
  (* | PushD of float *)
  (* | PushB of bool *)
  (* | PushC of char *)
  (* | PushS of string *)
  (* | PushFn of string *)
  (* | Load of int64 *)
  (* | Ref of int64 *)
  (* | BinOp of string *)
  (* | PrefixOp  of string *)
  (* | Print *)
  (* | Ret *)
  (* | Call of string * int *)
  (* | Jmp of int64 *)
  (* | Jnz of int64 *)
  (* | Label of string *)
  (* | Nop *)
let emulate ( st : State.machine_state ) ( instr:  Instr.instr ) : out_and_state=  
  match instr with
  | PushI i -> (None, {st with stack= ( I i )::st.stack })
  | PushD d -> (None, {st with stack= ( D d )::st.stack })
  | PushB b -> (None, {st with stack= ( B b )::st.stack })
  | PushC c -> (None, {st with stack= ( C c )::st.stack })
  | PushS s -> (None, {st with stack= ( S s )::st.stack })
  | PushFn s -> begin
    let () = Vector.push_back st.mem (Fn s) in
    (None, st )
  end
  | Load o -> 
      let index = ( List.hd st.fp ) + (Int64.to_int o) in
      let () = if index = (Vector.length st.mem) then
        Vector.push_back st.mem (List.hd st.stack)
      else 
        Vector.set st.mem index (List.hd st.stack)
      in
      (None, {st with stack = ( List.tl st.stack ) })
  | Ref o ->
      let index =  ( List.hd st.fp )  + ( Int64.to_int o ) in
      let elem =  Vector.get st.mem index in
      (None, {st with stack = elem::st.stack })
  | BinOp o -> 
      let rhs =   List.hd st.stack in
      let lhs = st.stack |>  List.tl |>List.hd in
      let new_stack =  st.stack |> List.tl |> List.tl in
      let new_top  = 
        match (lhs,rhs ) with
         | (I i, I j) ->  
             let res = match o with
              | "+" -> I (Int64.add i  j)
              | "-" -> I (Int64.sub i j)
              | "*" -> I (Int64.mul i j)
              | "/" -> I (Int64.div i j)
              | "!=" -> B (not (Int64.equal i  j))
              | "==" -> B (Int64.equal i j)
              | ">" -> B ((Int64.compare i j) > 0)
              | ">=" -> B ((Int64.compare i  j)>=0)
              | "<" -> B ((Int64.compare i  j) < 0)
              | "<=" -> B ((Int64.compare i  j) <= 0)
              | _ -> Err  ("Invalid Operator : " ^ o ^ " used on two int")
             in res
          | (D i, D j) ->
              let res = match o with
                | "+" -> D (i +. j)
                | "-" -> D (i -. j)
                | "*" -> D (i *. j)
                | "/" -> D (i /. j)
                | "!=" -> B (not (Float.equal i j))
                | "==" -> B (Float.equal i j)
                | ">" -> B ((Float.compare i j) > 0)
                | ">=" -> B ((Float.compare i  j)>=0)
                | "<" -> B ((Float.compare i  j) < 0)
                | "<=" -> B ((Float.compare i  j) <= 0)
                | _ -> Err  ("Invalid Operator : " ^ o ^ " used on two double") 
              in res
          | (C i, C j) ->
              let res = match o with
                | "+" -> S ((String.make 1 i) ^ (String.make 1 j))
                | "!=" -> B (not (Char.equal i j))
                | "==" -> B (Char.equal i j)
                | ">" -> B ((Char.compare i j) > 0)
                | ">=" -> B ((Char.compare i  j)>=0)
                | "<" -> B ((Char.compare i  j) < 0)
                | "<=" -> B ((Char.compare i  j) <= 0) 
                | _ -> Err ( "Invalid Operator : " ^ o ^ " used on two char")
              in res
          | (B i, B j) ->
              let res = match o with
                | "!=" -> B (not (Bool.equal i j))
                | "==" -> B (Bool.equal i j)
                | "and" -> B (i && j)
                | "or" -> B (i || j)
                | _ -> Err ("Invalid operator : " ^ o ^ " used on two bool" )
              in res
          | (S i, S j) ->
              let res =match o with
                | "+" -> S (i ^ j)
                | "!=" -> B (not (String.equal i j))
                | "==" -> B (String.equal i j)
                | ">" -> B ((String.compare i j) > 0)
                | ">=" -> B ((String.compare i  j)>=0)
                | "<" -> B ((String.compare i  j) < 0)
                | "<=" -> B ((String.compare i  j) <= 0) 
                | _ -> Err ( "Invalid Operator : " ^ o ^ " used on two string")
              in res
            | _ -> 
                Err (
                  "Type mismatch in lhs and rhs: lhs is "
                  ^ (string_of_data lhs) 
                  ^ " , rhs is "
                  ^ (string_of_data rhs) 
                  ^ " , and op is "
                  ^ o
                )
              in (None, {st with stack=new_top::new_stack})
  | PrefixOp o -> 
      let rhs = List.hd st.stack in
      let new_stack = List.tl st.stack in
      let new_top = 
        match rhs with
        | I i -> 
            let res = match o with
            | "-" -> I (Int64.neg i)
            | _ -> Err ("Invalid operator : " ^ o ^ " used on int")
            in res
        | D d ->
            let res = match o with
            | "-"  -> D (Float.neg d)
            | _ -> Err ("Invalid operator : " ^ o ^ " used on double")
            in res
        | B b ->
            let res = match o with
            | "!" -> B (Bool.not b)
            | _ -> Err ("Invalid operator : " ^ o ^ " used on bool")
            in res
        | _ -> Err (( string_of_data rhs ) ^ " doesnt support any unary operator used, Operator used : " ^ o)
      in (None , {st with stack = new_top::new_stack})
  | Print -> (Some (List.hd st.stack), {st with stack= ( List.tl st.stack )})
  | Ret ->
      let new_fp = List.tl st.fp in
      let new_ip = List.hd st.ret_addr in
      let to_be_dropped = ( ( Vector.length st.mem  ) - (List.hd st.fp)) + (List.hd st.args_cnt)  in
      let () = Vector.drop_last to_be_dropped st.mem in
      let new_state = 
        { st with
        ip = new_ip;
        fp = new_fp;
        args_cnt = List.tl st.args_cnt;
        ret_addr = List.tl st.ret_addr;
        (* mem =  Array.sub st.mem 0 (( Array.length st.mem ) - to_be_dropped) ; *)
        } in
      (None, new_state)
  | Call (lbl,cnt) -> 
    let res = match Util.get_label_addr lbl st.all_labels with
      | Some (ptr,_) ->  
          (None, {st with ret_addr= st.ip::st.ret_addr; ip =ptr;fp=(Vector.length st.mem)::st.fp;args_cnt=cnt::st.args_cnt })
      | None -> (None ,st)
      in
      res
  | Jmp off -> (None , {st with ip = st.ip + (Int64.to_int off)   })
  | Jnz off ->
      let top = List.hd st.stack in
      let jump =  match top with
        | B t -> t
        | D d -> d <> 0.0
        | I i -> i <> 0L
        | _ -> false 
      in
      let new_stack = List.tl st.stack in
      let new_st = if jump then  {st with ip = st.ip + (Int64.to_int off) ; stack = new_stack }
      else {st with stack=new_stack} in
      (None, new_st)
    | Label _ -> (None, st)
    | Nop -> (None,st)



let rec emulate_all_instrs ( instrs: instr array  )( state : State.machine_state ) = begin
  match Util.safe_get_index instrs (state.ip) with
   | None -> begin
     ()
   end
   | Some instr -> begin
       let (a,s) = emulate state instr in
       let new_state = {s with ip = s.ip + 1} in
       match a with
        | None -> begin
          emulate_all_instrs instrs new_state
        end
        | Some x -> begin
            match x with
              | Data.Err _ -> let () = print_endline ("Error!!!" ^ (string_of_data x)) in ()
              | _ -> 
                  let () = print_string (string_of_data x) in
                  emulate_all_instrs instrs new_state
        end
   end
end
