let is_label i = match i with
 | (_, Instr.Label _) -> true
 | _ -> false


let extract_label_name (i, instr) = match instr with
  | Instr.Label x -> (i, x)
  | _ ->  (i, "")


let get_label_addr label l = List.find_opt (fun (_,l) -> l=label) l

let safe_get_index arr ind = try Some (arr.(ind)) with
    _ -> None

let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j [] ;;
