open Util;;

module Vector = struct
  type 'a t =  (int,'a) Hashtbl.t
  exception Not_found

  let create n= Hashtbl.create n
  let length h = Hashtbl.length h
  let get h i = Hashtbl.find_opt h i
  let set h k v= Hashtbl.replace h k v
  let delete h k = Hashtbl.remove h k
  let drop_last n h = begin
    let len = length h in
    let last = len -  1 in
    let start = last -  n in
    let sequence = start -- last in
    List.iter (fun i -> delete h i ) sequence
  end
end
