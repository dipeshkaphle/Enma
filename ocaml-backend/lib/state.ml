type machine_state = { fp : int list;
    ip : int ;
    stack : Data.data list;
    mutable mem : Data.data array;
    (* memoryLen : int; *)
    ret_addr : int list;
    all_labels : ( int * string  ) list;
    args_cnt : int list; 
  } [@@deriving show]

