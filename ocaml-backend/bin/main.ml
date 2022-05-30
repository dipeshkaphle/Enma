open Ocaml_backend_lib;;

let () = 
    let args = Sys.argv  in
    match args with
        | [|_|] -> 
            print_endline "Please Provide Arguments : <this-binary-name> <input-file>"
        | _ -> 
                let ch = open_in args.(1) in
                let s = really_input_string ch (in_channel_length ch) in
                let () = close_in ch in
                let all_lines = String.split_on_char '\n' s in
                let instrs = Array.of_list ( List.map Instr.parse_instr all_lines ) in
                let indexed_instrs = (List.mapi (fun i x -> (i, x)) (Array.to_list  instrs)) in
                let all_labels = indexed_instrs |> (List.filter Util.is_label) 
                            |> (List.map Util.extract_label_name) in
                let state : State.machine_state =  { 
                    fp = [0]; ip = 0; stack = []; 
                    mem = (Vector.Vector.create 1000) ; ret_addr= []; 
                    all_labels = all_labels; args_cnt = []
                } in
                Emulator.emulate_all_instrs instrs state
