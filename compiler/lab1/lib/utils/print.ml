let print_list ~to_string ls = 
  let res_str = List.fold_left (fun acc -> fun x -> acc ^(to_string x)^", ") "" ls  in
    print_string ("[" ^ res_str ^ "]")
;;