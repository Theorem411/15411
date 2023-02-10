open Core
let print_source ?(channel = Out_channel.stdout) sexps =
  let formatter = Format.formatter_of_out_channel channel in
  List.iter sexps ~f:(fun i -> Sexp.pp_hum formatter i);
  Format.pp_print_flush formatter ()
;;


let print_with_name name sexps = 
  print_source ((sexp_of_string (name ^ ":    ")) :: sexps)