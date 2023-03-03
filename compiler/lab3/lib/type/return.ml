open Core
module A = Aste
let global_err = Error_msg.create ()
;;
exception ReturnError
let error ~msg ~ast = 
  Error_msg.error global_err (Mark.src_span ast) ~msg;
  raise ReturnError
;;

let rec ret_checker_bool (prog : A.mstm) = 
  match Mark.data prog with 
  | A.Declare decl -> ret_checker_bool decl.body
  | A.Assign _ -> false
  | A.If ifs -> ret_checker_bool ifs.lb && ret_checker_bool ifs.rb
  | A.While _ -> false
  | A.Return _ -> true
  | A.Nop -> false
  | A.Seq (s1, s2) -> ret_checker_bool s1 || ret_checker_bool s2
  | A.NakedExpr _ -> false
  | A.AssertFail -> true
  | A.NakedCall _ -> false

let ret_checker_stm (prog : A.mstm) = 
  if ret_checker_bool prog then ()
  else error ~msg:(sprintf "program does not have explicit return") ~ast:prog
;;
let ret_checker_glob (mglob : A.mglob) =
  match (Mark.data mglob) with
  | A.Fundef { fdef; _ } -> ret_checker_stm fdef
  | _ -> ()

let ret_checker: A.program -> unit = List.iter ~f:ret_checker_glob;;