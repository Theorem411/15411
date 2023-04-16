open Core
module T = Tree_l4
module SM = Symbol.Map

let debug_mode = false
let debug_print (err_msg : string) : unit = if debug_mode then printf "%s" err_msg else ()

type fspace_target =
  { fname : Symbol.t
  ; args : (Temp.t * int) list
  ; fdef : T.stm list
  ; return : T.mpexp option
  }

let pp_fspace_target { fname; args; fdef; return } : string =
  sprintf
    "------------ L1 blc: %s(%s) --------------\n  %s\n  ret: %s\n  "
    (Symbol.name fname)
    (List.map args ~f:(fun (t, sz) -> sprintf "%s:%i" (Temp.name t) sz)
    |> String.concat ~sep:", ")
    (List.map fdef ~f:T.Print.pp_stm |> String.concat ~sep:"\n")
    (match return with
    | None -> "-"
    | Some e -> T.Print.pp_mpexp e)
;;

let pp_target (target : fspace_target SM.t) : string =
  let target = SM.to_alist target in
  let res =
    List.map target ~f:(fun (_, fspace) -> pp_fspace_target fspace)
    |> String.concat ~sep:"\n\n"
  in
  sprintf "targets: \n%s\n" res
;;

let is_lab1_stm = function
  | T.MovFuncApp _ ->
    (* let () = debug_print (sprintf "    this is the reason why! %s() \n" (Symbol.name fname)) in *)
    false
  | _ -> true
;;

let is_lab1_code (code : T.stm list) : bool =
  (* let () = debug_print (sprintf "    is_lab1(%s)? %b\n" (List.map code ~f:T.Print.pp_stm |> String.concat ~sep:";") (is_lab1_code code)) in *)
  let booleans = List.map code ~f:is_lab1_stm in
  List.fold booleans ~init:true ~f:( && )
;;
(* 
let rm_redundant_ret (fdef : T.block list) : T.block option = 
  if List.length fdef <> 2 then None
  else  
  match List.last fdef with 
  | None -> None
  | Some { jump; block; _ } -> 
    match (block, jump) with 
    | ([T.Return None], T.JRet) -> failwith "no"
    | _ -> None *)

let is_target ({ fname; args; fdef } : T.fspace_block) : fspace_target option =
  (* let () = debug_print (sprintf ">>> is %s target? fdef=%i\n" (Symbol.name fname) (List.length fdef)) in *)
  if String.equal fname.name "_c0_main"
  then None
  else if List.length fdef <> 1
  then None
  else (
    let block = List.hd_exn fdef in
    let code = block.block |> List.rev in
    (* let () = debug_print (sprintf "    |code| = %i\n" (List.length code)) in *)
    if List.length code > 50
    then None
    else if not (is_lab1_code code)
    then None
    else (
      let return, rest =
        match code with
        | T.Return ret :: rest -> ret, rest
        | _ ->
          failwith
            (sprintf
               "inline: encounter L1 func %s without a return at the end, what is trans \
                doing?"
               (Symbol.name fname))
      in
      let fdef = List.rev rest in
      let fspace_target = { fname; args; fdef; return } in
      (* let () = debug_print (sprintf "    find target! %s\n" (pp_fspace_target fspace_target)) in *)
      Some fspace_target))
;;

let split_program (program : T.program) : fspace_target SM.t * T.program =
  let prog = List.filter_map program ~f:is_target in
  let res1 = List.map prog ~f:(fun fspace -> fspace.fname, fspace) |> SM.of_alist_exn in
  let res2 =
    List.filter_map program ~f:(fun prog ->
        match is_target prog with
        | Some _ -> None
        | None -> Some prog)
  in
  res1, res2
;;

let inline_block (targets : fspace_target SM.t) (block : T.block) : T.block =
  let foldf acc stm =
    match stm with
    | T.MovFuncApp { dest; fname; args = es; _ } ->
      (* let () = debug_print (sprintf "fname=%s\n" (Symbol.name fname)) in *)
      (match SM.find targets fname with
      | None -> stm :: acc
      | Some { args; fdef; return; _ } ->
        let mov_args =
          List.zip_exn args es
          |> List.map ~f:(fun ((dest, _), src) -> T.MovPureExp { dest; src })
        in
        let finisher =
          match dest, return with
          | Some (d, _), Some e -> [ T.MovPureExp { dest = d; src = e } ]
          | Some _, None ->
            failwith
              "inline: impossible! encounter a target block with destination but no \
               return!"
          | _ -> []
        in
        let inline = List.concat [ mov_args; fdef; finisher ] |> List.rev in
        inline @ acc)
    | stm -> stm :: acc
  in
  let code : T.stm list = List.fold block.block ~init:[] ~f:foldf in
  let code = List.rev code in
  { block with block = code }
;;

let inline_fspace (targets : fspace_target SM.t) ({ fname; args; fdef } : T.fspace_block)
    : T.fspace_block
  =
  { fname; args; fdef = List.map fdef ~f:(inline_block targets) }
;;

let inline (prog : T.program) : T.program =
  let () = debug_print (sprintf "Before: \n%s\n\n" (T.Print.pp_program prog)) in
  let targets, prog = split_program prog in
  let () = debug_print (pp_target targets) in
  let prog = List.map prog ~f:(inline_fspace targets) in
  let () = debug_print (sprintf "After: \n%s\n\n" (T.Print.pp_program prog)) in
  prog
;;
