open Core
module T = Tree_l4
module SM = Symbol.Map

type fspace_target =
  { fname : Symbol.t
  ; args : (Temp.t * int) list
  ; fdef : T.stm list
  ; return : T.mpexp option
  }

let is_target ({ fname; args; fdef } : T.fspace_block) : fspace_target option =
  if List.length fdef <> 1
  then None
  else (
    let block = List.hd_exn fdef in
    let code = block.block |> List.rev in
    if List.length code <= 15
    then (
      let return, rest =
        match code with
        | T.Return ret :: rest -> ret, rest
        | _ ->
          failwith
            "inline: encounter basic block without a return at the end, what is trans \
             doing?"
      in
      let fdef = List.rev rest in
      Some { fname; args; fdef; return })
    else None)
;;

let split_program (program : T.program) : fspace_target SM.t * T.fspace_block list =
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
  let foldf acc = function
    | T.MovFuncApp { dest; fname; args = es; _ } ->
      let { args; fdef; return; _ } = SM.find_exn targets fname in
      let mov_args =
        List.zip_exn args es
        |> List.map ~f:(fun ((dest, _), src) -> T.MovPureExp { dest; src })
      in
      let finisher =
        match dest, return with
        | Some (d, _), Some e -> [ T.MovPureExp { dest = d; src = e } ]
        | Some _, None ->
          failwith
            "inline: impossible! encounter a target block with destination but no return!"
        | _ -> []
      in
      let inline = List.concat [ mov_args; fdef; finisher ] |> List.rev in
      inline @ acc
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
  let targets, prog = split_program prog in
  let prog = List.map prog ~f:(inline_fspace targets) in
  prog
;;
