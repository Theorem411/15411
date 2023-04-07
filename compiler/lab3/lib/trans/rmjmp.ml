open Core
module Tree = Tree_l4

module T = struct
  type t = Label.bt [@@deriving compare, equal, sexp, hash]
end

module LM = Map.Make (T)
module LS = Set.Make (T)
module LH = Hashtbl.Make (T)

type entry =
  { code : Tree.stm list
  ; jtag : Tree.jump_t
  ; idx : int
  ; depth : int
  }

let lh_init (fdef : Tree.block list) : entry LH.t =
  let mapf idx ({ label; block; jump; loop_depth } : Tree.block) =
    label, { idx; code = block; jtag = jump; depth = loop_depth }
  in
  let fdef' = List.mapi fdef ~f:mapf in
  LH.of_alist_exn fdef'
;;

let pred_init (fdef : Tree.block list) : LS.t LH.t =
  let mapf ({ label; jump; _ } : Tree.block) : (Label.bt * Label.bt) list =
    match jump with
    | Tree.JCon { lt; lf } -> [ Label.bt lt, label; Label.bt lf, label ]
    | Tree.JUncon l -> [ Label.bt l, label ]
    | _ -> []
  in
  let elst = List.map fdef ~f:mapf |> List.concat in
  let etbl = LH.of_alist_multi elst in
  LH.map etbl ~f:LS.of_list
;;

let update_juncon (binfo : entry LH.t) (bid : Label.bt) (new_lab : Label.t) : unit =
  let entry = LH.find_exn binfo bid in
  let tl = List.last_exn entry.code in
  let entry' =
    match entry.jtag, tl with
    | Tree.JUncon l, Tree.Goto l' ->
      if Label.equal l l'
      then (
        let rest = List.drop_last_exn entry.code in
        { entry with code = rest @ [ Tree.Goto new_lab ]; jtag = Tree.JUncon new_lab })
      else failwith "rmjmp: impossible!"
    | _ -> failwith "rmjmp: impossible!"
  in
  LH.update binfo bid ~f:(fun _ -> entry')
;;

let update_jcon (binfo : entry LH.t) (bid : Label.bt) (nlt : Label.t) (nlf : Label.t)
  : unit
  =
  let entry = LH.find_exn binfo bid in
  let tl = List.last_exn entry.code in
  let entry' =
    match entry.jtag, tl with
    | Tree.JCon { lt; lf }, Tree.If { cond; lt = lt'; lf = lf' } ->
      if not (Label.equal lt lt')
      then failwith "rmjmp: impossible!"
      else if not (Label.equal lf lf')
      then failwith "rmjmp : impossible!"
      else (
        let rest = List.drop_last_exn entry.code in
        { entry with
          code = rest @ [ Tree.If { cond; lt = nlt; lf = nlf } ]
        ; jtag = Tree.JCon { lt = nlt; lf = nlf }
        })
    | _ -> failwith "rmjmp: impossible"
  in
  LH.update binfo bid ~f:(fun _ -> entry')
;;

let entry_is_empty ({ jtag; code; _ } : entry) : bool =
  (*_ whether entry is a blc that ends on a goto l, and with no code *)
  match jtag with
  | Tree.JUncon _ -> if List.length code = 1 then true else false
  | _ -> false
;;

let entry_is_empty_opt ({ jtag; code; _ } : entry) : Label.t option =
  (*_ return label of the sole successor *)
  match jtag, List.last_exn code with
  | Tree.JUncon l, Tree.Goto l' ->
    if Label.equal l l' then if List.length code = 1 then Some l else None else None
  | _ -> None
;;

let entry_is_single_pred (preds : LS.t LH.t) (l : Label.bt) : bool =
  let pred_of_l = LH.find_exn preds l in
  if LS.length pred_of_l = 1 then true else false
;;

let entry_is_single_pred_opt (preds : LS.t LH.t) (l : Label.bt) : Label.bt option =
  let pred_of_l = LH.find_exn preds l in
  if LS.length pred_of_l = 1
  then (
    let single_parent = LS.to_list pred_of_l |> List.hd_exn in
    Some single_parent)
  else None
;;

let run_till_none (fdef : Tree.block list) : entry LH.t =
  (*_ init tbl from label to entry *)
  let btbl = lh_init fdef in
  (*_ init tbl from label to predecessors *)
  let preds = pred_init fdef in
  (*_ init wq *)
  let init_fn (blc : Tree.block) : Label.bt option =
    let entry = LH.find_exn btbl blc.label in
    if entry_is_empty entry
    then Some blc.label
    else if entry_is_single_pred preds blc.label
    then Some blc.label
    else None
  in
  let init_lst = List.filter_map fdef ~f:init_fn in
  let wq = Queue.of_list init_lst in
  (*_ rm_passing *)
  let rm_passing (lab : Label.bt) : unit =
    let entry = LH.find_exn btbl lab in
    match entry_is_empty_opt entry with 
    | Some sole_chld -> failwith "no"
    | None -> failwith "no"
  in
  let rec loop () =
    match Queue.dequeue wq with
    | Some lab ->
      rm_passing lab;
      loop ()
    | None -> ()
  in
  let () = loop () in
  btbl
;;

let tbl_to_blc_lst (tbl : entry LH.t) : Tree.block list =
  let lst = LH.to_alist tbl in
  let mapf (label, { code; jtag; idx; depth }) : int * Tree.block =
    idx, { label; block = code; jump = jtag; loop_depth = depth }
  in
  let lst' = List.map lst ~f:mapf in
  let compare (i1, _) (i2, _) = Int.compare i1 i2 in
  let lst_sort = List.sort lst' ~compare in
  List.map lst_sort ~f:snd
;;

let rmjmp_fspace (fspace : Tree.fspace_block) : Tree.fspace_block =
  let tbl = run_till_none fspace.fdef in
  let fdef = tbl_to_blc_lst tbl in
  { fspace with fdef }
;;

let rmjmp = List.map ~f:rmjmp_fspace
