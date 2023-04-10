(* open Core
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

let rm_empty_blc_passing (btbl : entry LH.t) (preds : LS.t LH.t) : unit =
  let all_empty_blcs = LH.filter btbl ~f:entry_is_empty in
  let iterf1 ~key:l ~data:entry =
    (*_ 1. get all predecessors of l *)
    let l_preds = LH.find_exn preds l in
    (*_ 2. get the single chld of l *)
    let l_chld =
      match entry_is_empty_opt entry with
      | Some l' -> l'
      | None -> failwith "rmjmp: ur kidding right?"
    in
    (*_ 3. update l_chld's pred to l; pred(l_chld) = (pred(l_chld) - l) u l_preds *)
    let update_f = function
      | Some s -> LS.union (LS.remove s l) l_preds
      | None -> failwith "rmjmp: ur joking"
    in
    let () = LH.update preds (Label.bt l_chld) ~f:update_f in
    (*_ 4. for each lp \in l_preds, update their successor to l_chld *)
    let iterf2 lp =
      let { jtag; _ } = LH.find_exn btbl lp in
      match jtag with
      | Tree.JUncon _ -> update_juncon btbl lp l_chld
      | Tree.JCon { lt; lf } ->
        if Label.equal_bt (Label.bt lt) l
        then update_jcon btbl lp l_chld lf
        else if Label.equal_bt (Label.bt lf) l
        then update_jcon btbl lp lt l_chld
        else failwith "rmjmp: cond jtag don't have this successor?!"
      | _ -> failwith "rmjmp: but u said u have successor!"
    in
    let () = LS.iter l_preds ~f:iterf2 in
    (*_ 5. delete l from btbl *)
    let () = LH.remove btbl l in 
    ()
  in
  LH.iteri all_empty_blcs ~f:iterf1
;;

let rm_single_pred_passing (ls : Label.bt list) (btbl : entry LH.t) (preds : LS.t LH.t)
  : unit
  =
  failwith "no"
;;

let rm_dead_blc_passing (btbl : entry LH.t) (preds : LS.t LH.t) : unit =
  (*_ init wq with initially dead blocks *)
  let wq_init = LH.filter preds ~f:(fun s -> LS.length s = 0) |> LH.keys in
  let wq = Queue.of_list wq_init in
  let rec loop () =
    match Queue.dequeue wq with
    | Some l ->
      let l_preds = LH.find_exn preds l in
      if LS.length l_preds = 0
      then (
        (*_ 1. find all of this dead block's children *)
        let { jtag; _ } = LH.find_exn btbl l in
        let chlds =
          match jtag with
          | Tree.JRet -> []
          | Tree.JUncon l' -> [ l' ]
          | Tree.JCon { lt; lf } -> [ lt; lf ]
        in
        let chlds = List.map chlds ~f:Label.bt in
        (*_ 2. delete this block from table *)
        let () = LH.remove btbl l in
        (*_ 3. delete l from each chld's predecessor *)
        let f (chld_l : Label.bt) =
          let l_preds = LH.find_exn preds chld_l in
          LH.update preds chld_l ~f:(fun _ -> LS.remove l_preds l)
        in
        let () = List.iter chlds ~f in
        (*_ 4. add children back to wq *)
        let () = Queue.enqueue_all wq chlds in
        (*_ 5. loop on *)
        loop ())
      else loop ()
    | None -> ()
  in
  loop ()
;;

let run_till_none (fdef : Tree.block list) : entry LH.t =
  (*_ init tbl from label to entry *)
  let btbl = lh_init fdef in
  (*_ init tbl from label to predecessors *)
  let preds = pred_init fdef in
  (*_ label list *)
  let labs = List.map fdef ~f:(fun blc -> blc.label) in
  (*_ dead block eliminations *)
  let () = rm_dead_blc_passing btbl preds in
  (*_ empty block elimination *)
  let () = rm_empty_blc_passing btbl preds in
  (*_ single pred block elimination *)
  let () = rm_single_pred_passing labs btbl preds in
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

let rmjmp = List.map ~f:rmjmp_fspace *)
