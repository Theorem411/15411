open Core
module Tree = Tree_l4

module T = struct
  type t = Label.bt [@@deriving compare, equal, sexp, hash]
end

module LM = Map.Make (T)
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

let entry_is_target ({ jtag; code; _ } : entry) : Label.t option =
  match jtag, List.last_exn code with
  | Tree.JUncon l, Tree.Goto l' ->
    if Label.equal l l' then if List.length code = 1 then Some l else None else None
  | _ -> None
;;

let run_till_none (fdef : Tree.block list) : entry LH.t =
  let btbl = lh_init fdef in
  let wq = Queue.of_list (List.map fdef ~f:(fun b -> b.label)) in
  let rm_passing (lab : Label.bt) : unit =
    let blc = LH.find_exn btbl lab in
    match blc.jtag with
    | Tree.JRet -> ()
    | Tree.JCon { lt; lf } ->
      let blc_t = LH.find_exn btbl (Label.bt lt) in
      let blc_f = LH.find_exn btbl (Label.bt lf) in
      (match entry_is_target blc_t, entry_is_target blc_f with
       | Some newt, Some newf ->
         update_jcon btbl lab newt newf;
         Queue.enqueue wq lab
       | None, Some newf ->
         update_jcon btbl lab lt newf;
         Queue.enqueue wq lab
       | Some newt, None ->
         update_jcon btbl lab newt lf;
         Queue.enqueue wq lab
       | None, None -> ())
    | Tree.JUncon l ->
      let blc_off = LH.find_exn btbl (Label.bt l) in
      (match entry_is_target blc_off with
       | Some newl ->
         update_juncon btbl lab newl;
         Queue.enqueue wq lab
       | None -> ())
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
