open Core
module B = Block
module V = Graph.Vertex
module SP = Singlepass
module AS = Assem_l4
module VertexTable = Graph.VertexTable
module L = Label

module T = struct
  type t = L.bt [@@deriving compare, equal, sexp, hash]
end

module LComp = Comparable.Make (T)
module LM = LComp.Map
module LS = LComp.Set
module LT = Hashtbl.Make (T)
module OperS = AS.Set

(*_ For general passing: important data for each blocks *)
type block_du_t =
  { bdef : OperS.t
  ; buse : OperS.t
  }

type block_def_use = block_du_t LM.t

type block_lv_t =
  { liveout : OperS.t (*_ update by union with liveouts of successors *)
  ; livein : OperS.t (*_ update by replace *)
  }

type block_in_out = block_lv_t LT.t
type cfg_pred = LS.t LM.t
type lab_to_block = B.block LM.t

(*_ shared by many other compiler phases that needs liveness info *)
type live_package_t =
  { block_def : OperS.t LM.t
  ; block_use : OperS.t LM.t
  ; block_in : OperS.t LM.t
  ; block_out : OperS.t LM.t
  ; cfg_pred : LS.t LM.t
  ; l2block : B.block LM.t
  }

(*_ many init functions *)
let lab_to_block_init (fspace : B.fspace) : lab_to_block =
  let lbs = List.map fspace.fdef_blocks ~f:(fun b -> b.label, b) in
  LM.of_alist_exn lbs
;;

let wq_init ({ fdef_blocks; _ } : B.fspace) =
  let blst = List.rev fdef_blocks in
  (* let blst = List.filter blst ~f:(fun b -> match b.jump with | B.JRet -> true | _ -> false) in testing idea *)
  (* let () = prerr_endline (sprintf "wqinit:%s\n" (List.map blst ~f:(fun b -> Label.name_bt b.label) |> String.concat ~sep:",\n")) in  *)
  let res = Queue.of_list blst in
  res
;;

let cfg_pred_init ({ fdef_blocks; _ } : B.fspace) : cfg_pred =
  let mapf ({ label; jump; _ } : B.block) =
    match jump with
    | B.JCon { jt; jf } -> [ L.bt jt, label; L.bt jf, label ]
    | B.JUncon l -> [ L.bt l, label ]
    | _ -> []
  in
  let l2p = List.map fdef_blocks ~f:mapf |> List.concat in
  LM.map ~f:LS.of_list (LM.of_alist_multi l2p)
;;

let block_def_use_init (fspace : B.fspace) : block_def_use =
  let mapf (b : B.block) =
    let buse, bdef = SP.uses_defs_block b in
    b.label, { buse; bdef }
  in
  let l2du = List.map fspace.fdef_blocks ~f:mapf in
  LM.of_alist_exn l2du
;;

let block_in_out_init ({ fdef_blocks; _ } : B.fspace) : block_in_out =
  let block_lv_init () = { liveout = OperS.empty; livein = OperS.empty } in
  let mapf ({ label; _ } : B.block) = label, block_lv_init () in
  let l2i = List.map fdef_blocks ~f:mapf in
  LT.of_alist_exn l2i
;;

(* let rm_dead_blocks (fspace : B.fspace) : B.fspace * cfg_pred =
  let cfg = cfg_pred_init fspace in
  let dlabs = LM.filter cfg ~f:(fun ls -> LS.is_empty ls) |> LM.keys |> LS.of_list in
  let cfg_remain = LM.filter cfg ~f:(fun ls -> not (LS.is_empty ls)) in
  let fdef_blocks' =
    List.filter fspace.fdef_blocks ~f:(fun { label; _ } -> not (LS.mem dlabs label))
  in
  { fspace with fdef_blocks = fdef_blocks' }, cfg_remain
;; *)

(*_ the general passing algorithm *)
let mk_liveness_fspace (fspace : B.fspace) : live_package_t =
  (* init:
     1. init wq : the reverse of the original block ordering 
     2. init cfg : predecessor map 
     3. init l2du : label to block-level def and use 
     4. init l2io : label table from label to active livein and liveout 
     5. init SP.t : table from line to final livein & liveout info *)
  let wq = wq_init fspace in
  let cfg = cfg_pred_init fspace in
  let l2du = block_def_use_init fspace in
  let l2io = block_in_out_init fspace in
  let l2b = lab_to_block_init fspace in
  (*_ the general passing algorithm 
    input: a freshly dequeued block 
    to-do: 
    0. keep the old version of livein(b)
    1. update current block's livein.
      livein'(b) = useb + (liveout(b) - defb)
    2. update all predecessor's liveout. For b' in Pred(b)
      liveout(b') += livein(b)
    3. check if livein(b) == livein'(b). If not, add all predecessors back to wq
     *)
  let general_passing (block : B.block) : unit =
    (*_ load info into variables *)
    let ({ label; _ } : B.block) = block in
    let pred =
      match LM.find cfg label with
      | Some ps -> ps
      | None -> LS.empty
    in
    let { bdef; buse } = LM.find_exn l2du label in
    let { liveout; livein } = LT.find_exn l2io label in
    (*_ update own livein *)
    let livein' = OperS.union buse (OperS.diff liveout bdef) in
    let () = LT.update l2io label ~f:(fun _ -> { livein = livein'; liveout }) in
    (*_ update pred's liveout *)
    let update_opt = function
      | Some { liveout; livein } -> { liveout = OperS.union liveout livein'; livein }
      | None -> failwith "live: not possible"
    in
    let iterf (label : Label.bt) = LT.update l2io label ~f:update_opt in
    let () = LS.iter pred ~f:iterf in
    (*_ check livein convergence and put pred back on wq *)
    let pred_blocks = List.map (LS.to_list pred) ~f:(fun l -> LM.find_exn l2b l) in
    if OperS.equal livein livein' then () else Queue.enqueue_all wq pred_blocks
  in
  let rec loop (_ : unit) : unit =
    match Queue.dequeue wq with
    | Some block ->
      general_passing block;
      loop ()
    | None -> ()
  in
  let () = loop () in
  (*_ do one final single passing using biot *)
  (* 
  let mapf ((b, liveout) : B.block * V.Set.t) = SP.singlepass sptbl b liveout in
  let (_ : V.Set.t list) = List.map l2io' ~f:mapf in *)
  (* let () = prerr_endline "surprise! SP finishes!\n" in *)
  let block_def = LM.map l2du ~f:(fun b2du -> b2du.bdef) in
  let block_use = LM.map l2du ~f:(fun b2du -> b2du.buse) in
  let block_in = LT.map l2io ~f:(fun b2io -> b2io.livein) |> LM.of_hashtbl_exn in
  let block_out = LT.map l2io ~f:(fun b2io -> b2io.liveout) |> LM.of_hashtbl_exn in
  { block_def; block_use; block_in; block_out; cfg_pred = cfg; l2block = l2b }
;;

(*_ the final mk_graph_fspace function *)

let mk_graph_fspace (fspace : B.fspace) =
  (*_ do one single_pass for each block *)
  let spt = SP.init_table fspace in
  let { block_out; l2block; _ } = mk_liveness_fspace fspace in
  let iterf ~key:l ~data:liveout =
    let b = LM.find_exn l2block l in
    let liveout = V.op_set_to_vertex_set liveout in
    let _ : V.Set.t = SP.singlepass spt b liveout in
    ()
  in
  let () = LM.iteri block_out ~f:iterf in
  (*_ create vertex and edge using singlepass *)
  let vertices, edges = SP.get_edges_vertices spt fspace in
  (*_ create a hashtable graph *)
  let graph' = VertexTable.create () in
  let () =
    V.Set.iter vertices ~f:(fun v -> VertexTable.set graph' ~key:v ~data:V.Set.empty)
  in
  let () =
    List.iter edges ~f:(fun (a, b) ->
      let a_set = VertexTable.find_exn graph' a in
      let b_set = VertexTable.find_exn graph' b in
      if not (V.equal a b)
      then (
        let () = VertexTable.set graph' ~key:b ~data:(V.Set.add b_set a) in
        let () = VertexTable.set graph' ~key:a ~data:(V.Set.add a_set b) in
        ())
      else ())
  in
  (* let res = V.Map.of_hashtbl_exn graph' in
  let () = prerr_endline "done!" in
  res *)
  (* Graph.print (V.Map.of_hashtbl_exn graph'); *)
  graph', spt
;;
