(* open Core
module B = Block
module V = Graph.Vertex
module SP = Singlepass
module AS = Assem_l4
module L = Label

module T = struct
  type t = L.bt [@@deriving compare, equal, sexp, hash]
end

module LM = Map.Make (T)
module LS = Set.Make (T)
module LT = Hashtbl.Make (T)

(*_ For general passing: important data for each blocks *)
type block_du_t =
  { bdef : V.Set.t
  ; buse : V.Set.t
  }

type block_def_use = block_du_t LM.t

type block_lv_t =
  { liveout : V.Set.t (*_ update by union with liveouts of successors *)
  ; livein : V.Set.t (*_ update by replace *)
  }

type block_in_out = block_lv_t LT.t
type cfg_pred = LS.t LM.t

type lab_to_block = B.block LM.t 

(*_ many init functions *)
let lab_to_block_init ({ fdef_blocks; _} : AS.fspace) = 
  let 
;;
let wq_init ({ fdef_blocks; _ } : AS.fspace) = Queue.of_list fdef_blocks

let cfg_pred_init ({ fdef_blocks; _ } : AS.fspace) : cfg_pred =
  let mapf ({label; jump; _} : AS.block) =
    match jump with
    | AS.JCon { jt; jf } -> [ L.bt jt, label; L.bt jf, label ]
    | AS.JUncon l -> [ L.bt l, label ]
    | _ -> []
  in
  let l2p = List.map fdef_blocks ~f:mapf |> List.concat in
  LM.map ~f:LS.of_list (LM.of_alist_multi l2p)
;;

let block_def_use_init ({ fdef_blocks; _ } : AS.fspace) : block_def_use =
  failwith "not there"
;;

let block_in_out_init ({ fdef_blocks; _ } : AS.fspace) : block_in_out =
  let block_lv_init () = { liveout = V.Set.empty; livein = V.Set.empty } in
  let mapf ({ label; _ } : AS.block) = label, block_lv_init () in
  let l2i = List.map fdef_blocks ~f:mapf in
  LT.of_alist_exn l2i
;;

(*_ the general passing algorithm *)
let mk_liveness_fspace (fspace : AS.fspace) =
  (*_ init: wq,  *)
  let wq = wq_init fspace in
  let cfg = cfg_pred_init fspace in
  let b2du = block_def_use_init fspace in
  let biot = block_in_out_init fspace in
  let sptbl : SP.t = failwith "no" in
  let general_passing (block : AS.block) : unit =
    let ({ label; _ } : AS.block) = block in
    let pred = LM.find_exn cfg label in
    let { bdef; buse } = LM.find_exn b2du label in
    let { liveout; livein } = LT.find_exn biot label in
    (*_ update own livein *)
    let livein' = V.Set.union buse (V.Set.diff liveout bdef) in
    let () = LT.update biot label ~f:(fun _ -> { livein = livein'; liveout }) in
    (*_ update pred's liveout *)
    let update_opt = function
      | Some { liveout; livein } -> { liveout = V.Set.union liveout livein'; livein }
      | None -> failwith "live: not possible"
    in
    let iterf ({label; _}: AS.block) = LT.update biot label ~f:update_opt in
    let () = BS.iter pred ~f:iterf in
    (*_ check livein convergence and put pred back on wq *)
    let () = if V.Set.equal livein livein' then () else Queue.enqueue_all wq (BS.to_list pred) in
    ()
  in
  (*_ do one final single passing using biot *)
  failwith "n"
;;

(*_ the final mk_graph_fspace function *)
module VertexTable = Hashtbl.Make (V)

let mk_graph_fspace (fspace : B.fspace) = failwith "not implemented" *)
