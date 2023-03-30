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
type block_def_use = (V.Set.t * V.Set.t) LM.t
type block_lv_t = {
    liveout: V.Set.t (*_ update by union with liveouts of successors *)
  ; livein: V.Set.t (*_ update by replace *)
}
type block_in_out = block_lv_t LT.t
type cfg_pred = LS.t LM.t

(*_ many init functions *)

let wq_init ({ fdef_blocks; _ } : AS.fspace) = Queue.of_list fdef_blocks

let cfg_pred_init ({ fdef_blocks; _ } : AS.fspace ) : cfg_pred = 
  let mapf ({label; jump; _} : AS.block) = (
    match jump with 
    | AS.JCon {jt; jf} -> [(L.bt jt, label); (L.bt jf, label)]
    | AS.JUncon l -> [(L.bt l, label)]
    | _ -> []
  ) in 
  let l2p = List.map fdef_blocks ~f:mapf |> List.concat in
  LM.map ~f:LS.of_list (LM.of_alist_multi l2p)
;;

let block_def_use_init ({ fdef_blocks; _} : AS.fspace ) : block_def_use = 
  failwith "not there"
;;

let block_in_out_init ( { fdef_blocks; _ } : AS.fspace ) : block_in_out = 
  let block_lv_init () = { liveout = V.Set.empty; livein = V.Set.empty } in
  let mapf ({ label; _ } : AS.block ) = (label, block_lv_init ()) in
  let l2i = List.map fdef_blocks ~f:mapf in
  LT.of_alist_exn l2i
;;

(*_ the general passing algorithm *)
let mk_liveness_fspace (fspace : AS.fspace) =
  (*_ init: wq,  *)
  let wq = wq_init fspace in
  let cfg = cfg_pred_init fspace in
  (* let general_passing ({label; block; _} : AS.block) : unit =  in *)
  failwith "n"
;;

(*_ the final mk_graph_fspace function *)
module VertexTable = Hashtbl.Make (V)

let mk_graph_fspace (fspace : B.fspace) = failwith "not implemented" *)
