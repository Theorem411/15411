open Core
module B = Block
module V = Graph.Vertex
module AS = Assem_new
module IntTable = Hashtbl.Make (Int)

type ht_entry =
  { d : V.Set.t
  ; u : V.Set.t
  ; lin : V.Set.t
  ; lout : V.Set.t
  }

type live_t = (int, V.Set.t) Hashtbl.t

let get_init (_ : int) : live_t * live_t =
  let empty_1 : live_t = IntTable.create () in
  let empty_2 : live_t = IntTable.create () in
  empty_1, empty_2
;;

(* let print_vset (es : V.Set.t) =
  print_string "[";
  V.Set.iter
    ~f:(fun v ->
      V.print v;
      print_string ", ")
    es;
  print_string "]"
;; *)

let update_info table (live_in, live_out) i = 
  let lin = IntTable.find_exn live_in i in 
  let lout = IntTable.find_exn live_out i in 
  IntTable.update table i ~f:(
    fun v -> (match v with
    | Some(info) -> {d = info.d; u = info.d; lin; lout} 
    | None -> failwith ("Can not find entry for " ^ (Int.to_string i)))
  );;

let handle_instrs
    (table : (int, ht_entry) Hashtbl.t)
    (instrs : (int * AS.instr) list)
    (indicies : int list)
    (input : V.Set.t)
    : V.Set.t
  =
  let n = List.length instrs in
  (* gets current live_in and live_out, updates them accordingly *)
  let (live_in, live_out) : live_t * live_t = get_init n in
  let liveness_aux (i, _) () =
    let info = IntTable.find_exn table i in
    (* let all_succs = succ i in *)
    let live_out_i =
      match i = n - 1 with
      | true -> input
      | false -> IntTable.find_exn live_in (i + 1)
    in
    let live_in_i = V.Set.union info.u (V.Set.diff live_out_i info.d) in
    let () =
      match IntTable.add live_in ~key:i ~data:live_in_i with
      | _ -> ()
    in
    let () =
      match IntTable.add live_out ~key:i ~data:live_out_i with
      | _ -> ()
    in
    ()
  in
  let () = List.fold_right instrs ~f:liveness_aux ~init:() in
  let update = update_info table (live_in, live_out) in 
  let () = List.iter ~f:(fun i -> update i) indicies in 
  let last = List.nth_exn indicies ((List.length indicies) - 1) in 
  IntTable.find_exn live_in last
;;

(* let singlepass : (int, ht_entry) Hashtbl.t -> B.fspace_block -> V.Set.t -> V.Set.t *)
let singlepass
    (table : (int, ht_entry) Hashtbl.t)
    (block : B.block)
    (input : V.Set.t)
    : V.Set.t
  = 

;;

(* let get_edge_vertex: (int, ht_entry) Hashtbl.t -> B.fspace_block -> (V.Set.t) * (V.t * V.t list) *)