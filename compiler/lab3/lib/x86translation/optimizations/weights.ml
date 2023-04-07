open Core
module V = Graph.Vertex
module AS = Assem_l4
module VT = Graph.VertexTable

(* TEMP ANALYSIS *)
let add_weight (tbl : int VT.t) (key : V.t) (v : int) =
  let current_value = VT.find tbl key in
  match current_value with
  | None -> VT.set tbl ~key ~data:v
  | Some value -> VT.set tbl ~key ~data:(value + v)
;;

(* MOVE ANALYSIS *)
module PairV = struct
  (* Assume the following:
    1. Always smaller temp is first
  *)
  type t = V.t * V.t [@@deriving sexp, compare, hash]
end

module PairVTbl = Hashtbl.Make (PairV)

let ordered (a, b) : PairV.t =
  let res = V.compare a b in
  if res > 0 then b, a else a, b
;;

let add_weight_pair (tbl : int PairVTbl.t) (key : PairV.t) (v : int) =
  let key = ordered key in
  let current_value = PairVTbl.find tbl key in
  match current_value with
  | None -> PairVTbl.set tbl ~key ~data:v
  | Some value -> PairVTbl.set tbl ~key ~data:(value + v)
;;

let process_move consume depth = function
  | AS.Mov { dest = AS.Temp t1; src = AS.Temp t2; _ } ->
    consume (V.T t1, V.T t2) (Int.pow 10 depth)
  | AS.Mov { dest = AS.Reg r; src = AS.Temp t; _ } ->
    consume (V.T t, V.R r) (Int.pow 10 depth)
  | AS.Mov { dest = AS.Temp t; src = AS.Reg r; _ } ->
    consume (V.T t, V.R r) (Int.pow 10 depth)
  | _ -> ()
;;

let get_sorted_moves tbl =
  let moves : (PairV.t * int) list = PairVTbl.to_alist tbl in
  (* sorted in descending order of weights *)
  let sorted_moves =
    List.sort moves ~compare:(fun (_, w1) (_, w2) -> Int.compare w2 w1)
  in
  let res = List.map sorted_moves ~f:(fun (p, _) -> p) in
  if true
  then
    (fun () ->
      prerr_endline
        (sprintf
           "Stopped ordering moves:[%s]"
           (String.concat
              ~sep:","
              (List.map sorted_moves ~f:(fun ((a, b), d) ->
                   sprintf "(%s <-> %s)[%d]" (V._to_string a) (V._to_string b) d)))))
      ()
  else ();
  res
;;

let calc_weights (sptbl : Singlepass.t) (b : Block.fspace) =
  let uses : int VT.t = VT.create () in
  let depths : int VT.t = VT.create () in
  let pair_tbl : int PairVTbl.t = PairVTbl.create () in
  (* do a sinlge iteration *)
  List.iter b.fdef_blocks ~f:(fun b ->
      List.iter b.block ~f:(fun (l, instr) ->
          (*  adding individual temp information *)
          let used_in_line = Singlepass.get_uses_exn sptbl l in
          (* process only temps *)
          V.Set.iter used_in_line ~f:(function
              | V.T _ as v ->
                add_weight uses v 1;
                add_weight depths v (Int.pow 10 b.depth)
              | V.R _ -> ());
          (* process moves *)
          process_move (add_weight_pair pair_tbl) b.depth instr;
          ()));
  let moves = get_sorted_moves pair_tbl in
  let weight (v : V.t) =
    match v with
    | V.R _ -> None
    | V.T t ->
      let u = VT.find uses (V.T t) in
      (* overall calculate function u * (sum of 10 ^ d_i) ??? *)
      match u with
      | Some u' ->  let d = VT.find_exn depths (V.T t) in Some (u' * d)
      | None -> None
  in
  weight, moves
;;

(* match v with
  | V.R _ -> None
  | V.T _ ->  *)
