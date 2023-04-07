open Core
module V = Graph.Vertex
module AS = Assem_l4

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

let add_weight (tbl : int PairVTbl.t) (key : PairV.t) (v : int) =
  let key = ordered key in
  let current_value = PairVTbl.find tbl key in
  match current_value with
  | None -> PairVTbl.set tbl ~key ~data:v
  | Some value -> PairVTbl.set tbl ~key ~data:(value + v)
;;

let process_block (consume : PairV.t -> int -> unit) (b : AS.block) : unit =
  List.iter b.block ~f:(function
      | AS.Mov { dest = AS.Temp t1; src = AS.Temp t2; _ } ->
        consume (V.T t1, V.T t2) (Int.pow 10 b.depth)
      | AS.Mov { dest = AS.Reg r; src = AS.Temp t; _ } ->
        consume (V.T t, V.R r) (Int.pow 10 b.depth)
      | AS.Mov { dest = AS.Temp t; src = AS.Reg r; _ } ->
        consume (V.T t, V.R r) (Int.pow 10 b.depth)
      | _ -> ())
;;

let order_moves (f : AS.fspace) : (V.t * V.t) list =
  prerr_endline "Started ordering moves";
  let tbl : int PairVTbl.t = PairVTbl.create () in
  List.iter f.fdef_blocks ~f:(process_block (add_weight tbl));
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
