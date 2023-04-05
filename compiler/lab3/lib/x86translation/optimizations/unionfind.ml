open Core
module T = Temp

(* ChatGPT greatly helper to generate this file *)
(* Define a mutable record for the disjoint set *)
type disjoint_set =
  { mutable parent : T.t option
  ; mutable rank : int
  }

module TT = Hashtbl.Make (T)

type forest = disjoint_set TT.t

(* Create a new disjoint set forest *)
let create_forest () : disjoint_set TT.t = TT.create ()

(* Add a new vertex to the disjoint set forest *)
let add_vertex (forest : disjoint_set TT.t) (x : 'a) =
  TT.add_exn forest ~key:x ~data:{ parent = None; rank = 0 }
;;

(* Find the root of the given vertex using path compression *)
let rec find (forest : disjoint_set TT.t) (x : T.t) =
  let node =
    match TT.find forest x with
    | Some node -> node
    | None ->
      add_vertex forest x;
      (*if the vertex is not there, add it and continue *)
      TT.find_exn forest x
  in
  match node.parent with
  | None -> x
  | Some p ->
    let root = find forest p in
    node.parent <- Some root;
    root
;;

(* Union the sets containing x and y, if they are not already in the same set *)
let union (forest : disjoint_set TT.t) (x : 'a) (y : 'a) =
  let root_x = find forest x in
  let root_y = find forest y in
  if Core.phys_equal root_x root_y
  then (
    let node_x = TT.find_exn forest root_x in
    let node_y = TT.find_exn forest root_y in
    if node_x.rank > node_y.rank
    then node_y.parent <- Some root_x
    else (
      node_x.parent <- Some root_y;
      if node_x.rank = node_y.rank then node_y.rank <- node_y.rank + 1))
;;

let union_to (forest : disjoint_set TT.t) (x, y) (z : T.t) =
  prerr_endline
    (sprintf "union to (%s, %s) -> %s" (Temp.name x) (Temp.name y) (Temp.name z));
  let root_x = find forest x in
  let root_y = find forest y in
  let new_parent = find forest z in
  if not (Core.phys_equal root_x root_y)
  then (
    let node_x = TT.find_exn forest root_x in
    let node_y = TT.find_exn forest root_y in
    let node_new_parent = TT.find_exn forest new_parent in
    node_x.parent <- Some new_parent;
    node_y.parent <- Some new_parent;
    node_new_parent.rank <- max node_x.rank node_y.rank + 1)
  else (
    let node_x = TT.find_exn forest root_x in
    let node_new_parent = TT.find_exn forest new_parent in
    node_new_parent.rank <- node_x.rank + 1)
;;

(* Get final parents of each element *)
let get_final_parents (forest : disjoint_set TT.t) =
  let elements = TT.keys forest in
  List.zip_exn elements (List.map ~f:(find forest) elements)
;;
