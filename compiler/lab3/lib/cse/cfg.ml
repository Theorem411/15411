open Core

(*_ cfg main type *)
module L = Label

module LKey = struct
  type t = L.bt [@@deriving compare, equal, sexp, hash]
end

module LComp = Comparable.Make (LKey)
module LM = LComp.Map
module LS = LComp.Set
module LH = Hashtbl.Make (LKey)
module IComp = Comparable.Make (Int)
module IM = IComp.Map

(*_ type definition *)
type t =
  { entry : L.bt
  ; graph : LS.t LM.t
  ; preds : LS.t LM.t
  ; rev_post_order_i2b : L.bt IM.t
  ; rev_post_order_b2i : int LM.t
  }

type edge_t =
  { pred : L.bt
  ; chld : L.bt
  }

type cfg_input =
  { entry : L.bt
  ; nodes : L.bt list
  ; edges : edge_t list
  }

type cfg_output =
  { cfg : t
  ; dead : L.bt list
  }

let pp_edge_t { pred; chld } : string = sprintf "%s->%s" (L.name_bt pred) (L.name_bt chld)

let pp_graph_hashtbl (graph : L.bt list LH.t) : string =
  let graph' = LH.to_alist graph in
  sprintf
    "{\n%s\n}\n"
    (List.map graph' ~f:(fun (l, nbrs) ->
       sprintf "%s:%s" (L.name_bt l) (List.map nbrs ~f:L.name_bt |> String.concat ~sep:","))
    |> String.concat ~sep:"\n")
;;

let pp_graph (graph : LS.t LM.t) : string =
  let graph' = LM.to_alist graph in
  sprintf
    "{\n%s\n}\n"
    (List.map graph' ~f:(fun (l, nbrs) ->
       sprintf
         "%s:%s"
         (L.name_bt l)
         (List.map (LS.to_list nbrs) ~f:L.name_bt |> String.concat ~sep:","))
    |> String.concat ~sep:"\n")
;;

let pp_cfg_input { entry; nodes; edges } : string =
  sprintf
    "entry = %s; \nnodes = %s; \nedges = %s\n"
    (Label.name_bt entry)
    (List.map nodes ~f:Label.name_bt |> String.concat ~sep:", ")
    (List.map edges ~f:pp_edge_t |> String.concat ~sep:", ")
;;

let pp_rev_post_order_i2b (i2b : L.bt IM.t) : string = 
  let i2b = IM.to_alist i2b in
  List.map i2b ~f:(fun (i, l) -> sprintf "%i:%s" i (Label.name_bt l)) |> String.concat ~sep:", "
;;

let pp_rev_post_order_b2i (b2i : int LM.t) : string = 
  let b2i = LM.to_alist b2i in
  List.map b2i ~f:(fun (l, i) -> sprintf "%s:%i" (Label.name_bt l) i) |> String.concat ~sep:", "
;;
(*_ helper functions zone *)
let create_graph_and_preds (nodes : L.bt list) (edges : edge_t list)
  : LS.t LM.t * LS.t LM.t
  =
  (*_ init *)
  let pred2chld = List.map edges ~f:(fun { pred; chld } -> pred, chld) in
  let chld2pred = List.map edges ~f:(fun { pred; chld } -> chld, pred) in
  let preds = LH.of_alist_multi chld2pred in
  let graph = LH.of_alist_multi pred2chld in
  (* let () = printf "graph is init to %s\n" (pp_graph_hashtbl graph) in *)
  (*_ fill in missing nodes *)
  let touch htbl k =
    LH.update htbl k ~f:(fun vopt ->
      match vopt with
      | None -> []
      | Some vlst -> vlst)
  in
  let () =
    List.iter nodes ~f:(fun l ->
      touch preds l;
      touch graph l)
  in
  (* let () = printf "post-process graph becomes %s\n" (pp_graph_hashtbl graph) in *)
  (*_ convert into graph type *)
  let preds_lst = LH.to_alist preds in
  let graph_lst = LH.to_alist graph in
  let preds_lst' = List.map preds_lst ~f:(fun (l, lst) -> l, LS.of_list lst) in
  let graph_lst' = List.map graph_lst ~f:(fun (l, lst) -> l, LS.of_list lst) in
  let preds = LM.of_alist_exn preds_lst' in
  let graph = LM.of_alist_exn graph_lst' in
  (* let () = printf "ship-off graph = %s\n" (pp_graph graph) in *)
  graph, preds
;;

let rev_post_order (entry : L.bt) (graph : LS.t LM.t) : L.bt IM.t * int LM.t * L.bt list =
  (* let () = printf "what is graph?\n %s\n" (pp_graph graph)in *)
  let rec dfs v visited =
    if LS.mem visited v
    then (
      (* let () = printf "revisit %s\n" (L.name_bt v) in *)
      [], visited)
    else (
      (* let () = printf "visit %s for the first time\n" (L.name_bt v) in *)
      let nbrs = LM.find_exn graph v |> LS.to_list in
      let visited' = LS.add visited v in
      (* let () =
        printf
          "proceed visiting %s\n"
          (List.map nbrs ~f:L.name_bt |> String.concat ~sep:", ")
      in *)
      let rev_post_order, visited'' =
        List.fold nbrs ~init:([], visited') ~f:(fun (res, vst) u ->
          let res', vst' = dfs u vst in
          res' @ res, vst')
      in
      (* let () = printf "finish %s \n" (L.name_bt v) in *)
      v :: rev_post_order, visited'')
  in
  let rev_post_ordering, visited = dfs entry LS.empty in
  (*_ post processing *)
  let i_rev_post_ordering = List.mapi rev_post_ordering ~f:(fun i l -> i, l) in
  let b_rev_post_ordering = List.map i_rev_post_ordering ~f:(fun (i, b) -> b, i) in
  let i2b = IM.of_alist_exn i_rev_post_ordering in
  let b2i = LM.of_alist_exn b_rev_post_ordering in
  let dead = LM.keys (LM.filter_keys graph ~f:(fun l -> not (LS.mem visited l))) in
  (* let () =
    printf "who's dead? %s\n" (List.map dead ~f:L.name_bt |> String.concat ~sep:", ")
  in *)
  i2b, b2i, dead
;;

(*_ lib function implementations *)
let create { entry; nodes; edges } : cfg_output =
  let graph, preds = create_graph_and_preds nodes edges in
  let rev_post_order_i2b, rev_post_order_b2i, dead = rev_post_order entry graph in
  let cfg = { entry; rev_post_order_i2b; rev_post_order_b2i; graph; preds } in
  { cfg; dead }
;;

let i2b ({ rev_post_order_i2b; _ } : t) (idx : int) : L.bt =
  IM.find_exn rev_post_order_i2b idx
;;

let b2i ({ rev_post_order_b2i; _ } : t) (bid : L.bt) : int =
  LM.find_exn rev_post_order_b2i bid
;;

let preds_i ({ rev_post_order_i2b; rev_post_order_b2i; preds; _ } : t) (idx : int)
  : int list
  =
  let bid = IM.find_exn rev_post_order_i2b idx in
  let preds_b = LM.find_exn preds bid |> LS.to_list in
  List.map preds_b ~f:(fun b -> LM.find_exn rev_post_order_b2i b)
;;

let preds_b ({ preds; _ } : t) (bid : L.bt) : LS.t = LM.find_exn preds bid
