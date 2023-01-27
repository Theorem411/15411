(*
   * This file defines the interference graph library, which provides functions to 
   * - build interference graph out of liveness range
   * - 
*)
type vertex
(* defines the graph type *)
type t

(* the color type *)
type color

(* returns a vertex ordering for greedy graph coloring*)
val ordering: t -> vertex list

(* create a new color *)
val coloring: t -> (vertex * color) list

val to_list: t -> (vertex * vertex list) list
