(*
   * This file defines the interference graph library, which provides functions to 
   * - build interference graph out of liveness range
   * - 
*)

open Core
module AS = Assem_l4

module Vertex : sig
  type reg [@@deriving compare, sexp]

  type t =
    | R of AS.reg
    | T of Temp.t
  [@@deriving compare, sexp, hash]

  include Comparable.S with type t := t

  val op_to_vertex_opt : AS.operand -> t option
  val _to_string : t -> string
  val print : t -> unit
end

module VertexTable : Hashtbl.S with type key = Vertex.t

(*_ the graph type *)
type t = Vertex.Set.t Vertex.Map.t (* adjacency list *)

(*_ graph utilities *)
val to_list : t -> (Vertex.t * Vertex.t list) list
val coloring : t -> temp_weight:(Vertex.t -> int option) -> int Vertex.Map.t

(*_ liveness *)
(* val mk_interfere_graph : AS.instr list -> t *)
type new_graph = Vertex.Set.t VertexTable.t

(* val can_coalesce : new_graph -> Vertex.t -> Vertex.t -> bool *)
(* coalesce two vertices into third vertex and return the new vertex set *)
val coalesce : new_graph -> Vertex.t * Vertex.t -> Vertex.t -> Vertex.Set.t

(*_ debug printing *)
val print : t -> unit