(*
   * This file defines the interference graph library, which provides functions to 
   * - build interference graph out of liveness range
   * - 
*)

open Core

module AS = Assem
module Vertex : sig
   type reg [@@deriving compare, sexp]
   type t = R of AS.reg | T of Temp.t [@@deriving compare, sexp, hash]
   include Comparable.S with type t := t
   val op_to_vertex_opt : AS.operand -> t option
   val print : t -> unit
end
(*_ the graph type *)
type t = Vertex.Set.t Vertex.Map.t (* adjacency list *)
(*_ graph utilities *)
val to_list: t -> (Vertex.t * Vertex.t list) list
val coloring: t -> (Vertex.t * int) list
(*_ liveness *)
val mk_interfere_graph : AS.instr list -> t

(*_ debug printing *)
val print : t -> unit