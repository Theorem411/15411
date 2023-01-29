(*
   * This file defines the interference graph library, which provides functions to 
   * - build interference graph out of liveness range
   * - 
*)

open Core
module Vertex : sig
   type reg = EAX | EDX [@@deriving compare, sexp]
   type t = R of reg | T of Temp.t [@@deriving compare, sexp]
   include Comparable.S with type t := t
end
(** the graph type *)
type t = Vertex.Set.t Vertex.Map.t (* adjacency list *)
(** graph utilities *)
val from_list: (Vertex.t * Vertex.t) list -> Vertex.Set.t Vertex.Map.t
val to_list: t -> (Vertex.t * Vertex.t list) list

val ordering: t -> Vertex.t list
val coloring: t -> (Vertex.t * int) list