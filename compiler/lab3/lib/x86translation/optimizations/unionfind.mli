type disjoint_set
open Core
module T = Temp

module TT : Hashtbl.S with type key = Temp.t
type forest =  disjoint_set TT.t

val create_forest : unit -> forest
val add_vertex : forest -> Temp.t -> unit
val find : forest -> Temp.t -> Temp.t
val union : forest -> Temp.t -> Temp.t -> unit
val union_to : forest -> Temp.t * Temp.t -> Temp.t -> unit
val get_final_parents : forest -> (Temp.t * Temp.t) list
