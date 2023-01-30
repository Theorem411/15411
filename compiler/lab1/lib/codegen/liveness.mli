(*
   * This file contains liveness functionality, particularly the dataflow analysis
   * - 
   * - 
*)

val live_out : Assem.instr list -> 
val live_in : Assem.instr list -> 

val live_range : Assem.instr list -> int * int list