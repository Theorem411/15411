open Core
module T = Tree_l4

module SM = Symbol.Map 


type fspace_target = 
  { fname : Symbol.t 
  ; args : (Temp.t * int) list
  ; fdef : T.stm list
  ; return : T.mpexp option 
  }

let is_target (fspace : T.fspace_block) : fspace_target option = 
  failwith "no"
;;
let split_program (program : T.program) : fspace_target SM.t * T.fspace_block list = 
  failwith "no"
;;

let inline (prog : T.program) : T.program = 

  failwith "no"
;;