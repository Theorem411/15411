(* module T = Tree_l4
module A = Assem_l4

let munch_exp : Temp.t -> T.mpexp -> A.instr list = 
  let rec munch_exp_rev (t : Temp.t) (exp : T.mpexp) : A.instr list = 
    let e, esize = exp in
    match e with
  | T.Const const ->
      
      failwith "Not implemented"
  | T.Temp temp ->
      (* Implement the Temp case *)
      failwith "Not implemented"
  | T.Binop { op; lhs; rhs } ->
      (* Implement the Binop case *)
      failwith "Not implemented"
  | T.Cmpop { op; size; lhs; rhs } ->
      (* Implement the Cmpop case *)
      failwith "Not implemented"
  | T.Unop { op; p } ->
      (* Implement the Unop case *)
      failwith "Not implemented"
  | T.Mem addr ->
      (* Implement the Mem case *)
      failwith "Not implemented"
  | T.Addr addr ->
      (* Implement the Addr case *)
      failwith "Not implemented"
  | T.Alloc size ->
      (* Implement the Alloc case *)
      failwith "Not implemented"
  | T.Calloc { len; typ } ->
      (* Implement the Calloc case *)
      failwith "Not implemented"
  in
  fun t exp -> List.rev (munch_exp_rev t exp)
;;

let munch_stm : T.stm -> A.instr list = function  
| If { cond; lt; lf } ->
  failwith "Not implemented"
| Goto label ->
  failwith "Not implemented"
| Label label ->
  failwith "Not implemented"
| MovEfktExp { dest; ebop; lhs; rhs } ->
  failwith "Not implemented"
| MovPureExp { dest; src } ->
  failwith "Not implemented"
| MovFuncApp { dest; fname; args } ->
  failwith "Not implemented"
| MovToMem { mem; src } ->
  failwith "Not implemented"
| Return mpexp_opt ->
  failwith "Not implemented"
| AssertFail ->
  failwith "Not implemented"
;;
let munch_block : T.block -> A.block = 
  failwith "no"
;;
let codegen : T.program -> A.program = 
  failwith "no"
;;