module T = Tree_l4
module A = Assem_l4


let temp (t, size : Temp.t * int) : A.operand = 
  A.Local (A.Temp t), size
;;

let reg (r, size : A.reg * int) : A.operand = 
  A.Local (A.Reg r), size
;;

let imm (n : Int32.t) : A.operand = 
  A.Imm n, 4 
;;

let mem (addr, size : A.addr * int) : A.operand =
  A.Remote addr, size
;;

let addr (addr : A.addr) : A.operand = 
  A.Addr addr, 8
;;


let cogen : T.program -> A.program = failwith "not"