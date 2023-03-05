open Core
module A = Assem_new

type block_label_t =
  | Label of Label.t
  | FunName of (Symbol.t * Temp.t list)

type jump_tag_t =
  | JRet
  | JCon of
      { jt : Label.t
      ; jf : Label.t
      }
  | JUncon of Label.t

type block =
  { label : block_label_t
  ; block : A.instr list
  ; jump : jump_tag_t
  }

type fspace_block =
  { fname : Symbol.t
  ; args : Temp.t list
  ; fdef_block : block list
  }

type block_program = fspace_block list

let is_jump (i : A.instr) : bool =
  match i with
  | A.Ret | A.Jmp _ | A.Cjmp _ -> true
  | _ -> false
;;

let rec helper (a : A.instr list list) (rest : A.instr list) =
  match rest with
  | [] -> a
  | _ ->
    let block = List.take_while rest ~f:(fun i -> i |> is_jump) in
    let rest' = List.drop rest (List.length block) in
    if List.length block = 0 then failwith "empty?";
    let () =
      printf "[%s]\n" (List.map block ~f:A.format_instr |> String.concat ~sep:"\n")
    in
    helper (block :: a) rest'
;;

let test (f : A.fspace) : unit =
  let () = printf "%s:" (Symbol.name f.fname) in 
  let (_ : A.instr list list) = helper [] f.fdef in
  ()
;;

let of_block (_ : A.fspace) : fspace_block = failwith "not imp"

let block_former (funcs : A.fspace list) : block_program = 
  let () = List.iter funcs ~f:test in
  List.map ~f:of_block funcs
