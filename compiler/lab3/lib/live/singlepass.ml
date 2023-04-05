open Core
module B = Block
module V = Graph.Vertex
module AS = Assem_l4
module IntTable = Hashtbl.Make (Int)

let dump_liveness : bool ref = ref false

type ht_entry =
  { d : V.Set.t
  ; u : V.Set.t
  ; lin : V.Set.t
  ; lout : V.Set.t
  ; instr : AS.instr
  }

type t = (int, ht_entry) Hashtbl.t
type live_t = (int, V.Set.t) Hashtbl.t

let get_init (_ : int) : live_t * live_t =
  let empty_1 : live_t = IntTable.create () in
  let empty_2 : live_t = IntTable.create () in
  empty_1, empty_2
;;

let op_to_vset (op : AS.operand) : V.Set.t =
  match V.op_to_vertex_opt op with
  | Some v -> V.Set.singleton v
  | None -> V.Set.empty
;;

let op_to_vset_mem_mov (op : AS.operand) : V.Set.t =
  match V.op_to_vertex_opt op with
  | Some v -> V.Set.of_list [ v ]
  | None -> V.Set.of_list []
;;

(* let print_vset (es : V.Set.t) =
  print_string "[";
  V.Set.iter
    ~f:(fun v ->
      V.print v;
      print_string ", ")
    es;
  print_string "]"
;; *)
(*_ return def set and uses set  and adds the function params to the stack if*)
let def_n_use (instr : AS.instr) : V.Set.t * V.Set.t =
  match instr with
  | AS.Mov { dest; src; _ } -> op_to_vset dest, op_to_vset src
  | AS.MovSxd { dest; src } -> op_to_vset dest, op_to_vset src
  | AS.MovFrom { dest; src; _ } -> op_to_vset_mem_mov dest, op_to_vset src
  | AS.MovTo { dest; src; _ } ->
    V.Set.empty, V.Set.union_list [ op_to_vset src; op_to_vset dest ]
  | AS.Unop { dest; _ } -> op_to_vset dest, op_to_vset dest
  | AS.PureBinop { dest; lhs; rhs; _ } ->
    op_to_vset dest, V.Set.union_list [ op_to_vset lhs; op_to_vset rhs ]
  | AS.EfktBinop { op = AS.Div | AS.Mod; dest; lhs; rhs } ->
    ( V.Set.union_list (List.map ~f:op_to_vset [ dest; AS.Reg AS.EAX; AS.Reg AS.EDX ])
    , V.Set.union_list [ op_to_vset lhs; op_to_vset rhs ] )
  | AS.EfktBinop { op = AS.ShiftL | AS.ShiftR; dest; lhs; rhs } ->
    ( V.Set.union_list (List.map ~f:op_to_vset [ dest; AS.Reg AS.ECX ])
    , V.Set.union_list [ op_to_vset lhs; op_to_vset rhs ] )
  | AS.Jmp _ | AS.Cjmp _ -> V.Set.empty, V.Set.empty
  | AS.Ret -> V.Set.empty, V.Set.singleton (V.R AS.EAX)
  | AS.Lab _ -> V.Set.empty, V.Set.empty
  | AS.Cmp { lhs; rhs; _ } ->
    V.Set.empty, V.Set.union_list [ op_to_vset lhs; op_to_vset rhs ]
  | AS.AssertFail -> V.Set.empty, V.Set.empty
  | AS.Set { src; _ } ->
    V.Set.union_list (List.map ~f:op_to_vset [ src; AS.Reg AS.EAX ]), V.Set.empty
  (* defines a lot of registers *)
  | AS.Call { args_in_regs; args_overflow; _ } ->
    (* %rdi, %rsi, %rdx, %rcx, %r8, %r9, %rax - caller saved registers *)
    ( V.Set.of_list
        (List.map
           ~f:(fun r -> V.R r)
           [ AS.EAX; AS.EDI; AS.ESI; AS.EDX; AS.ECX; AS.R8D; AS.R9D ])
      (* @ List.map ~f:(fun (r, _) -> V.T r) args_overflow) *)
    , V.Set.of_list
        (List.map ~f:(fun (r, _) -> V.R r) args_in_regs
        @ List.map ~f:(fun (r, _) -> V.T r) args_overflow) )
  | AS.Directive _ | Comment _ -> V.Set.empty, V.Set.empty
  | AS.LoadFromStack stack_args ->
    V.Set.of_list (List.map ~f:(fun (t, _) -> V.T t) stack_args), V.Set.empty
;;

(* let format_v_set s =
  String.concat ~sep:"," (List.map (V.Set.to_list s) ~f:(fun v -> V._to_string v))
;; *)

(* let format_table_entry (k : int) (e : ht_entry) : string =
  let instr_raw = AS.format_instr e.instr in
  let instr = String.slice instr_raw 0 (String.length instr_raw - 1) in
  sprintf
    "%d -> [%s] : d={%s}, u = {%s}, lin={%s}, lout={%s} "
    k
    instr
    (format_v_set e.d)
    (format_v_set e.u)
    (format_v_set e.lin)
    (format_v_set e.lout)
;; *)

(* let print_table (table : t) ~lines : string =
  let keys =
    match lines with
    | None -> IntTable.keys table
    | Some i -> List.map ~f:(fun (n, _) -> n) i
  in
  let keys = List.sort ~compare:Int.compare keys in
  String.concat
    ~sep:"\n"
    (List.map
       ~f:(fun k ->
         let v = IntTable.find_exn table k in
         format_table_entry k v)
       keys)
;; *)

let initialize_blocks table (fargs : Temp.t list) (x : B.block) =
  let b = x.block in
  List.iter b ~f:(fun (i, instr) ->
      let d, u = def_n_use instr in
      (* of load from stack add temp args to define *)
      let d =
        match instr with
        | AS.LoadFromStack _ ->
          let fargs_def = V.Set.of_list (List.map ~f:(fun t -> V.T t) fargs) in
          V.Set.union fargs_def d
        | _ -> d
      in
      let record = { d; u; lin = V.Set.empty; lout = V.Set.empty; instr } in
      Hashtbl.add_exn table ~key:i ~data:record)
;;

let init_table (f : B.fspace) =
  let table : t = IntTable.create () in
  let helper = initialize_blocks table f.args in
  List.iter f.fdef_blocks ~f:helper;
  table
;;

let update_info table (live_in, live_out) i =
  let lin = IntTable.find_exn live_in i in
  let lout = IntTable.find_exn live_out i in
  IntTable.update table i ~f:(fun v ->
      match v with
      | Some info -> { d = info.d; u = info.u; lin; lout; instr = info.instr }
      | None -> failwith ("Can not find entry for " ^ Int.to_string i))
;;

let handle_instrs
    (table : (int, ht_entry) Hashtbl.t)
    ((instrs, args) : (int * AS.instr) list * Temp.t list option)
    (input : V.Set.t)
    : V.Set.t
  =
  let n = List.length instrs in
  (* let () = prerr_endline ("n = " ^ Int.to_string n) in  *)
  let indicies = List.map ~f:(fun (i, _) -> i) instrs in
  let first_index = List.nth_exn indicies 0 in
  let last_index = first_index + n - 1 in
  (* gets current live_in and live_out, updates them accordingly *)
  let (live_in, live_out) : live_t * live_t = get_init n in
  let liveness_aux (i, _) () =
    let info = IntTable.find_exn table i in
    (* the first line in the function defintion has to define all the args *)
    let d =
      if equal first_index i
      then
        V.Set.union
          info.d
          (V.Set.of_list (List.map ~f:(fun t -> V.T t) (Option.value args ~default:[])))
      else info.d
    in
    (* let all_succs = succ i in *)
    let live_out_i =
      (* let () = prerr_endline ("getting liveout = " ^ Int.to_string i ^ " where last_index is " ^ Int.to_string (last_index) ) in  *)
      match i = last_index with
      | true -> input
      | false -> IntTable.find_exn live_in (i + 1)
    in
    let live_in_i = V.Set.union info.u (V.Set.diff live_out_i d) in
    let () =
      match IntTable.add live_in ~key:i ~data:live_in_i with
      | _ -> ()
    in
    let () =
      match IntTable.add live_out ~key:i ~data:live_out_i with
      | _ -> ()
    in
    ()
  in
  let () = List.fold_right instrs ~f:liveness_aux ~init:() in
  let update = update_info table (live_in, live_out) in
  let () = List.iter ~f:(fun i -> update i) indicies in
  (* let last = List.nth_exn indicies first_index in *)
  IntTable.find_exn live_in first_index
;;

(* let singlepass : (int, ht_entry) Hashtbl.t -> B.fspace -> V.Set.t -> V.Set.t *)
let singlepass (table : (int, ht_entry) Hashtbl.t) (b : B.block) (input : V.Set.t)
    : V.Set.t
  =
  (* handling arguments *)
  let args, black_list =
    match b.label with
    | Label.BlockLbl _ -> None, V.Set.empty
    | Label.FunName { args; _ } ->
      ( Some args
      , V.Set.of_list
          (List.map ~f:(fun t -> V.T t) args
          @ List.map
              ~f:(fun r -> V.R r)
              [ AS.EDI; AS.ESI; AS.EDX; AS.ECX; AS.R8D; AS.R9D ]) )
  in
  let out_raw = handle_instrs table (b.block, args) input in
  let out = V.Set.diff out_raw black_list in
  out
;;

let get_block_vertices (b : B.block) =
  let instrs = List.map ~f:(fun (_, instr) -> instr) b.block in
  let args =
    match b.label with
    | Label.BlockLbl _ -> V.Set.empty
    | Label.FunName { args; _ } -> V.Set.of_list (List.map ~f:(fun t -> V.T t) args)
  in
  let set_list =
    V.Set.union_list
      (List.concat_map
         ~f:(fun instr ->
           let d, u = def_n_use instr in
           [ d; u ])
         instrs)
  in
  V.Set.union args set_list
;;

let get_all_vertices (b : B.fspace) =
  V.Set.union_list (List.map b.fdef_blocks ~f:get_block_vertices)
;;

let cartesian l l' =
  List.concat (List.map ~f:(fun e -> List.map ~f:(fun e' -> e, e') l') l)
;;

let get_edges (table : t) : (V.t * V.t) list =
  let f ~key:_ ~data:info acc =
    let d, lout, lin =
      V.Set.to_list info.d, V.Set.to_list info.lout, V.Set.to_list info.lin
    in
    let defined_regs =
      List.filter d ~f:(fun v ->
          match v with
          | V.R _ -> true
          | V.T _ -> false)
    in
    (* remove d lin later *)
    let instr_specific_edges =
      match info.instr with
      | AS.EfktBinop { dest; lhs; rhs; _ } ->
        let d, l, r =
          V.op_to_vertex_opt dest, V.op_to_vertex_opt lhs, V.op_to_vertex_opt rhs
        in
        List.filter_map
          ~f:(fun (a, b) ->
            match a, b with
            | Some(x), Some(y) -> Some(x, y)
            | _ -> None)
          [ d, l; d, r ]
      | _ -> []
    in
    let out_ed, defined_reg_edges = cartesian d lout, cartesian defined_regs lin in
    List.concat [ out_ed; defined_reg_edges; instr_specific_edges; acc ]
  in
  let init = ([] : (V.t * V.t) list) in
  IntTable.fold table ~init ~f
;;

let get_edges_vertices t (b : B.fspace) =
  let args =
    List.filter_opt (List.map ~f:(fun t -> V.op_to_vertex_opt (AS.Temp t)) b.args)
  in
  let arg_edges = cartesian args args in
  let e = arg_edges @ get_edges t in
  let v = get_all_vertices b in
  v, e
;;

let uses_defs_block (b : B.block) =
  let fargs : Temp.t list =
    match b.label with
    | Label.BlockLbl _ -> []
    | Label.FunName f -> f.args
  in
  (* basically (def,use) of a line, but adds function arguments into  *)
  let local_def_n_use instr =
    let dx, ux = def_n_use instr in
    (* of load from stack add temp args to define *)
    let d =
      match instr with
      | AS.LoadFromStack _ ->
        let fargs_def = V.Set.of_list (List.map ~f:(fun t -> V.T t) fargs) in
        V.Set.union fargs_def dx
      | _ -> dx
    in
    d, ux
  in
  let ub, db = V.Set.empty, V.Set.empty in
  let ufinal, dfinal =
    List.fold b.block ~init:(ub, db) ~f:(fun (ub, db) (_, instr) ->
        let dl, ul = local_def_n_use instr in
        V.Set.union ub (V.Set.diff ul db), V.Set.union db dl)
  in
  ufinal, dfinal
;;
