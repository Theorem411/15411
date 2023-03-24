open Core
module B = Block
module V = Graph.Vertex
module AS = Assem
module IntTable = Hashtbl.Make (Int)

let dump_liveness : bool ref = ref false
(* let print_info = if !dump_liveness then prerr_endline else prerr_endline *)

let print_info _ = ();;

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

(* let print_vset (es : V.Set.t) =
  print_string "[";
  V.Set.iter
    ~f:(fun v ->
      V.print v;
      print_string ", ")
    es;
  print_string "]"
;; *)

let op_to_v_exn (op : AS.operand) : V.t =
  match V.op_to_vertex_opt op with
  | Some v -> v
  | None -> failwith "NONE is given to op_to_v"
;;

(*_ return def set and uses set  and adds the function params to the stack if*)
let def_n_use (instr : AS.instr) : V.Set.t * V.Set.t =
  let op_to_vset (op : AS.operand) : V.Set.t =
    match V.op_to_vertex_opt op with
    | Some v -> V.Set.singleton v
    | None -> V.Set.empty
  in
  match instr with
  | AS.Mov { dest; src } -> op_to_vset dest, op_to_vset src
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
  | AS.Cmp (o1, o2) -> V.Set.empty, V.Set.union_list [ op_to_vset o1; op_to_vset o2 ]
  | AS.AssertFail -> V.Set.empty, V.Set.empty
  | AS.Set { src; _ } ->
    V.Set.union_list (List.map ~f:op_to_vset [ src; AS.Reg AS.EAX ]), V.Set.empty
  (* defines a lot of registers *)
  | AS.Call { args_in_regs; _ } ->
    ( V.Set.of_list
        (List.map
           ~f:(fun r -> V.R r)
           [ AS.EAX; AS.EDI; AS.ESI; AS.EDX; AS.ECX; AS.R8D; AS.R9D; AS.R10D; AS.R11D ])
    , V.Set.of_list (List.map ~f:(fun r -> V.R r) args_in_regs) )
  | AS.Directive _ | Comment _ -> V.Set.empty, V.Set.empty
  | AS.LoadFromStack stack_args ->
    (* ADD REGISTERS TO USE. *)
    V.Set.of_list (List.map ~f:(fun t -> V.T t) stack_args), V.Set.empty
;;

let format_v_set s =
  String.concat ~sep:"," (List.map (V.Set.to_list s) ~f:(fun v -> V._to_string v))
;;

let format_table_entry (k : int) (e : ht_entry) : string =
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
;;

let print_table (table : t) : string =
  let keys = IntTable.keys table in
  let keys = List.sort ~compare:Int.compare keys in
  String.concat
    ~sep:"\n"
    (List.map
       ~f:(fun k ->
         let v = IntTable.find_exn table k in
         format_table_entry k v)
       keys)
;;

let initialize_blocks table (fargs : Temp.t list) (x : B.block) =
  let b = x.block in
  List.iter b ~f:(fun (i, instr) ->
      let d, u = def_n_use instr in
      (* of load from stack add temp args to define *)
      let d, u =
        match instr with
        | AS.LoadFromStack _ ->
          let fargs_def = V.Set.of_list (List.map ~f:(fun t -> V.T t) fargs) in
          let u =
            V.Set.of_list
              (List.mapi ~f:(fun i _ -> op_to_v_exn (AS.Reg (AS.arg_i_to_reg i))) fargs)
          in
          V.Set.union fargs_def d, u
        | _ -> d, u
      in
      let record = { d; u; lin = V.Set.empty; lout = V.Set.empty; instr } in
      Hashtbl.add_exn table ~key:i ~data:record)
;;

let init_table (f : B.fspace_block) =
  let table : t = IntTable.create () in
  let helper = initialize_blocks table f.args in
  List.iter f.fdef_block ~f:helper;
  print_info (print_table table);
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
      then V.Set.of_list (List.map ~f:(fun t -> V.T t) (Option.value args ~default:[]))
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

(* let singlepass : (int, ht_entry) Hashtbl.t -> B.fspace_block -> V.Set.t -> V.Set.t *)
let singlepass (table : (int, ht_entry) Hashtbl.t) (b : B.block) (input : V.Set.t)
    : V.Set.t
  =
  let () = print_info ("doing now: " ^ B.format_block b) in
  let () =
    print_info
      ("input:["
      ^ String.concat
          ~sep:","
          (List.map (V.Set.to_list input) ~f:(fun v -> V._to_string v))
      ^ "]")
  in
  (* handling arguments *)
  let args, black_list =
    match b.label with
    | Block.BlockLabel _ -> None, V.Set.empty
    | Block.FunName (_, args) ->
      Some args, V.Set.of_list (List.map ~f:(fun t -> V.T t) args)
  in
  let out_raw = handle_instrs table (b.block, args) input in
  let out = V.Set.diff out_raw black_list in
  print_info
    ("output:["
    ^ String.concat ~sep:"," (List.map (V.Set.to_list out) ~f:V._to_string)
    ^ "]\n\n\n\n\n");
  print_info (print_table table);
  out
;;

let get_block_vertices (b : B.block) =
  let instrs = List.map ~f:(fun (_, instr) -> instr) b.block in
  let args =
    match b.label with
    | Block.BlockLabel _ -> V.Set.empty
    | Block.FunName (_, args) -> V.Set.of_list (List.map ~f:(fun t -> V.T t) args)
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

let get_all_vertices (b : B.fspace_block) =
  V.Set.union_list (List.map b.fdef_block ~f:get_block_vertices)
;;

let get_edges (table : t) : (V.t * V.t) list =
  let f ~key ~data:info acc =
    let d, lout, lin =
      V.Set.to_list info.d, V.Set.to_list info.lout, V.Set.to_list info.lin
    in
    let cartesian l l' =
      List.concat (List.map ~f:(fun e -> List.map ~f:(fun e' -> e, e') l') l)
    in
    let out_ed, in_ed = cartesian d lout, cartesian d lin in
    print_info
      (sprintf
         "[%d] edges: [%s]"
         key
         (String.concat
            ~sep:","
            (List.map (out_ed @ in_ed) ~f:(fun (a, b) ->
                 V._to_string a ^ "-" ^ V._to_string b))));
    List.concat [ out_ed; in_ed; acc ]
  in
  let init = ([] : (V.t * V.t) list) in
  IntTable.fold table ~init ~f
;;

let get_edges_vertices t b =
  let e = get_edges t in
  let v = get_all_vertices b in
  v, e
;;