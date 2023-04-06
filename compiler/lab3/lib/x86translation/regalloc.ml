open Core
module AS = Assem_l4
module V = Graph.Vertex
module VM = Graph.Vertex.Map
module R = Register
module Live = Live_faster
module CM = Map.Make (Int)
module CS = Set.Make (Int)
module TM = Temp.Map
module VT = Hashtbl.Make (V)
module Prespill = Prespill
module Coalesce = Coalesce

module REnum = struct
  type t = AS.reg [@@deriving compare, equal, sexp]
end

module RM = Map.Make (REnum)
module RS = Set.Make (REnum)

let asr2renum : AS.reg -> R.reg_enum = function
  | AS.EAX -> R.EAX
  | AS.EBX -> R.EBX
  | AS.ECX -> R.ECX
  | AS.EDI -> R.EDI
  | AS.EDX -> R.EDX
  | AS.ESI -> R.ESI
  | AS.RSP -> R.RSP
  | AS.RBP -> R.RBP
  | AS.R8D -> R.R8D
  | AS.R9D -> R.R9D
  | AS.R10D -> R.R10D
  | AS.R11D -> R.R11D
  | AS.R12D -> R.R12D
  | AS.R13D -> R.R13D
  | AS.R14D -> R.R14D
  | AS.R15D -> R.R15D
;;

(* let no_reg_alloc = true *)
(* let should_print_reg_map = false *)

type color = int

(*_ helper functions *)
let __v2op (v : V.t) : AS.operand =
  match v with
  | V.T t -> AS.Temp t
  | V.R r -> AS.Reg r
;;

type reg_n_temps =
  { reg : AS.reg option
  ; temps : Temp.t list
  }

type reg_or_spill =
  | Reg of R.reg_enum
  | Spl of int

let reg (r : AS.reg) = Reg (asr2renum r)

let __c2v (v2c : color VM.t) : reg_n_temps CM.t =
  let c2v_lst = VM.to_alist v2c |> List.map ~f:(fun (x, y) -> y, x) in
  let init = { reg = None; temps = [] } in
  let foldf { reg; temps } v =
    match v with
    | V.T t -> { reg; temps = t :: temps }
    | V.R r ->
      (match reg with
      | None -> { reg = Some r; temps }
      | Some _ -> failwith "regalloc: impossible situation")
  in
  let c2v = CM.of_alist_fold ~init ~f:foldf c2v_lst in
  c2v
;;

let __r2c (c2v : reg_n_temps CM.t) : color RM.t =
  let c2rt = CM.to_alist c2v in
  let mapf (c, { reg; _ }) =
    match reg with
    | None -> None
    | Some r -> Some (r, c)
  in
  let c2rt' = List.map c2rt ~f:mapf |> List.filter_opt in
  let r2c = RM.of_alist_exn c2rt' in
  r2c
;;

let __pc2pr (c2v : reg_n_temps CM.t) : reg_or_spill CM.t =
  CM.filter_mapi c2v ~f:(fun ~key:_ ~data:rnt -> Option.map rnt.reg ~f:(fun r -> reg r))
;;

let __t2c (c2v : reg_n_temps CM.t) : color TM.t =
  let c2v' = CM.to_alist c2v in
  let mapf (c, { temps; _ }) = List.map temps ~f:(fun t -> t, c) in
  let t2c = List.map c2v' ~f:mapf |> List.concat in
  TM.of_alist_exn t2c
;;

let __free_colors (c2v : reg_n_temps CM.t) : color list =
  (*_ return an increasing list of colors that don't have a precolor *)
  let c2v' =
    CM.filter_mapi c2v ~f:(fun ~key:c ~data:{ reg; _ } ->
        match reg with
        | None -> Some c
        | Some _ -> None)
  in
  CM.keys c2v' |> List.sort ~compare:Int.compare
;;

let __free_regs (c2v : reg_n_temps CM.t) : reg_or_spill list =
  let pre_regs =
    CM.data c2v |> List.filter_map ~f:(fun { reg; _ } -> reg) |> RS.of_list
  in
  let free_regs =
    List.filter_map AS.all_regs ~f:(fun r ->
        if RS.mem pre_regs r then None else Some (reg r))
  in
  free_regs
;;

type t =
  { reg_spill_map : reg_or_spill TM.t
  ; updater : Temp.t -> reg_or_spill
  }

let reg_alloc (fspace : AS.fspace) : t =
  (*_ think of three layers: 1. the temps 2. the colors 3. the registers *)
  (*_ do the coloring magic: produce (T/R) to c mapping *)
  let graph_tbl = Live.mk_graph_fspace (Block.of_fspace fspace) in
  (*  *)
  (* Prespill modifies the graph_tbl *)
  let stack_map, prespill_count = Prespill.prespill graph_tbl in
  let spl_map = TM.map ~f:(fun i -> Spl i) stack_map in
  let v2c = Graph.coloring (V.Map.of_hashtbl_exn graph_tbl) in
  (*  *)
  (* Coalesce modifies the graph_tbl and v2c_tbl *)
  let v2c_tbl = VT.of_alist_exn (VM.to_alist v2c) in
  let moves : (V.t * V.t) list = [] in
  let coalesce_map : V.t TM.t = Coalesce.coalesce graph_tbl v2c_tbl moves in
  (* done coealescing, doing the old algorithm *)
  let v2c = VM.of_hashtbl_exn v2c_tbl in
  let c2v = __c2v v2c in
  (*_ t2c : map temp to the their colors*)
  let t2c = __t2c c2v in
  (*_ c2r : map color to either registers or spilled positions *)
  let pc2pr = __pc2pr c2v in
  let free_colors = __free_colors c2v in
  let free_regs = __free_regs c2v in
  let c2r, rem = List.zip_with_remainder free_colors free_regs in
  let c2r =
    match rem with
    | None -> c2r
    | Some (First cs) ->
      let c2spl = List.mapi cs ~f:(fun i c -> c, Spl (i + prespill_count)) in
      c2r @ c2spl
    | Some (Second _) -> c2r
  in
  let c2r = CM.of_alist_exn c2r in
  let c2r = CM.of_alist_exn (CM.to_alist c2r @ CM.to_alist pc2pr) in
  (* combine precolor with postcolor*)
  (*_ compose t2c tith c2r to get t2r_or_spl *)
  let t2r = TM.mapi t2c ~f:(fun ~key:_ ~data:c -> CM.find_exn c2r c) in
  let updater (t : Temp.t) =
    match TM.find spl_map t with
    | Some spill -> spill
    | None ->
      (match TM.find coalesce_map t with
      | None -> TM.find_exn t2r t
      | Some (V.R reg) -> Reg (asr2renum reg)
      | Some (V.T parent) -> TM.find_exn t2r parent)
  in
  { reg_spill_map = t2r; updater }
;;

(* gives number of memory cells used by finding max
   spl index and adding 1. If no used, then returns 0 *)
let mem_count (t2r : reg_or_spill TM.t) : int =
  let idx_list =
    TM.filter_map t2r ~f:(fun r_or_spl ->
        match r_or_spl with
        | Reg _ -> None
        | Spl j -> Some j)
    |> TM.to_alist
    |> List.map ~f:(fun (_, c) -> c)
  in
  let max_spl_idx = Option.value ~default:(-1) (List.max_elt ~compare idx_list) in
  max_spl_idx + 1
;;

let caller_save (t2r : reg_or_spill TM.t) : R.reg_enum list =
  TM.filter_map t2r ~f:(fun r_or_spl ->
      match r_or_spl with
      | Reg r -> Some r
      | _ -> None)
  |> TM.data
  |> List.filter ~f:R.caller_saved
  |> List.dedup_and_sort ~compare:R.compare_reg_enum
;;

let callee_save (t2r : reg_or_spill TM.t) : R.reg_enum list =
  TM.filter_map t2r ~f:(fun r_or_spl ->
      match r_or_spl with
      | Reg r -> Some r
      | _ -> None)
  |> TM.data
  |> List.filter ~f:R.callee_saved
  |> List.dedup_and_sort ~compare:R.compare_reg_enum
;;

let pp_temp_map (t2r : reg_or_spill TM.t) : string =
  let t2r = TM.to_alist t2r in
  let pp_r_or_spl = function
    | Reg r -> R.format_reg_32 r
    | Spl i -> sprintf "spl[%s]" (Int.to_string i)
  in
  let res =
    List.map t2r ~f:(fun (t, rot) -> sprintf "%s:%s" (Temp.name t) (pp_r_or_spl rot))
  in
  sprintf "{%s\n}\n" (String.concat res ~sep:",")
;;