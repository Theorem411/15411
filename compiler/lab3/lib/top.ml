(* L1 Compiler
 * Top Level Environment
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 * Modified: Nick Roberts <nroberts@alumni.cmu.edu>
 *   - Use Cmdliner instead of Getopt for command-line parsing.
 * Modified: Henry Nelson <hcnelson99@gmail.com>
 *   - Switch from ocamlbuild to dune 2.7
 *   - TODO: Add support for expect tests
 *   - Update to Core v0.14
 *)

open Core
module TranslationM = Trans_l4
module Aste = Aste_l4
module AssemM = Assem_l4
module Cogen = Codegen_l4
module TreeM = Tree_l4
module Asts = Asts
module Statsem = Statsem_l4

(* Command line arguments *)

module Opt_level = struct
  type t = Opt_none

  let show = function
    | Opt_none -> "O0"
  ;;

  let parse = function
    | "0" -> Result.Ok Opt_none
    | "1" -> Result.Ok Opt_none
    | "2" -> Result.Error (`Msg "Error: -O2 unimplemented (lab 5)")
    | arg -> Result.Error (`Msg ("Error: Unknown --opt arg: " ^ arg))
  ;;

  let conv =
    let print ppf opt = Format.fprintf ppf "%s" (show opt) in
    Cmdliner.Arg.conv (parse, print)
  ;;
end

module Emit = struct
  type t =
    | X86_64
    | Abstract_assem

  let show = function
    | X86_64 -> "x86-64"
    | Abstract_assem -> "abs"
  ;;

  let parse = function
    | "abs" -> Result.Ok Abstract_assem
    | "x86-64" -> Result.Ok X86_64
    | arg -> Result.Error (`Msg ("Unknown emit arg: " ^ arg))
  ;;

  let conv =
    let print ppf emit = Format.fprintf ppf "%s" (show emit) in
    Cmdliner.Arg.conv (parse, print)
  ;;
end

module HeaderFilename = struct
  type t = string

  let show (x : t) : string = x

  let parse = function
    | arg -> Result.Ok arg
  ;;

  let conv =
    let print ppf emit = Format.fprintf ppf "%s" (show emit) in
    Cmdliner.Arg.conv (parse, print)
  ;;
end

(* let defaul: t = "../runtime/15411-l3.h0";; *)

type cmd_line_args =
  { verbose : bool
  ; dump_parsing : bool
  ; dump_ast : bool
  ; dump_ir : bool
  ; inline : bool
  ; dump_assem : bool
  ; dump_ssa : bool
  ; typecheck_only : bool
  ; regalloc_only : bool
  ; emit : Emit.t
  ; unsafe : bool
  ; opt_level : Opt_level.t
  ; filename : string
  ; header_filename : string
  }

(* A term (using the vocabulary of the Cmdliner library) that can be used to
 * parse command-line arguments. *)
let cmd_line_term : cmd_line_args Cmdliner.Term.t =
  let open Cmdliner in
  (* See https://github.com/janestreet/ppx_let *)
  (* This allows a more convenient syntax for using the Cmdliner
   * library: If we use let%map instead of normal "let", and we
   * have a declaration of the form:
   *
   * let%map x = e1 in e2
   *
   * even if e1 is of type 'a Term.t, we can use x as having type 'a
   * in the body of e2.
   *)
  let module Let_syntax = struct
    let return = Term.const
    let map ~f a = Term.(return f $ a)
    let both a b = Term.(const Tuple2.create $ a $ b)
  end
  in
  let flag info = Arg.value (Arg.flag info) in
  let opt conv ~default info = Arg.value (Arg.opt conv default info) in
  let%map verbose =
    let doc = "If present, print verbose debug information." in
    flag (Arg.info [ "v"; "verbose" ] ~doc)
  and dump_parsing =
    let doc = "If present, print debug informaton from parsing." in
    flag (Arg.info [ "dump-parsing" ] ~doc)
  and dump_ast =
    let doc = "If present, print the parsed ast." in
    flag (Arg.info [ "dump-ast" ] ~doc)
  and dump_ir =
    let doc = "If present, print the translated ir ast." in
    flag (Arg.info [ "dump-ir" ] ~doc)
  and inline =
    let doc = "If present, do function inlining on ir" in
    flag (Arg.info [ "inline" ] ~doc)
  and dump_assem =
    let doc = "If present, print the final assembly." in
    flag (Arg.info [ "dump-assem" ] ~doc)
  and dump_ssa =
    let doc = "If present, print the program after ssa." in
    flag (Arg.info [ "dump-ssa" ] ~doc)
  and typecheck_only =
    let doc = "If present, exit after typechecking." in
    flag (Arg.info [ "t"; "typecheck-only" ] ~doc)
  and unsafe =
    let doc = "If present, ignores safety in mem access" in
    flag (Arg.info [ "unsafe" ] ~doc)
  and regalloc_only =
    let doc = "Regalloc only for l1 checkpoint" in
    flag (Arg.info [ "r"; "regalloc-only" ] ~doc)
  and emit =
    let doc = "[abs|x86-64] The type of assembly $(docv) to emit." in
    opt
      Emit.conv
      ~default:Emit.Abstract_assem
      (Arg.info [ "e"; "emit" ] ~doc ~docv:"TARGET")
  and opt_level =
    let doc = "[0|1|2] The optimization level $(docv)." in
    opt
      Opt_level.conv
      ~default:Opt_level.Opt_none
      (Arg.info [ "O"; "opt" ] ~doc ~docv:"OPT")
  and filename =
    let doc = "The source file $(docv) to compile." in
    Arg.(required (pos 0 (some non_dir_file) None (info [] ~doc ~docv:"FILE")))
  and header_filename =
    let doc = "The library file with option -l" in
    opt
      HeaderFilename.conv
      ~default:"../runtime/15411-l4.h0"
      (Arg.info [ "l" ] ~doc ~docv:"FILE")
  in
  { verbose
  ; dump_parsing
  ; dump_ast
  ; dump_ir
  ; inline
  ; dump_assem
  ; dump_ssa
  ; typecheck_only
  ; regalloc_only
  ; emit
  ; opt_level
  ; filename
  ; unsafe
  ; header_filename
  }
;;

let say_if (v : bool) (f : unit -> string) = if v then prerr_endline (f ())

let elaboration_step (ast, ast_h) cmd =
  say_if cmd.verbose (fun () -> "doing elaborating...");
  let elab_h : Aste.program = Elaborater.elaborate ast_h in
  let _elab_raw : Aste.program = Elaborater.elaborate ast in
  let elab_raw : Aste.program = Elaborater.add_main _elab_raw in
  say_if cmd.dump_ast (fun () -> Aste.Print.print_all elab_h);
  say_if cmd.dump_ast (fun () -> Aste.Print.print_all elab_raw);
  elab_h, elab_raw
;;

(* The main driver for the compiler: runs each phase. *)
let compile (cmd : cmd_line_args) : unit =
  (* ***********************************************************)
  (* ssa + global copy-const *)
  let ssa_off = true in
  (* register coalescing *)
  Coalesce.set_coalesce_off true;
  (* COMMON GROUP *)
  let common_off = true in
  (* peephole *)
  Codegen_l4.set_lea_off common_off;
  Translate.set_strength_off common_off;
  let strength_assem_off = common_off in
  (* const fold *)
  let const_fold_off = common_off in
  (* inline *)
  let inline_off = common_off in
  (* basic tail call *)
  Translate.set_tail_off common_off;
  (* block align *)
  Translate.set_block_algn_off common_off;
  (* ***********************************************************)
  let aSSEM_MAGIC = 1000 in
  if cmd.dump_parsing then ignore (Parsing.set_trace true : bool);
  (* Parse *)
  say_if cmd.verbose (fun () -> "Parsing... " ^ cmd.filename);
  let ast_h = Parse.parse cmd.header_filename in
  let ast = Parse.parse cmd.filename in
  say_if cmd.dump_ast (fun () -> Ast.Print.pp_program ast_h);
  say_if cmd.dump_ast (fun () -> Ast.Print.pp_program ast);
  (* Elaborate *)
  let elab_h, elab_raw = elaboration_step (ast, ast_h) cmd in
  say_if cmd.dump_ast (fun () -> Aste.Print.print_all elab_h);
  say_if cmd.dump_ast (fun () -> Aste.Print.print_all elab_raw);
  say_if cmd.verbose (fun () -> "doing type checking...");
  let asts_raw = Statsem.static_semantic ~hdr:elab_h ~src:elab_raw in
  let (() : unit) = Return.ret_checker elab_raw in
  say_if cmd.verbose (fun () -> "renaming what is necc...");
  let elab = Preprocess.rename elab_h asts_raw in
  say_if cmd.dump_ast (fun () -> Asts.Print.print_all elab);
  (* Typecheck *)
  if cmd.typecheck_only then exit 0;
  (* Translate *)
  say_if cmd.verbose (fun () -> "Translating...");
  let raw_ir = TranslationM.translate elab in
  (* say_if cmd.dump_ir (fun () -> TreeM.Print.pp_program raw_ir); *)
  let ir = if const_fold_off then raw_ir else Strength.strength_reduction raw_ir in
  say_if cmd.dump_ir (fun () -> TreeM.Print.pp_program ir);
  (*_ opt: function inline *)
  (* say_if cmd.verbose (fun () -> "Doing function inline...");*)
  let ir = if inline_off then ir else Inline.inline ir in
  (*say_if cmd.inline (fun () -> TreeM.Print.pp_program ir); *)
  (* Codegen *)
  say_if cmd.verbose (fun () -> "Codegen...");
  let mfail = Label.create () in
  let assem' = Codegen_l4.codegen ~mfl:mfail ~unsafe:cmd.unsafe ir in
  let ssa_off = ssa_off || List.length assem' > aSSEM_MAGIC in
  say_if cmd.dump_assem (fun () -> AssemM.format_program assem');
  let assem =
    if ssa_off
    then assem'
    else (
      say_if cmd.verbose (fun () -> "Starting ssa...");
      let assem_ssa' = Ssa.ssa assem' in
      (* print_endline "after all ssa"; *)
      (* print_endline (Ssa.pp_program assem_ssa'); *)
      say_if cmd.verbose (fun () -> "Starting propogation ...");
      let assem_ssa_prop = Propagation.propagate assem_ssa' in
      (* print_endline (Ssa.pp_program assem_ssa_prop); *)
      say_if cmd.verbose (fun () -> "Doing phi_opt");
      let assem_ssa_phi_opt = Propagation.phiopt assem_ssa_prop in
      (* print_endline "assem_ssa_phi_opt"; *)
      (* print_endline (Ssa.pp_program assem_ssa_phi_opt); *)
      say_if cmd.verbose (fun () -> "Starting de-ssa ...");
      let assem = Ssa.de_ssa assem_ssa_phi_opt in
      (* print_endline (AssemM.format_program assem); *)
      say_if cmd.dump_ssa (fun () -> "Dumping ssa...");
      (* let () = if cmd.dump_ssa then (fun () -> Propagation.debug assem) () else () in *)
      assem)
  in
  say_if cmd.dump_assem (fun () -> "SSAAAAAAAAAAAAAAA");
  say_if cmd.dump_assem (fun () -> AssemM.format_program assem);
  let assem = if strength_assem_off then assem else Assem_strength.strength assem in
  (* print_endline "after all de-ssa"; *)
  say_if cmd.verbose (fun () -> "Emitting...");
  match cmd.emit with
  (* Output: abstract 3-address assem *)
  | Abstract_assem ->
    let file = cmd.filename ^ ".abs" in
    say_if cmd.verbose (fun () -> sprintf "Writing abstract assem to %s..." file);
    Out_channel.with_file file ~f:(fun out ->
        Out_channel.fprintf out "%s" (AssemM.format_program assem))
  | X86_64 ->
    let file = cmd.filename ^ ".s" in
    say_if cmd.verbose (fun () -> sprintf "Writing x86 assem to %s..." file);
    Out_channel.with_file file ~f:(fun out ->
        let output_x86_instr instr = Out_channel.fprintf out "%s\n" (X86.format instr) in
        let translated = Translate.translate assem ~mfail ~unsafe:cmd.unsafe in
        say_if cmd.verbose (fun () -> "Doing speed up");
        let union = Translate.get_string_list translated in
        output_x86_instr (X86.Directive (".file\t\"" ^ cmd.filename ^ "\""));
        output_x86_instr (X86.Directive ".text");
        List.iter ~f:output_x86_instr union)
;;

let run (cmd : cmd_line_args) : unit =
  try if cmd.regalloc_only then compile cmd else compile cmd with
  (*_ regalloc cmd*)
  | Error_msg.Error ->
    prerr_endline "Compilation failed.";
    exit 1
;;

(* Compiler entry point
 * Use the cmd_line_term to parse the command line arguments, and pass the
 * parsed args to the run function.
 *)
let main () =
  let open Cmdliner in
  let cmd_line_info = Cmd.info "c0c" ~doc:"Compile a c0c source file." in
  let cli_parse_result : cmd_line_args Cmd.t = Cmd.v cmd_line_info cmd_line_term in
  match Cmd.eval_value cli_parse_result with
  | Ok (`Ok cmd_line) -> run cmd_line
  | Ok `Version -> Stdlib.exit Cmd.Exit.ok
  | Ok `Help -> Stdlib.exit Cmd.Exit.ok
  | Error _ -> Stdlib.exit Cmd.Exit.cli_error
;;
