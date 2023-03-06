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
module TranslationM = Trans_new
module AssemM = Assem_new
module Cogen = Cogen_new
module TreeM = Tree_new
module Statsem = Statsem

(* Command line arguments *)

module Opt_level = struct
  type t = Opt_none

  let show = function
    | Opt_none -> "O0"
  ;;

  let parse = function
    | "0" -> Result.Ok Opt_none
    | "1" -> Result.Error (`Msg "Error: -O1 unimplemented (lab 2)")
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
  ; dump_assem : bool
  ; typecheck_only : bool
  ; regalloc_only : bool
  ; emit : Emit.t
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
  and dump_assem =
    let doc = "If present, print the final assembly." in
    flag (Arg.info [ "dump-assem" ] ~doc)
  and typecheck_only =
    let doc = "If present, exit after typechecking." in
    flag (Arg.info [ "t"; "typecheck-only" ] ~doc)
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
      ~default:"../runtime/15411-l3.h0"
      (Arg.info [ "l" ] ~doc ~docv:"FILE")
  in
  { verbose
  ; dump_parsing
  ; dump_ast
  ; dump_ir
  ; dump_assem
  ; typecheck_only
  ; regalloc_only
  ; emit
  ; opt_level
  ; filename
  ; header_filename
  }
;;

let say_if (v : bool) (f : unit -> string) = if v then prerr_endline (f ())

let regalloc (cmd : cmd_line_args) =
  match String.chop_suffix cmd.filename ~suffix:".in" with
  | None ->
    prerr_endline "Invalid input filename";
    exit 1
  | Some base_filename ->
    let input_json = Yojson.Basic.from_file cmd.filename in
    let input = Lab1_checkpoint.program_of_json input_json in
    let output = Regalloc.regalloc input in
    let filename = base_filename ^ ".out" in
    Out_channel.with_file filename ~f:(fun out ->
        Out_channel.output_string
          out
          (output |> Lab1_checkpoint.json_of_allocations |> Yojson.Basic.to_string))
;;

let elaboration_step (ast, ast_h) cmd =
  say_if cmd.verbose (fun () -> "doing elaborating...");
  let elab_h : Aste.program = Elaborater.elaborate ast_h in
  let _elab_raw : Aste.program = Elaborater.elaborate ast in
  let elab_raw : Aste.program = Elaborater.add_main _elab_raw in
  say_if cmd.dump_ast (fun () -> Aste.Print.print_all elab_h);
  say_if cmd.dump_ast (fun () -> "\n------------------------------------------\n");
  say_if cmd.dump_ast (fun () -> Aste.Print.print_all elab_raw);
  elab_h, elab_raw
;;

(* The main driver for the compiler: runs each phase. *)
let compile (cmd : cmd_line_args) : unit =
  if cmd.dump_parsing then ignore (Parsing.set_trace true : bool);
  (* Parse *)
  say_if cmd.verbose (fun () -> "Parsing... " ^ cmd.filename);
  let ast_h = Parse.parse cmd.header_filename in
  (* TODO check that ast_h has the only fdecl or typedef *)
  (* TODO rename functions in ast_h *)
  let ast = Parse.parse cmd.filename in
  say_if cmd.dump_ast (fun () -> Ast.Print.pp_program ast_h);
  say_if cmd.dump_ast (fun () -> "\n------------------------------------------\n");
  say_if cmd.dump_ast (fun () -> Ast.Print.pp_program ast);
  (* Elaborate *)
  (* let elab_h, elab = elaboration_step (ast, ast_h) cmd in *)
  let elab_h, elab_raw = elaboration_step (ast, ast_h) cmd in
  (*if cmd.dump_ast then ignore ((exit 0): unit); *)
  say_if cmd.verbose (fun () -> "doing type Checking...");
  let (() : unit) = Statsem.static_semantic ~hdr:elab_h ~src:elab_raw in
  let (() : unit) = Return.ret_checker elab_raw in
  say_if cmd.verbose (fun () -> "renaming what is necc...");
  let elab = Preprocess.rename elab_h elab_raw in
  say_if cmd.dump_ast (fun () -> "\n------------------------------------------\n");
  say_if cmd.dump_ast (fun () -> Aste.Print.print_all elab);
  (* Typecheck *)
  (* Typechecker.typecheck ast; *)
  if cmd.typecheck_only then exit 0;
  (* Translate *)
  say_if cmd.verbose (fun () -> "Translating...");
  let ir = TranslationM.translate (elab_h @ elab) in
  say_if cmd.dump_ir (fun () -> TreeM.Print.pp_program ir);
  (* Codegen *)
  say_if cmd.verbose (fun () -> "Codegen...");
  (* let assem_blocks = Cogen.cogen_block ir in
  let assem = Cogen.cogen assem_blocks in *)
  let assem = Cogen.cogen ir in
  say_if cmd.dump_assem (fun () -> AssemM.format_program assem);
  (* Testing blocks *)
  say_if cmd.dump_assem (fun () ->
  Block.pp_all_blocks (Block.blocks_former assem));
  (* Testing blocks *)
  match cmd.emit with
  (* Output: abstract 3-address assem *)
  | Abstract_assem ->
    let file = cmd.filename ^ ".abs" in
    say_if cmd.verbose (fun () -> sprintf "Writing abstract assem to %s..." file);
    Out_channel.with_file file ~f:(fun out ->
        (* let output_instr instr = Out_channel.fprintf out "\t%s\n" (AssemM.format instr) in
        output_instr (AssemM.Directive (".file\t\"" ^ cmd.filename ^ "\""));
        output_instr (AssemM.Directive ".function\tmain()");
        List.iter ~f:output_instr assem;
        output_instr (AssemM.Directive ".ident\t\"15-411 L1 reference compiler\"")) *)
        Out_channel.fprintf out "%s" (AssemM.format_program assem))
  | X86_64 ->
    let file = cmd.filename ^ ".s" in
    say_if cmd.verbose (fun () -> sprintf "Writing x86 assem to %s..." file);
    Out_channel.with_file file ~f:(fun out ->
        let output_x86_instr instr = Out_channel.fprintf out "%s\n" (X86.format instr) in
        let translated = Translate.translate assem in
        let union = Translate.get_string_list translated in
        output_x86_instr (X86.Directive (".file\t\"" ^ cmd.filename ^ "\""));
        output_x86_instr (X86.Directive ".text");
        List.iter ~f:output_x86_instr union)
;;

let run (cmd : cmd_line_args) : unit =
  try if cmd.regalloc_only then regalloc cmd else compile cmd with
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