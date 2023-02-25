open Core
module T = Ctype
module SM = Symbol.Map
module SS = Symbol.Set
module A = Aste
module TS = Comparable.Make(struct 
  type t = T.tau 
  [@@deriving compare, sexp]
end)
(*_ exception and error printing *)
let global_err = Error_msg.create ()

exception TypeError

let error ~msg ~ast =
  Error_msg.error global_err (Mark.src_span ast) ~msg;
  raise TypeError
;;

module Delta = struct
  type t = T.fsig SM.t * SS.t (*_ f is declared with sig and f is defined *)
  let is_declared ((decl, _) : t) (f : Symbol.t) = SM.mem decl f
  let is_defined ((_, def) : t) (f : Symbol.t) = SS.mem def f
end
module Omega = struct 
  type t = T.t TS.Map.t
  let type_symbols (omega : t) = 
    let taus = TS.Map.key_set omega in
    let taus' = TS.Set.filter_map taus ~f:(fun tau -> match tau with T.FakeTyp _ -> true | _ -> false) in
  let is_type (omega : t) (s : Symbol.t) = TS.Map.mem omega s
end

let ck_glob (glob : A.mglob) = ()
;;
let static_semantic (prog : A.program) = ()
;;