%{
(* L1 Compiler
 * L1 grammar
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Now conforms to the L1 fragment of C0
 *
 * Modified: Maxime Serrano <mserrano@andrew.cmu.edu> Fall 2014
 * Should be more up-to-date with 2014 spec
 *
 * Modified: Alice Rao <alrao@andrew.cmu.edu> Fall 2017
 *   - Update to use Core instead of Core.Std and ppx
 *
 * Modified: Nick Roberts <nroberts@alumni.cmu.edu>
 *   - Update to use menhir instead of ocamlyacc.
 *   - Improve presentation of marked asts.
 *
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

let mark
  (data : 'a)
  (start_pos : Lexing.position)
  (end_pos : Lexing.position) : 'a Mark.t =
  let src_span = Mark.of_positions start_pos end_pos in
  Mark.mark data src_span
%}

%token Eof
%token Semicolon
%token <Int32.t> Dec_const
%token <Int32.t> Hex_const
%token <Symbol.t> Ident
%token <Symbol.t> TypeIdent
%token Return
%token Int Bool
%token True False
%token Struct
%token NULL Alloc Alloc_array
%token L_square R_square Dot Arrow
%token If Else While For
%token Plus Minus Star Slash Percent
%token Assign Plus_eq Minus_eq Star_eq Slash_eq Percent_eq 
%token L_brace R_brace
%token L_paren R_paren
%token ShiftL ShiftR ShiftL_eq ShiftR_eq
%token Less Less_eq Greater Greater_eq
%token Eq_eq Neq
%token Unary
%token LOr LAnd BOr BXor BAnd BNot LNot 
%token BAnd_eq Bor_eq BXor_eq
%token Minus_minus Plus_plus
%token QuestionMark Colon
%token Void Comma Assert 
%token <Symbol.t * Symbol.t> Typedef

(* Unary is a dummy terminal.
 * We need dummy terminals if we wish to assign a precedence
 * to a production that does not correspond to the precedence of
 * the rightmost terminal in that production.
 * Implicit in this is that precedence can only be inferred for
 * terminals. Therefore, don't try to assign precedence to "rules"
 * or "productions".
 *
 * Minus_minus is a dummy terminal to parse-fail on.
 *)
%right QuestionMark Colon
%left LOr
%left LAnd
%left BOr
%left BXor
%left BAnd
%left Eq_eq Neq
%left Less Less_eq Greater Greater_eq
%left ShiftL ShiftR
%left Plus Minus
%left Star Slash Percent
%right Unary

%nonassoc else_hack_1
%nonassoc Else

%start program

(* It's only necessary to provide the type of the start rule,
 * but it can improve the quality of parser type errors to annotate
 * the types of other rules.
 *)
%type <Ast.mgdecl list> program
%type <Ast.mstm list> stms
%type <Ast.stm> stm
%type <Ast.mstm> m(stm)
%type <Ast.decl> decl
%type <Ast.stm> simp
%type <Ast.exp> exp
%type <Ast.mexp> m(exp)
%type <Core.Int32.t> int_const
%type <Ast.binop> binop
%type <Ast.binop option> asnop
%type <Ast.param list> param_list
%type <Ast.param list> param_follow
%type <Ast.param> param
%type <Ast.gdecl> typedef
%type <Ast.gdecl> fdefn
%type <Ast.gdecl> fdecl

%%


program :
  | Eof
    { [] }
  | g = gdecl; p = program; Eof
    {g :: p}
  ;

(* This higher-order rule produces a marked result of whatever the
 * rule passed as argument will produce.
 *)
m(x) :
  | x = x;
      (* $startpos(s) and $endpos(s) are menhir's replacements for
       * Parsing.symbol_start_pos and Parsing.symbol_end_pos, but,
       * unfortunately, they can only be called from productions. *)
      { mark x $startpos(x) $endpos(x) }
  ;

type_ :
  | Int { Ast.T.RealTyp Int }
  | Bool { Ast.T.RealTyp Bool}
  | ident = Ident;
    { Ast.T.FakeTyp (ident) }
  ;

ret_type : 
    | t = type_ { Some(t) }
    | Void { None }

stms :
  | (* empty *)
      { [] }
  | hd = m(stm); tl = stms;
      { hd :: tl }
  ;

stm :
  | s = simp; Semicolon;
      { s }
  | c = control;
      { c }
  | b = block;
      { b }

block : 
  | L_brace; body = stms; R_brace;
  {Ast.Block body }

decl :
  | tp = type_; ident = Ident;
      { Ast.New_var (ident, tp) }
  | tp = type_; ident = Ident; Assign; e = m(exp);
      { Ast.Init (ident, tp, e) }
  ;

simp :
  | lhs = m(exp);
    op = asnop;
    rhs = m(exp);
      { Ast.Assign {left=lhs; right=rhs; asgnop=op} }
  | lhs = m(exp);
    op = postop;
    { Ast.PostOp {left=lhs; op=op}  }
  | d = decl;
      { Ast.Declare d }
  | e = m(exp);
        { Ast.Exp e }
  ;

simpopt :
  | (* empty *)
    { None }
  | s = m(simp)
    { Some s }

exp :
  | L_paren; e = exp; R_paren;
      { e }
  | c = int_const;
      { Ast.Const c }
  | True; { Ast.True }
  | False; { Ast.False }
  | NULL; { Ast.NULL }
  | ident = Ident;
      { Ast.Var ident }
  | ident = Ident; args = arg_list;
      { Ast.Call {name = ident; args = args} }
  | u = unop; { u }
  | e = m(exp);
    Dot;
    ident = Ident;
    { Ast.Dot {field = ident; lhs = e} }
  | e = m(exp);
    Arrow;
    ident = Ident;
    { Ast.Arrow {field = ident; lhs = e} }
  | lhs = m(exp);
    op = binop;
    rhs = m(exp);
      { Ast.Binop { op; lhs; rhs; } }
  | Alloc;
    L_paren;
    t = type_;
    R_paren;
    { Ast.Alloc {t} }
  | Alloc_array;
    L_paren;
    t = type_;
    Comma;
    e = m(exp);
    R_paren;
    { Ast.AllocArray {t; e} }
  | lhs = m(exp);
    L_square;
    rhs = m(exp);
    R_square;
    { Ast.AccessArray {lhs; rhs} }
  | Star;
    e = m(exp);
    { Ast.Deref e }
  | cond = m(exp); 
    QuestionMark; 
    f = m(exp);
    Colon; 
    s = m(exp); 
      { Ast.Ternary {cond = cond; first=f; second = s} }
  ;

arg_follow :
  | (* empty *)
      { [] }
  | Comma; e = m(exp); args = arg_follow; {
    e :: args
  }

arg_list: 
    | L_paren; R_paren; {[]}
    | L_paren; e = m(exp); args = arg_follow  ; R_paren; {
        e :: args
    }

unop : 
  | Minus; e = m(exp); %prec Unary
      { Ast.Unop { op = Ast.Negative; operand = e; } }
  | LNot; e = m(exp); %prec Unary
      { Ast.Unop { op = Ast.L_not ; operand = e; } }
  | BNot; e = m(exp); %prec Unary
      { Ast.Unop { op = Ast.B_not; operand = e; } }

int_const :
  | c = Dec_const;
      { c }
  | c = Hex_const;
      { c }
  ;


ifstm : 
    | If; 
    L_paren; 
    e = m(exp);
    R_paren;
    t = m(stm);
    Else; 
    f = m(stm);
      { Ast.If {cond = e; thenstm = t; elsestm = Some f } }
  | If; 
    L_paren; 
    e = m(exp);
    R_paren;
    t = m(stm);
    %prec else_hack_1
      { Ast.If {cond = e; thenstm = t; elsestm = None } }

control : 
    | i = ifstm;
      { i }
    | While;
      L_paren; 
      e = m(exp);
      R_paren;
      body = m(stm);
        {Ast.While {cond = e; body = body}}
    | For ;
      L_paren;
      init = simpopt;
      Semicolon;
      cond = m(exp);
      Semicolon;
      post = simpopt;
      R_paren;
      body = m(stm);
        { Ast.For { init = init; cond = cond; post = post; body = body}  }
    | Return; Semicolon; 
        {Ast.Return None}
    | Return; 
      e = m(exp);
      Semicolon;
        {Ast.Return (Some(e))}
    | Assert; L_paren; e = m(exp); R_paren; Semicolon
        {Ast.Assert e}

typedef : 
    | typedef_object = Typedef; Semicolon; {
        Ast.TypedefTest typedef_object
    } 

param : 
    | t = type_ ; ident = Ident; {
        Ast.Param {t = t; name = ident}
    }

param_follow :
  | (* empty *)
      { [] }
  | Comma; p = param; ps = param_follow; {
    p :: ps
  }

param_list: 
    | L_paren; R_paren; {[]}
    | L_paren; p = param; ps = param_follow  ; R_paren; {
        p :: ps
    }   

fdecl: 
    | r_opt = ret_type; ident = Ident; params = param_list; Semicolon
    { Ast.FunDec {name = ident; ret_type = r_opt; params = params} } 

fdefn: 
    | r_opt= ret_type; ident = Ident; params = param_list; body = m(block)
    { Ast.FunDef {name = ident; ret_type = r_opt; params = params; body = body} } 

gdecl: 
    | fundec = m(fdecl)  {fundec}
    | fundef = m(fdefn)  {fundef}
    | tdef = m(typedef)  {tdef}

(* See the menhir documentation for %inline.
 * This allows us to factor out binary operators while still
 * having the correct precedence for binary operator expressions.
 *)
%inline
binop :
  | Plus;
      { Ast.Plus }
  | Minus;
      { Ast.Minus }
  | Star;
      { Ast.Times }
  | Slash;
      { Ast.Divided_by }
  | Percent;
      { Ast.Modulo }
  | Less;
      { Ast.Less}
  | Less_eq;
      { Ast.Less_eq}
  | Greater;
      { Ast.Greater}
  | Greater_eq;
      { Ast.Greater_eq}
  | Eq_eq;
      { Ast.Equals}
  | Neq;
      { Ast.Not_equals}
  | LAnd;
      { Ast.L_and}
  | LOr;
      { Ast.L_or}
  | BAnd;
      { Ast.B_and}
  | BOr;
      { Ast.B_or}
  | BXor;
      { Ast.B_xor}
  | ShiftL;
      { Ast.ShiftL}
  | ShiftR;
      { Ast.ShiftR}
  ;

asnop :
  | Assign
      { None }
  | Plus_eq
      { Some Ast.Plus }
  | Minus_eq
      { Some Ast.Minus }
  | Star_eq
      { Some Ast.Times }
  | Slash_eq
      { Some Ast.Divided_by }
  | Percent_eq
      { Some Ast.Modulo }
  | ShiftL_eq
      { Some Ast.ShiftL }
  | ShiftR_eq
      { Some Ast.ShiftR }
  | BAnd_eq
      { Some Ast.B_and }
  | Bor_eq
      { Some Ast.B_or }
  | BXor_eq
      { Some Ast.B_xor }
  ;


postop : 
  | Plus_plus 
      { Ast.Plus }
  | Minus_minus 
      { Ast.Minus }
  ;
%%
