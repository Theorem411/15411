module Ast = Ast
module AstElab = Aste_l4
val elaborate: Ast.program -> AstElab.program
val add_main: AstElab.program -> AstElab.program