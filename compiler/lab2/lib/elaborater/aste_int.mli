module A = Aste

(*_ this function aims to do some further elaborating that aims to 
   - replace all && with & 
   - replace all || with | 
   - replace all True with Const 1
   - replace all False with Const 0
   - replace all ~ exp with exp ^ Const -1
   *)
val booster_elaborate : A.program -> A.program
