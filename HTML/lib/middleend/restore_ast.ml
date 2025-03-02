open AstLib.Ast
open Anf_ast

let rec convert_immexpr = function
  | ImmConst c -> EConst c
  | ImmIdentifier ident -> EId ident
  | ImmConstraint (e, ty) -> EConstraint (convert_immexpr e, ty)

let convert_id_to_pat = function
  | Id s -> PId s
  | IdConstraint (s, ty) -> PConstraint (PId s, ty)

let rec convert_id_to_pat_or_op =
  let open Common.Ops in
   function
  | Id s when List.mem s (base_unops @ base_binops) -> POpOp s
  | Id s -> (POpPat (PId s))
  | IdConstraint (s, ty) -> POrOpConstraint ((convert_id_to_pat_or_op (Id s)), ty)

let rec convert_cexpr = function
  | CApp (ce1, ce2) -> EApp (convert_cexpr ce1, convert_cexpr ce2)
  | CIf (ie, ae1, ae2)-> EIf (convert_immexpr ie, convert_aexpr ae1, convert_aexpr ae2)
  | CImmExpr ie -> convert_immexpr ie

and convert_aexpr = function
  | ALetIn (id, ce, ae) -> 
      let pat = convert_id_to_pat id in
      let expr = convert_cexpr ce in
      let body = convert_aexpr ae in
      EClsr (DLet (Not_recursive, (POpPat pat, expr)), body)
  | ACExpr ce -> convert_cexpr ce

let convert_let_body (id, args, ae) =
  (* guaranteed no tuples lists etc.*)
  let rec roll_efun = function
    | [] -> convert_aexpr ae
    | x :: tl  -> EFun (convert_id_to_pat x, roll_efun tl)
  in
  let body = roll_efun args in
  (convert_id_to_pat_or_op id, body)

let convert_anf_decl = function
    | ADSingleLet (rf, let_body )-> 
      let lb = convert_let_body let_body in
      DLet (rf ,lb)
    | ADMutualRecDecl (rf, lb1, lb2, lbs) -> 
      let lb1 = convert_let_body lb1 in
      let lb2 = convert_let_body lb2 in
      let lbs = List.map convert_let_body lbs in
      DLetMut(rf, lb1, lb2, lbs)

let convert_anf_prog = List.map convert_anf_decl 