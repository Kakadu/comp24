open AstLib.Ast
open IR

let bin_op_to_str bo =
  let map = function
    | Add -> "+"
    | Sub -> "_"
    | Mul -> "*"
    | Div -> "/"
    | Leq -> "<="
    | Less -> "<"
    | Geq -> ">="
    | Gre -> ">"
    | Eq -> "="
    | Neq -> "!="
    | And -> "&&"
    | Or -> "||"
  in
  let res = [ "( "; map bo; " )" ] in
  String.concat "" res
;;

let rec transform_back = function
  | NoPMEConst c -> EConst c
  | NoPMEId id -> EId id
  | NoPMEBinOp (bin_op, e1, e2) ->
    EApp
      ( EApp (EId (IdentOfDefinable (IdentOp (bin_op_to_str bin_op))), transform_back e1)
      , transform_back e2 )
  | NoPMEApp (e1, e2) -> EApp (transform_back e1, transform_back e2)
  | NoPMEFun (pat, expr) -> EFun (pat, transform_back expr)
  | NoPMEIf (i, t, e) -> EIf (transform_back i, transform_back t, transform_back e)
  | NoPMEClsr (decl, expr2) -> EClsr (transform_back_decl decl, transform_back expr2)
  | NoPMEConstraint (e, typ) -> EConstraint (transform_back e, typ)
  | NoPMEList (e1, e2) -> EList (transform_back e1, transform_back e2)
  | NoPMETuple (e1, e2, els) ->
    ETuple (transform_back e1, transform_back e2, List.map transform_back els)

and transform_back_decl = function
  | NoPMDLet (rec_flag, (decl, expr)) ->
    let expr = transform_back expr in
    DLet (rec_flag, (decl, expr))
  | NoPMDLetMut (rec_flag, (pat, expr), (pat1, expr1), tl) ->
    let applied = tl in
    let res = List.map (fun (pat, e) -> pat, transform_back e) applied in
    DLetMut (rec_flag, (pat, transform_back expr), (pat1, transform_back expr1), res)
;;

let transform_expr_in_decl f =
  let helper = function
    | NoPMDLet (rec_flag, (pat, expr)) -> NoPMDLet (rec_flag, (pat, f expr))
    | NoPMDLetMut (rec_flag, (pat, expr), (pat1, expr1), tl) ->
      let res = List.map (fun (pat, e) -> pat, f e) tl in
      NoPMDLetMut (rec_flag, (pat, f expr), (pat1, f expr1), res)
  in
  helper
;;

let rec optimize e =
  let rec helper = function
    | NoPMEBinOp (And, NoPMEConst (CBool true), e)
    | NoPMEBinOp (And, e, NoPMEConst (CBool true))
    | NoPMEIf (NoPMEConst (CBool true), e, _) -> helper e
    | NoPMEApp (e1, e2) -> eapp (helper e1) (helper e2)
    | NoPMEClsr (decl, expr) -> eclsr (transform_expr_in_decl helper decl) (helper expr)
    | (NoPMEConst _ | NoPMEId _) as e -> e
    | NoPMEConstraint (e, t) -> e_typed ~typ:(Some t) (helper e)
    | NoPMEFun (pat, e) -> efun pat (helper e)
    | NoPMEIf (i, t, e) -> eif (helper i) (helper t) (helper e)
    | NoPMEList (e1, e2) -> elist (helper e1) (helper e2)
    | NoPMETuple (e1, e2, es) -> etuple (helper e1) (helper e2) (List.map helper es)
    | NoPMEBinOp (bop, e1, e2) -> ebin_op bop (helper e1) (helper e2)
  in
  let optimized = helper e in
  if optimized = e then e else optimize optimized
;;
