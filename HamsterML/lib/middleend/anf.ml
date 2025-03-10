open Ast
open ME
open Utils.R
open Base
module Format = Stdlib.Format

type imm_expr =
  | ImmInt of int
  | ImmString of string
  | ImmBool of bool
  | ImmId of id
  | ImmList of imm_expr list
  | ImmTuple of imm_expr list
  | ImmOperation of op
  | ImmUnit

type cexpr =
  | CApplication of cexpr * cexpr * cexpr list
  | CIf of imm_expr * aexpr * aexpr option
  | CConstructList of imm_expr * imm_expr
  | CImm of imm_expr

and aexpr =
  | ALetIn of pattern * cexpr * aexpr
  | ACExpr of cexpr

type single_anf_binding = ALet of id * id list * aexpr

type anf_decl =
  | ADSingleLet of funType * single_anf_binding
  | ADMutualRecDecl of single_anf_binding list

type anf_prog = anf_decl list

module NameEnv = struct
  include Utils.NameEnv
end

let _let_in pattern body scope = ALetIn (pattern, body, scope)
let _ac x = ACExpr (CImm x)

let _if i t e =
  match e with
  | None -> CIf (i, _ac t, None)
  | Some e -> CIf (i, _ac t, Some (_ac e))
;;

let get_fresh_name =
  let* fresh_num = fresh in
  let fresh_name = "anf_" ^ Int.to_string fresh_num in
  return @@ fresh_name
;;

let value_to_anf = function
  | Int i -> ImmInt i
  | Bool b -> ImmBool b
  | String s -> ImmString s
  | Unit -> ImmUnit
;;

(* bind -> name, expr *)
let extract_inner_bind (bind : me_bind) =
  let pattern, args, body = bind in
  if List.length args = 0
  then pattern, body
  else failwith "Inner bind can't contain arguments"
;;

let extract_binds (binds : me_bind list) =
  match binds with
  | [] -> failwith "Inner let can't contains zero bindings"
  | first_bind :: other_binds ->
    let first_bind = extract_inner_bind first_bind in
    first_bind, other_binds
;;

let _application f args =
  let f = CImm f in
  let args = List.map args ~f:(fun x -> CImm x) in
  match args with
  | [ arg ] -> return @@ CApplication (f, arg, [])
  | arg :: args_tl -> return @@ CApplication (f, arg, args_tl)
  | [] -> failwith "Empty application O_O"
;;

let rec flatten_application expr =
  match expr with
  | MEApplication (l, r) ->
    let f, args = flatten_application l in
    f, args @ [ r ]
  | _ -> expr, []
;;

let rec convert_exprs exprs k =
  let rec helper acc = function
    | [] -> k (List.rev acc)
    | h :: tl -> convert_expr h (fun imm -> helper (imm :: acc) tl)
  in
  helper [] exprs

and convert_expr (expr : me_expr) (k : imm_expr -> aexpr t) =
  match expr with
  | MEConst v -> k @@ value_to_anf v
  | MEVar id -> k @@ ImmId id
  | MEOperation op -> k @@ ImmOperation op
  | MEConstraint (expr, _) -> convert_expr expr k
  | MEApplication (_, _) as app ->
    let f, args = flatten_application app in
    convert_expr f (fun imm_f ->
      convert_exprs args (fun imm_args ->
        let* name = get_fresh_name in
        let* scope = k @@ ImmId name in
        let* application = _application imm_f (List.rev imm_args) in
        return @@ _let_in (Var name) application scope))
  | MEList exprs -> convert_exprs exprs (fun imm_exprs -> k @@ ImmList imm_exprs)
  | METuple (expr1, expr2, exprs) ->
    let exprs = expr1 :: expr2 :: exprs in
    convert_exprs exprs (fun imm_exprs -> k @@ ImmTuple imm_exprs)
  | MEListConcat (l, r) ->
    let* new_var = get_fresh_name in
    let* scope = k @@ ImmId new_var in
    convert_expr l (fun imm_l ->
      convert_expr r (fun imm_r ->
        return @@ _let_in (Var new_var) (CConstructList (imm_l, imm_r)) scope))
  | MEIf (i, t, Some e) ->
    let* new_var = get_fresh_name in
    let* scope = k @@ ImmId new_var in
    convert_expr i (fun imm_i ->
      convert_expr t (fun imm_t ->
        convert_expr e (fun imm_e ->
          return @@ _let_in (Var new_var) (_if imm_i imm_t (Some imm_e)) scope)))
  | MEIf (i, t, None) ->
    let* new_var = get_fresh_name in
    let* scope = k @@ ImmId new_var in
    convert_expr i (fun imm_i ->
      convert_expr t (fun imm_t ->
        return @@ _let_in (Var new_var) (_if imm_i imm_t None) scope))
  | MELet (rec_flag, binds, Some scope) ->
    let (pattern, body), other_binds = extract_binds binds in
    convert_expr body (fun imm_first_body ->
      match other_binds with
      | hd :: tl ->
        let new_scope = MELet (rec_flag, hd :: tl, Some scope) in
        convert_expr new_scope (fun imm_scope ->
          return @@ _let_in pattern (CImm imm_first_body) (_ac imm_scope))
      | [] ->
        convert_expr scope (fun imm_scope ->
          return @@ _let_in pattern (CImm imm_first_body) (_ac imm_scope)))
  | MELet (_, _, None) -> failwith "Inner Let can't exist without scope"
;;

let anf_decl (decl : me_expr) =
  let collect_function name args body =
    let* body = convert_expr body (fun imm -> return @@ _ac imm) in
    return (name, args, body)
  in
  let _pattern_to_name pattern =
    match pattern with
    | Var name -> name
    | _ -> failwith "Global function names can't contain patterns after LL"
  in
  let _args_to_ids (args : args) =
    List.map args ~f:(fun arg ->
      match arg with
      | Var id -> id
      | _ -> failwith "Arguments can't be pattern after LL")
  in
  let _single_anf_binding (bind : me_bind) =
    let pattern, args, body = bind in
    let* pattern, args, body =
      collect_function (_pattern_to_name pattern) (_args_to_ids args) body
    in
    return (ALet (pattern, args, body))
  in
  match decl with
  | MELet (rec_flag, binds, None) ->
    let* single_anf_binds = map_list binds ~f:_single_anf_binding in
    if List.length single_anf_binds = 1
    then return @@ ADSingleLet (rec_flag, List.hd_exn single_anf_binds)
    else return @@ ADMutualRecDecl single_anf_binds
  | _ -> failwith "Incorrect starting point was encountered during ANF"
;;

let anf_prog (prog : me_prog) = map_list prog ~f:anf_decl
