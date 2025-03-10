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
  | CApplication of cexpr * cexpr
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

  let rec generate_name (env : t) (name : id) =
    let* fresh_num = fresh in
    let new_name = "anf_var_" ^ Int.to_string fresh_num in
    match find new_name env with
    | None -> return (extend (name, new_name) env, new_name)
    | Some _ -> generate_name env name
  ;;

  let default =
    List.fold
      [ "list_get"
      ; "list_length"
      ; "list_head"
      ; "list_tail"
      ; "tuple_get"
      ; "print_int"
      ; "print_string"
      ]
      ~init:empty
      ~f:(fun env name -> extend (name, name) env)
  ;;
end

let _application l r = CApplication (CImm l, CImm r)
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

let rec convert_expr (env : NameEnv.t) (expr : me_expr) (k : imm_expr -> aexpr t) =
  match expr with
  | MEConst v -> k @@ value_to_anf v
  | MEVar id ->
    (try k @@ ImmId (NameEnv.find_exn id env) with
     | _ -> failwith id)
  | MEOperation op -> k @@ ImmOperation op
  | MEConstraint (expr, _) -> convert_expr env expr k
  | MEApplication (l, r) ->
    let* new_var = get_fresh_name in
    let* scope = k @@ ImmId new_var in
    convert_expr env l (fun imm_l ->
      convert_expr env r (fun imm_r ->
        return @@ _let_in (Var new_var) (_application imm_l imm_r) scope))
  | MEList exprs ->
    let rec helper acc = function
      | hd :: tl -> convert_expr env hd (fun imm_hd -> helper (imm_hd :: acc) tl)
      | [] -> k @@ ImmList (List.rev acc)
    in
    helper [] exprs
  | METuple (expr1, expr2, exprs) ->
    let exprs = expr1 :: expr2 :: exprs in
    let rec helper acc = function
      | hd :: tl -> convert_expr env hd (fun imm_hd -> helper (imm_hd :: acc) tl)
      | [] -> k @@ ImmTuple (List.rev acc)
    in
    helper [] exprs
  | MEListConcat (l, r) ->
    let* new_var = get_fresh_name in
    let* scope = k @@ ImmId new_var in
    convert_expr env l (fun imm_l ->
      convert_expr env r (fun imm_r ->
        return @@ _let_in (Var new_var) (CConstructList (imm_l, imm_r)) scope))
  | MEIf (i, t, Some e) ->
    let* new_var = get_fresh_name in
    let* scope = k @@ ImmId new_var in
    convert_expr env i (fun imm_i ->
      convert_expr env t (fun imm_t ->
        convert_expr env e (fun imm_e ->
          return @@ _let_in (Var new_var) (_if imm_i imm_t (Some imm_e)) scope)))
  | MEIf (i, t, None) ->
    let* new_var = get_fresh_name in
    let* scope = k @@ ImmId new_var in
    convert_expr env i (fun imm_i ->
      convert_expr env t (fun imm_t ->
        return @@ _let_in (Var new_var) (_if imm_i imm_t None) scope))
  | MELet (rec_flag, binds, Some scope) ->
    let (pattern, body), other_binds = extract_binds binds in
    let* new_env, pattern =
      match pattern with
      | Var name ->
        let* new_name = get_fresh_name in
        return (NameEnv.extend (name, new_name) env, Var new_name)
      | _ -> return (env, pattern)
    in
    convert_expr env body (fun imm_first_body ->
      match other_binds with
      | hd :: tl ->
        let new_scope = MELet (rec_flag, hd :: tl, Some scope) in
        convert_expr new_env new_scope (fun imm_scope ->
          return @@ _let_in pattern (CImm imm_first_body) (_ac imm_scope))
      | [] ->
        convert_expr new_env scope (fun imm_scope ->
          return @@ _let_in pattern (CImm imm_first_body) (_ac imm_scope)))
  | MELet (_, _, None) -> failwith "Inner Let can't exist without scope"
;;

let anf_decl (env : NameEnv.t) (decl : me_expr) =
  let collect_args env (args : id list) =
    Base.List.fold_right args ~init:(return env) ~f:(fun id env ->
      let* env = env in
      return @@ NameEnv.extend (id, id) env)
  in
  let collect_function env name args body =
    let* env = collect_args env args in
    let* env, new_name = NameEnv.generate_name env name in
    let* body = convert_expr env body (fun imm -> return @@ _ac imm) in
    return (env, new_name, args, body)
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
  let _single_anf_binding (env : NameEnv.t) (bind : me_bind) =
    let pattern, args, body = bind in
    let* env, pattern, args, body =
      collect_function env (_pattern_to_name pattern) (_args_to_ids args) body
    in
    return (env, ALet (pattern, args, body))
  in
  match decl with
  | MELet (rec_flag, binds, None) ->
    let* env, single_anf_binds =
      fold_list binds ~init:(env, []) ~f:(fun (env, binds) bind ->
        let* env, bind = _single_anf_binding env bind in
        return (env, binds @ [ bind ]))
    in
    if List.length single_anf_binds = 1
    then return @@ (env, ADSingleLet (rec_flag, List.hd_exn single_anf_binds))
    else return @@ (env, ADMutualRecDecl single_anf_binds)
  | _ -> failwith "Incorrect starting point was encountered during ANF"
;;

let anf_prog (prog : me_prog) =
  let* _, res_prog =
    fold_list prog ~init:(NameEnv.default, []) ~f:(fun acc decl ->
      let env, decls = acc in
      let* env, decl = anf_decl env decl in
      return (env, decl :: decls))
  in
  return @@ List.rev res_prog
;;
