open Base
open Anf_ast
open Cf_ast
open State.IntStateM
open State.IntStateM.Syntax
open Utils
open Std

let new_var () =
  let* fresh = fresh in
  return ("a" ^ string_of_int fresh)
;;

let rec anf_expr env (expr : cf_expr) (cont : imm_expr -> aexpr State.IntStateM.t)
  : aexpr State.IntStateM.t
  =
  match expr with
  | CFConst c ->
    (match c with
     | Ast.CInt n -> cont (ImmInt n)
     | Ast.CBool b -> cont (ImmBool b)
     | Ast.CUnit -> cont ImmUnit
     | Ast.CNil -> cont ImmNil)
  | CFVar x ->
    cont
      (ImmId
         (match Map.find env x with
          | Some v -> v
          | None -> Format.sprintf "(Variable %s not found)" x))
    (* TODO ^^^ *)
  | CFApp (func, arg) ->
    let* name = new_var () in
    let* body = cont (ImmId name) in
    anf_expr env func (fun imm_func ->
      anf_expr env arg (fun imm_arg ->
        return (ALet (name, CApp (imm_func, imm_arg), body))))
  | CFIfElse (i, t, e) ->
    let* name = new_var () in
    let* body = cont (ImmId name) in
    anf_expr env i (fun i ->
      let* t = anf_expr env t (fun x -> return @@ ACExpr (CImmExpr x)) in
      let* e = anf_expr env e (fun x -> return @@ ACExpr (CImmExpr x)) in
      return (ALet (name, CIfElse (i, t, e), body)))
  | CFLetIn (id, expr, body) ->
    let* name = new_var () in
    let new_env = Map.set env ~key:id ~data:name in
    anf_expr env expr (fun imm_expr ->
      let* body = anf_expr new_env body cont in
      return (ALet (name, CImmExpr imm_expr, body)))
  | CFTuple _ | CFList _ -> failwith "Not implemented"
;;

let anf_def env (def : cf_definition) =
  match def with
  | CFLet (id, args, expr) ->
    let env = List.fold args ~init:env ~f:(fun acc x -> Map.set acc ~key:x ~data:x) in
    let env = Map.set env ~key:id ~data:id in
    let* aexpr = anf_expr env expr (fun x -> return @@ ACExpr (CImmExpr x)) in
    return (Fn (id, args, aexpr), env)
;;

let default_env =
  let env =
    stdlib @ runtime
    |> List.fold
         ~init:(Map.empty (module String))
         ~f:(fun acc x -> Map.set acc ~key:x.name ~data:x.name)
  in
  env
;;

(** Removes `let ax = ... in ax` and `let ax = ay in ...` *)
let remove_useless_bindings =
  let remaps = Hashtbl.create (module String) in
  let useless =
    let rec useless_c = function
      | CIfElse (c, a1, a2) -> CIfElse (c, useless_a a1, useless_a a2)
      | x -> x
    and useless_a = function
      | ALet (id1, CImmExpr (ImmId id2), a)
        when String.equal (String.common_prefix2 id1 id2) "a" ->
        Hashtbl.set remaps ~key:id1 ~data:id2;
        useless_a a
      | ALet (id1, c, ACExpr (CImmExpr (ImmId id2))) when String.equal id1 id2 ->
        ACExpr (useless_c c) |> useless_a
      | ALet (id, c, a) -> ALet (id, useless_c c, useless_a a)
      | ACExpr c -> ACExpr (useless_c c)
    in
    function
    | Fn (id, args, a) -> Fn (id, args, useless_a a)
  in
  let remap =
    let rec remap_i = function
      | ImmId id -> ImmId (Hashtbl.find remaps id |> Option.value ~default:id)
      | ImmTuple xs -> ImmTuple (List.map ~f:remap_i xs)
      | ImmList xs -> ImmList (List.map ~f:remap_i xs)
      | x -> x
    and remap_c = function
      | CIfElse (c, a1, a2) -> CIfElse (remap_i c, remap_a a1, remap_a a2)
      | CApp (i1, i2) -> CApp (remap_i i1, remap_i i2)
      | CImmExpr i -> CImmExpr (remap_i i)
    and remap_a = function
      | ALet (id, c, a) -> ALet (id, remap_c c, remap_a a)
      | ACExpr c -> ACExpr (remap_c c)
    in
    function
    | Fn (id, args, a) -> Fn (id, args, remap_a a)
  in
  Fn.compose remap useless
;;

let anf (ast : Cf_ast.program) : Anf_ast.program =
  let helper ast =
    List.fold
      ast
      ~init:(return (([] : fn list), default_env))
      ~f:(fun acc x ->
        let* fns, env = acc in
        let* x, env = anf_def env x in
        return (x :: fns, env))
    >>| fun (defs, _) -> List.rev defs
  in
  run (helper ast) |> List.map ~f:remove_useless_bindings
;;
