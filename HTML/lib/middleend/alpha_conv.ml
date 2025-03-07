open Anf_ast
open AstLib.Ast

let get_base_name name = List.hd (String.split_on_char '.' name)

type ident_typ =
  | TopLevel
  | LetIn

type ident_counters =
  { top_level : int
  ; let_in : int
  }

module R = struct
  module Env = struct
    let empty = Base.Map.empty (module Base.String)
    let update map key value = Base.Map.update map key ~f:(fun _ -> value)
    let lookup_env key map = Base.Map.find map key
  end

  open Base.Result

  type env = (string, ident_counters, Base.String.comparator_witness) Base.Map.t
  type 'a t = env -> env * ('a, string) Result.t

  let fail error env = env, fail error
  let return value env = env, return value

  let ( >>= ) (monad : 'a t) f env =
    let env, result = monad env in
    match result with
    | Error e -> env, Error e
    | Ok value -> f value env
  ;;

  let ( let* ) = ( >>= )
  let ( >>| ) m f = m >>= fun x -> return @@ f x
  let run m = snd @@ m Env.empty

  let map f xs =
    let* res =
      List.fold_left
        (fun acc x ->
          let* acc = acc in
          let* res = f x in
          return (res :: acc))
        (return [])
        xs
    in
    return (List.rev res)
  ;;

  let lookup_env_counters name env =
    let n = Env.lookup_env name env in
    match n with
    | Some n -> env, Ok n
    | None -> env, Ok { top_level = 0; let_in = 0 }
  ;;

  let incr_counter name ident_typ env =
    let name = get_base_name name in
    let incr_counter counters env =
      let counters =
        match ident_typ with
        | LetIn -> { counters with let_in = counters.let_in + 1 }
        | TopLevel -> { counters with top_level = counters.top_level + 1 }
      in
      let env = Env.update env name counters in
      env, Ok ()
    in
    let env, counters = lookup_env_counters name env in
    match counters with
    | Ok counters -> incr_counter counters env
    | Error msg -> fail msg env
  ;;

  let zero = { top_level = 0; let_in = 0 }
  let update key value env = Env.update env key value, Ok ()
  let get_env env = env, Ok env
end

open R

let identifier_to_str = function
  | IdConstraint (s, _) -> s
  | Id s -> s
;;

open Common.Ident_utils

let gen_name_by_counters name counters = function
  | LetIn -> name ^ ".l" ^ string_of_int counters.let_in
  | TopLevel -> name ^ "." ^ string_of_int counters.top_level
;;

let gen_name name ident_typ =
  let name = get_base_name name in
  let* counters = lookup_env_counters name in
  return @@ gen_name_by_counters name counters ident_typ
;;

let get_new_id new_name = function
  | Id _ -> Id new_name
  | IdConstraint (_, typ) -> IdConstraint (new_name, typ)
;;

let rec has_occurs_iexpr name iexpr =
  match iexpr with
  | ImmIdentifier id -> ident_to_string id = name
  | ImmConstraint (imexpr, _) -> has_occurs_iexpr name imexpr
  | _ -> false (* TODO: Implement for all immediate expression types *)

and has_occurs_cexpr name cexpr =
  match cexpr with
  | CApp (cexpr1, cexpr2) -> has_occurs_cexpr name cexpr1 || has_occurs_cexpr name cexpr2
  | CIf (iexpr, aexpr1, aexpr2) ->
    has_occurs_iexpr name iexpr
    || has_occurs_aexpr name aexpr1
    || has_occurs_aexpr name aexpr2
  | CImmExpr iexpr -> has_occurs_iexpr name iexpr

and has_occurs_aexpr name aexpr =
  match aexpr with
  | ALetIn (id, cexpr, aexpr') ->
    let name_letin = identifier_to_str id in
    name_letin = name || has_occurs_cexpr name cexpr || has_occurs_aexpr name aexpr'
  | ACExpr cexpr -> has_occurs_cexpr name cexpr
;;

let rec find_replace_occurs_iexpr name new_name k = function
  | ImmIdentifier id when ident_to_string id = name ->
    let new_id =
      match id with
      | IdentOfDefinable x ->
        let ident_definable =
          match x with
          | IdentLetters _ -> IdentLetters new_name
          | IdentOp _ -> IdentOp new_name
        in
        IdentOfDefinable ident_definable
      | a -> a
    in
    let* _ =
      match k with
      | Some k -> k name
      | None -> return ()
    in
    return (ImmIdentifier new_id)
  | ImmConstraint (imexpr, typ) ->
    let* imexpr' = find_replace_occurs_iexpr name new_name k imexpr in
    return (ImmConstraint (imexpr', typ))
  | o -> return o

and find_replace_occurs_cexpr name new_name k = function
  | CApp (cexpr1, cexpr2) ->
    let* cexpr1' = find_replace_occurs_cexpr name new_name k cexpr1 in
    let* cexpr2' = find_replace_occurs_cexpr name new_name k cexpr2 in
    return @@ CApp (cexpr1', cexpr2')
  | CIf (iexpr, aexpr1, aexpr2) ->
    let* iexpr' = find_replace_occurs_iexpr name new_name k iexpr in
    let* aexpr1' = find_replace_occurs_aexpr name new_name k aexpr1 in
    let* aexpr2' = find_replace_occurs_aexpr name new_name k aexpr2 in
    return @@ CIf (iexpr', aexpr1', aexpr2')
  | CImmExpr iexpr ->
    let* iexpr' = find_replace_occurs_iexpr name new_name k iexpr in
    return @@ CImmExpr iexpr'

and find_replace_occurs_aexpr name new_name k = function
  | ALetIn (id, cexpr, aexpr) ->
    let name_letin = identifier_to_str id in
    let* k_name =
      let* counters = lookup_env_counters name_letin in
      let f name =
        let* counters_now = lookup_env_counters name in
        if counters != counters_now
        then return ()
        else
          let* _ = incr_counter name LetIn in
          return ()
      in
      return
      @@
      match k with
      | Some k ->
        Some
          (fun name ->
            let* _ = f name in
            k name)
      | None -> Some f
    in
    if name_letin = name
    then
      let* new_name_letin = gen_name name_letin LetIn in
      (* Generate new name only when it matches the target name *)
      let new_id = get_new_id new_name_letin id in
      let* cexpr' = find_replace_occurs_cexpr name new_name k_name cexpr in
      let* aexpr' = find_replace_occurs_aexpr name new_name_letin k_name aexpr in
      return @@ ALetIn (new_id, cexpr', aexpr')
    else
      (* Only rename name_letin if it occurs in the sub-expressions *)
      let* new_name_letin =
        let* _ =
          if has_occurs_cexpr name_letin cexpr || has_occurs_aexpr name_letin aexpr
          then
            let* _ = incr_counter name_letin LetIn in
            return ()
          else return ()
        in
        let* new_name_letin = gen_name name_letin LetIn in
        return new_name_letin
      in
      let* aexpr = find_replace_occurs_aexpr name_letin new_name_letin k aexpr in
      let* cexpr' = find_replace_occurs_cexpr name new_name k cexpr in
      let* aexpr' = find_replace_occurs_aexpr name new_name k aexpr in
      (* No k_name needed here *)
      (*Return the original name*)
      let new_id = get_new_id new_name_letin id in
      return @@ ALetIn (new_id, cexpr', aexpr')
  | ACExpr cexpr ->
    let* cexpr' = find_replace_occurs_cexpr name new_name k cexpr in
    return @@ ACExpr cexpr'
;;

let get_ident_of_lb = function
  | id, _, _ -> id
;;

let get_idents_of_lb = List.map get_ident_of_lb

let get_idents_of_decl = function
  | ADSingleLet (_, lb) -> [ get_ident_of_lb lb ]
  | ADMutualRecDecl (_, lb1, lb2, lbs) -> get_idents_of_lb (lb1 :: lb2 :: lbs)
;;

let map_ident_to_str = List.map identifier_to_str

let get_names_of_decl dlet =
  let strs = map_ident_to_str (get_idents_of_decl dlet) in
  List.map get_base_name strs
;;

let find_replace_occurs_lb name new_name (id, args, e) =
  let* e' = find_replace_occurs_aexpr name new_name None e in
  let* id =
    let name_of_decl = identifier_to_str id in
    if name_of_decl = name
    then
      let* _ = incr_counter name TopLevel in
      let* name = gen_name name TopLevel in
      return @@ get_new_id name id
    else return id
  in
  return (id, args, e')
;;

let find_replace_occurs_decl name = function
  | ADSingleLet (Not_recursive, lb) ->
    let* new_name = gen_name name TopLevel in
    let* lb' = find_replace_occurs_lb name new_name lb in
    return @@ ADSingleLet (Not_recursive, lb')
  | ADSingleLet (Recursive, ((id, _, _) as lb)) ->
    let* counters = lookup_env_counters name in
    let* counters =
      let name_of_decl = identifier_to_str id in
      if name_of_decl = name
      then return { counters with top_level = counters.top_level + 1 }
      else return counters
    in
    let new_name = gen_name_by_counters name counters TopLevel in
    let* lb' = find_replace_occurs_lb name new_name lb in
    return @@ ADSingleLet (Recursive, lb')
  | ADMutualRecDecl (Recursive, lb1, lb2, lb_tl) as dlet ->
    let names = get_names_of_decl dlet in
    if not @@ List.mem name names
    then return dlet
    else
      let* counters = lookup_env_counters name in
      let counters = { counters with top_level = counters.top_level + 1 } in
      let new_name = gen_name_by_counters name counters TopLevel in
      let* lb1' = find_replace_occurs_lb name new_name lb1 in
      let* lb2' = find_replace_occurs_lb name new_name lb2 in
      let* lb_tl' = map (find_replace_occurs_lb name new_name) lb_tl in
      return @@ ADMutualRecDecl (Recursive, lb1', lb2', lb_tl')
  | ADMutualRecDecl (Not_recursive, _, _, _) ->
    fail "Not recursive mutual decl is not supported yet"
;;

let find_replace_occurs_decls name prog =
  let rec helper acc = function
    | decl :: tl ->
      let* decl' = find_replace_occurs_decl name decl in
      let names = get_names_of_decl decl in
      let acc = decl' :: acc in
      if List.mem name names
      then return (List.concat [ List.rev tl; acc ])
      else helper acc tl
    | [] -> return acc
  in
  let* res_inv = helper [] prog in
  return (List.rev res_inv)
;;

let alpha_convert_prog (prog : anf_prog) =
  let rec helper acc = function
    | decl :: decls ->
      let names = get_names_of_decl decl in
      let rec fold_over_idents decl decls = function
        | name :: tl ->
          let* decl' = find_replace_occurs_decl name decl in
          let* decls' = find_replace_occurs_decls name decls in
          fold_over_idents decl' decls' tl
        | [] -> return (decl, decls)
      in
      let* decl', decls' = fold_over_idents decl decls names in
      let acc = decl' :: acc in
      helper acc decls'
    | [] -> return acc
  in
  let* res_inv = helper [] prog in
  return (List.rev res_inv)
;;

let init_env = Common.Stdlib.stdlib

let alpha_convert_prog prog =
  let run_env_filling =
    let rec fill_env = function
      | hd :: tl ->
        let* _ = update hd zero in
        fill_env tl
      | [] -> return ()
    in
    fill_env init_env
  in
  run
    (let* _ = run_env_filling in
     alpha_convert_prog prog)
;;
