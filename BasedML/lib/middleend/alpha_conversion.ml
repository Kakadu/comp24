(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Common.StateMonad
open Stdlib_funs

type context =
  { name_mapping : (string, string * int, Base.String.comparator_witness) Base.Map.t
  ; reserved_names : (string, Base.String.comparator_witness) Base.Set.t
  }

let rec generate_unique_name old_name ctx counter =
  let new_name = old_name, counter in
  if Base.Set.mem
       (Base.Set.union
          ctx.reserved_names
          (Base.Set.singleton (module Base.String) old_name))
       (Printf.sprintf "%s_%d" old_name counter)
  then generate_unique_name old_name ctx (counter + 1)
  else new_name
;;

let get_id id ctx =
  match Base.Map.find ctx.name_mapping id with
  | None -> fail "No name was found in map"
  | Some (old_name, counter) -> Printf.sprintf "%s_%d" old_name counter |> return
;;

let rec collect_function_arguments collected = function
  | EFunction (pat, next) -> collect_function_arguments (pat :: collected) next
  | expr -> List.rev collected, expr
;;

let rec construct_function (patterns : pattern list) (body : expr) : expr =
  match patterns with
  | [] -> body
  | p :: ps -> EFunction (p, construct_function ps body)
;;

let rec alpha_convert_pattern ctx = function
  | PConstant const -> (PConstant const, ctx) |> return
  | PWildCard -> (PWildCard, ctx) |> return
  | PIdentifier ident ->
    let ctx_with_id =
      { ctx with reserved_names = Base.Set.add ctx.reserved_names ident }
    in
    let old_name, counter = generate_unique_name ident ctx_with_id 0 in
    ( PIdentifier (Printf.sprintf "%s_%d" old_name counter)
    , { name_mapping =
          Base.Map.update ctx_with_id.name_mapping ident ~f:(fun existing_value ->
            match existing_value with
            | None | Some _ -> old_name, counter)
      ; reserved_names =
          Base.Set.add ctx.reserved_names (Printf.sprintf "%s_%d" old_name counter)
      } )
    |> return
  | PCons (left, right) ->
    let* renamed_left, ctx_after_left = alpha_convert_pattern ctx left in
    let* renamed_right, ctx_after_right = alpha_convert_pattern ctx_after_left right in
    (PCons (renamed_left, renamed_right), ctx_after_right) |> return
  | PConstraint (pat, typ) ->
    let* renamed_pat, ctx_after_pat = alpha_convert_pattern ctx pat in
    (PConstraint (renamed_pat, typ), ctx_after_pat) |> return
  | _ -> fail "pattern: unimplemented yet"
;;

let rec alpha_convert_expr ctx = function
  | EConstant const -> (EConstant const, ctx) |> return
  (* | EIdentifier ident when Base.Set.mem Utils.stdlib_names ident ->
     (EIdentifier ident, ctx) |> return *)
  | EIdentifier ident ->
    let ctx_with_id =
      { ctx with reserved_names = Base.Set.add ctx.reserved_names ident }
    in
    (match Base.Map.find ctx.name_mapping ident with
     | None ->
       let old_name, counter = generate_unique_name ident ctx_with_id 0 in
       ( EIdentifier (Printf.sprintf "unbound_%s_%d" old_name counter)
       , { name_mapping =
             Base.Map.update ctx_with_id.name_mapping ident ~f:(fun existing_value ->
               match existing_value with
               | None | Some _ -> old_name, counter)
         ; reserved_names =
             Base.Set.add
               ctx.reserved_names
               (Printf.sprintf "unbound_%s_%d" old_name counter)
         } )
       |> return
     | Some (old_name, counter) ->
       (EIdentifier (Printf.sprintf "%s_%d" old_name counter), ctx) |> return)
  | ELetIn (flag, PIdentifier main_id, EFunction (fun_pat, fun_body), inner) ->
    let rec args_rename_helper acc helper_context = function
      | [] -> (List.rev acc, helper_context) |> return
      | h :: tl ->
        let* renamed_h, ctx_after_h = alpha_convert_pattern helper_context h in
        args_rename_helper (renamed_h :: acc) ctx_after_h tl
    in
    let args, main_body = collect_function_arguments [] (EFunction (fun_pat, fun_body)) in
    let* new_main_id, ctx_after_main_pat =
      alpha_convert_pattern ctx (PIdentifier main_id)
    in
    (match flag with
     | Rec ->
       let* renamed_args, ctx_after_args =
         args_rename_helper [] ctx_after_main_pat args
       in
       let* renamed_body, _ = alpha_convert_expr ctx_after_args main_body in
       let* renamed_inner, _ = alpha_convert_expr ctx_after_main_pat inner in
       ( ELetIn
           (flag, new_main_id, construct_function renamed_args renamed_body, renamed_inner)
       , ctx )
       |> return
     | NotRec ->
       let* renamed_args, ctx_after_args = args_rename_helper [] ctx args in
       let* renamed_body, _ = alpha_convert_expr ctx_after_args main_body in
       let* renamed_inner, _ = alpha_convert_expr ctx_after_main_pat inner in
       ( ELetIn
           (flag, new_main_id, construct_function renamed_args renamed_body, renamed_inner)
       , ctx )
       |> return)
  | EIfThenElse (guard_branch, then_branch, else_branch) ->
    let* renamed_guard_branch, ctx_after_guard_branch =
      alpha_convert_expr ctx guard_branch
    in
    let* renamed_then_branch, ctx_after_then_branch =
      alpha_convert_expr ctx_after_guard_branch then_branch
    in
    let* renamed_else_branch, ctx_after_else_branch =
      alpha_convert_expr ctx_after_then_branch else_branch
    in
    ( EIfThenElse (renamed_guard_branch, renamed_then_branch, renamed_else_branch)
    , ctx_after_else_branch )
    |> return
  | EApplication (left, right) ->
    let* renamed_left, ctx_after_left = alpha_convert_expr ctx left in
    let* renamed_right, ctx_after_right = alpha_convert_expr ctx_after_left right in
    (EApplication (renamed_left, renamed_right), ctx_after_right) |> return
  | _ -> fail "unimplemented yet: expression"
;;

let rec alpha_convert_decl_list ctx acc = function
  | h :: tl ->
    (match h with
     | DSingleLet (rec_flag, DLet (pat, expr)) ->
       (* TODO: What about let test = fun x -> ... ? *)
       (* TODO: proccess differently for rec or not rec *)
       let* new_pat, ctx_after_pat = alpha_convert_pattern ctx pat in
       let* new_expr, _ = alpha_convert_expr ctx_after_pat expr in
       alpha_convert_decl_list
         ctx_after_pat
         (DSingleLet (rec_flag, DLet (new_pat, new_expr)) :: acc)
         tl
     | _ -> fail "unimplemented")
  | [] -> List.rev acc |> return
;;

let add_to_context_std_fun (ctx : context) ((name, llvm_name, _) : std_fun) =
  let reserved_names = Base.Set.add ctx.reserved_names llvm_name in
  let name_map = Base.Map.add_exn ctx.name_mapping ~key:name ~data:(llvm_name, -1) in
  { name_mapping = name_map; reserved_names }
;;

let add_to_context_all_std_funs ctx =
  List.fold_left add_to_context_std_fun ctx stdlib_funs
;;

let init_context =
  let empty_context =
    { name_mapping = (module Base.String) |> Base.Map.empty
    ; reserved_names = (module Base.String) |> Base.Set.empty
    }
  in
  add_to_context_all_std_funs empty_context
;;

let test_alpha_for_expr str =
  match Parser.parse Parser.p_exp str with
  | Ok expr ->
    (match run (alpha_convert_expr init_context expr) 0 with
     | _, Ok (expr, _) -> Format.printf "%a" Restore_src.RestoreSrc.frestore_expr expr
     | _, Error err -> Format.printf "%s" err)
  | Error err -> Format.printf "%s" err
;;

let test_alpha_for_decls str =
  match Parser.parse_program str with
  | Ok decls ->
    (match run (alpha_convert_decl_list init_context [] decls) 0 with
     | _, Ok lst -> Format.printf "%s" (Restore_src.RestoreSrc.restore_declarations lst)
     | _, Error err -> Format.printf "%s" err)
  | Error err -> Format.printf "%s" err
;;

let%expect_test "" =
  test_alpha_for_expr "test";
  [%expect {|
    unbound_test_0 |}]
;;

let%expect_test "" =
  test_alpha_for_expr "if test then test else test";
  [%expect {|
    (if unbound_test_0 then test_0 else test_0) |}]
;;

let%expect_test "" =
  test_alpha_for_expr "let f a b c = a + b + c + f in a";
  [%expect
    {|
    (let  f_0 = (fun a_0 -> (fun b_0 -> (fun c_0 -> ((plus_mlint_-1 ((plus_mlint_-1 ((plus_mlint_-1 a_0) b_0)) c_0)) unbound_f_0)))) in unbound_a_0) |}]
;;

let%expect_test "" =
  test_alpha_for_expr "let rec f a b c = a + b + c + f in a";
  [%expect
    {|
    (let rec f_0 = (fun a_0 -> (fun b_0 -> (fun c_0 -> ((plus_mlint_-1 ((plus_mlint_-1 ((plus_mlint_-1 a_0) b_0)) c_0)) f_0)))) in unbound_a_0) |}]
;;

let%expect_test "" =
  test_alpha_for_decls {|
let f = 5 + 5
let g = f + 10
let f = 6 + 6  
|};
  [%expect
    {|
    let  f_0 = ((plus_mlint_-1 5) 5)
    let  g_0 = ((plus_mlint_-1 f_0) 10)
    let  f_1 = ((plus_mlint_-1 6) 6) |}]
;;
