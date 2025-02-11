(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Common.StateMonad

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

let rec alpha_convert_expr ctx = function
  | EConstant const -> (EConstant const, ctx) |> return
  | EIdentifier ident ->
    let ctx_with_id =
      { ctx with reserved_names = Base.Set.add ctx.reserved_names ident }
    in
    let old_name, counter = generate_unique_name ident ctx_with_id 0 in
    ( EIdentifier (Printf.sprintf "%s_%d" old_name counter)
    , { name_mapping =
          Base.Map.update ctx_with_id.name_mapping ident ~f:(fun existing_value ->
            match existing_value with
            | None | Some _ -> old_name, counter)
      ; reserved_names =
          Base.Set.add ctx.reserved_names (Printf.sprintf "%s_%d" old_name counter)
      } )
    |> return
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
  | _ -> fail "unimplemented yet"
;;

let test_alpha_for_expr str =
  match Parser.parse Parser.p_exp str with
  | Ok expr ->
    (match
       run
         (alpha_convert_expr
            { name_mapping = (module Base.String) |> Base.Map.empty
            ; reserved_names = (module Base.String) |> Base.Set.empty
            }
            expr)
         0
     with
     | _, Ok (expr, _) -> Format.printf "%a" Restore_src.RestoreSrc.frestore_expr expr
     | _, Error err -> Format.printf "%s" err)
  | Error err -> Format.printf "%s" err
;;

let%expect_test "" =
  test_alpha_for_expr "test";
  [%expect {|
    test_0 |}]
;;

let%expect_test "" =
  test_alpha_for_expr "if test then test else test";
  [%expect {|
    (if test_0 then test_1 else test_2) |}]
;;
