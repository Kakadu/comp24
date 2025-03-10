(* Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Roflanml_lib
open Roflanml_lib.Ast
open Roflanml_lib.Typing
open Roflanml_lib.Roflanml_stdlib
open Roflanml_lib.Closure_conversion
open Roflanml_lib.Lambda_lifting
open Roflanml_lib.Anf_ast
open Roflanml_lib.Anf
open Ast_to_str

let get_bindings prog =
  List.fold
    prog
    ~init:(Set.empty (module String))
    ~f:(fun acc decl ->
      match decl with
      | DLet (_, id, _) -> Set.add acc id
      | DMutualLet (_, decls) ->
        List.fold decls ~init:acc ~f:(fun acc (id, _) -> Set.add acc id))
;;

let () =
  let open Result in
  let ( let* ) = ( >>= ) in
  let env = RoflanML_Stdlib.default |> Map.keys |> Set.of_list (module String) in
  let input = Stdio.In_channel.input_all Stdlib.stdin in
  let tyenvs =
    let* prog = Parser.parse input in
    let bindings = get_bindings prog in
    let* before_tyenv =
      match Typechecker.typecheck prog with
      | Ok tyenv ->
        return
          (Map.fold
             tyenv
             ~init:(Map.empty (module String))
             ~f:(fun ~key ~data acc ->
               if Set.mem bindings key
               then (
                 let (Typechecker.Scheme.S (_, ty)) = data in
                 Map.update acc key ~f:(fun _ -> ty))
               else acc))
      | Error err ->
        Stdlib.Format.printf "%a" pp_error err;
        fail "Typecheck error"
    in
    let prog = close_program prog env in
    let* prog = lift_program prog in
    let* prog = anf_program prog in
    let prog_str =
      List.fold_left prog ~init:"" ~f:(fun acc decl ->
        acc ^ "\n" ^ ast_to_str @@ anf_to_ast @@ decl)
    in
    let* prog =
      match Parser.parse prog_str with
      | Ok prog -> return prog
      | Error _ -> fail "Failed to parse after anf"
    in
    let* anf_tyenv =
      match Typechecker.typecheck prog with
      | Ok tyenv ->
        return
          (Map.fold
             tyenv
             ~init:(Map.empty (module String))
             ~f:(fun ~key ~data acc ->
               if Set.mem bindings key
               then (
                 let (Typechecker.Scheme.S (_, ty)) = data in
                 Map.update acc key ~f:(fun _ -> ty))
               else acc))
      | Error err ->
        Stdlib.Format.printf "%a" pp_error err;
        fail "Typecheck error after ANF"
    in
    return (before_tyenv, anf_tyenv)
  in
  match tyenvs with
  | Ok (before_tyenv, anf_tyenv) ->
    Stdlib.Format.printf "TypeEnv before ANF:\n%!";
    Map.fold before_tyenv ~init:() ~f:(fun ~key ~data _ ->
      Stdlib.Format.printf "%s: %a\n%!" key pp_ty data;
      ());
    Stdlib.Format.printf "TypeEnv after ANF:\n%!";
    Map.fold anf_tyenv ~init:() ~f:(fun ~key ~data _ ->
      Stdlib.Format.printf "%s: %a\n%!" key pp_ty data;
      ());
    let tyenvs_eq =
      Map.fold anf_tyenv ~init:true ~f:(fun ~key ~data acc ->
        if Typechecker.type_eq data (Map.find_exn before_tyenv key)
        then true && acc
        else false)
    in
    if tyenvs_eq
    then Stdlib.Format.printf "\nAll types are equal!%!"
    else Stdlib.Format.printf "\nTypes mismatch!%!"
  | Error err -> Stdlib.print_endline err
;;
