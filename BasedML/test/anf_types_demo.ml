(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let () =
  let is_stdlib target_name =
    List.exists (fun (nm, _, _, _) -> target_name = nm) Stdlib_funs.stdlib_funs
  in
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse_program s with
  | Ok ast_before ->
    let final =
      ast_before
      |> Middleend.Closure_conversion.convert_ast
      |> Middleend.Lambda_lifting.lift_ast
      |> Middleend.Anf.transform
    in
    let rec anf_helper acc = function
      | Ok h :: tl -> anf_helper (Middleend.Anf_to_str.anf_decl_to_string h ^ acc) tl
      | [] -> acc
      | _ -> "Error "
    in
    (match Parser.parse_program (anf_helper "" (List.rev final)) with
     | Ok ast_after ->
       (match Typeinference.infer_prog ast_before, Typeinference.infer_prog ast_after with
        | Ok map1, Ok map2 ->
          let all_types_match =
            Typeinference.StringMap.for_all
              (fun key typ1 ->
                if is_stdlib key
                then true
                else (
                  match Typeinference.StringMap.find_opt key map2 with
                  | Some typ2 ->
                    Format.printf "Name: %s\n" key;
                    Format.printf "Original program: %a\n" Ast.pp_type_name typ1;
                    Format.printf "ANF: %a\n\n" Ast.pp_type_name typ2;
                    if not (Typeinference.types_equal typ1 typ2)
                    then (
                      Format.printf "There's been a discrepancy for name %s\n" key;
                      false)
                    else true
                  | None ->
                    Format.printf "Name %s found in map1 but not in map2\n" key;
                    false))
              map1
          in
          if all_types_match
          then Format.printf "All types are correct\n"
          else Format.printf "There's been a type mismatch\n"
        | _ -> Format.printf "Error while typechecking\n")
     | _ -> Format.printf "Error while parsing ANF program\n")
  | _ -> Format.printf "Error while parsing original program\n"
;;
