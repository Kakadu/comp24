(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Anf_ast

let rec type_name_to_string tp =
  match tp with
  | TUnit -> "unit"
  | TInt -> "int"
  | TBool -> "bool"
  | TPoly name -> Printf.sprintf "'%s" name
  | TTuple lst ->
    let type_str =
      lst
      |> List.mapi (fun i tp ->
        if i <> 0
        then Printf.sprintf " * %s" (type_name_to_string tp)
        else type_name_to_string tp)
      |> String.concat ""
    in
    Printf.sprintf "(%s)" type_str
  | TFunction (tp_arg, tp_ret) ->
    Printf.sprintf "(%s -> %s)" (type_name_to_string tp_arg) (type_name_to_string tp_ret)
  | TList tp -> Printf.sprintf "(%s list)" (type_name_to_string tp)
;;

let list_to_string pp sep lst =
  let rec aux = function
    | [] -> ""
    | [ x ] -> pp x
    | x :: xs -> Printf.sprintf "%s%s%s" (pp x) sep (aux xs)
  in
  aux lst
;;

let rec imm_to_string = function
  | ImmInt i -> string_of_int i
  | ImmBool false -> "false"
  | ImmBool true -> "true"
  | ImmNil -> "[]"
  | ImmIdentifier id -> id
  | ImmUnit -> "()"
  | ImmTuple tup -> Printf.sprintf "(%s)" (list_to_string imm_to_string ", " tup)
  | ImmConstraint (imm, typ) ->
    Printf.sprintf "(%s : %s)" (imm_to_string imm) (type_name_to_string typ)
;;

let rec cexpr_to_string = function
  | CImmExpr imm -> imm_to_string imm
  | CIfThenElse (cond, then_branch, else_branch) ->
    Printf.sprintf
      "if %s then %s else %s"
      (imm_to_string cond)
      (aexpr_to_string then_branch)
      (aexpr_to_string else_branch)
  | CApplication (left, right, args) ->
    Printf.sprintf
      "%s %s %s"
      (imm_to_string left)
      (imm_to_string right)
      (args |> List.map imm_to_string |> String.concat " ")

and aexpr_to_string = function
  | ACExpr cexp -> cexpr_to_string cexp
  | ALetIn (pat, outer, inner) ->
    Printf.sprintf
      "let %s = %s in\n%s"
      pat
      (cexpr_to_string outer)
      (aexpr_to_string inner)
;;

let rec_flag_to_string = function
  | Rec -> "rec"
  | NotRec -> ""
;;

let anf_decl_to_string = function
  | ADSingleLet (rec_flag, ALet (pat, patterns, body)) ->
    Printf.sprintf
      "let %s %s %s = %s;;"
      (rec_flag_to_string rec_flag)
      pat
      (patterns |> String.concat " ")
      (aexpr_to_string body)
  | ADMutualRecDecl bindings ->
    let bindings_str =
      bindings
      |> List.mapi (fun i binding ->
        let binding_str =
          match binding with
          | ALet (pat, patterns, exp) ->
            Printf.sprintf
              "%s %s = %s"
              pat
              (patterns |> String.concat " ")
              (aexpr_to_string exp)
        in
        if i <> 0 then Printf.sprintf " and %s" binding_str else binding_str)
      |> String.concat ""
    in
    Printf.sprintf "let %s %s" "rec" bindings_str
;;

let program_to_string declarations =
  declarations |> List.map anf_decl_to_string |> String.concat "\n"
;;
