(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Anf_ast

let rec type_name_to_string tp =
  let rec_call tp = type_name_to_string tp in
  match tp with
  | TUnit -> "unit"
  | TInt -> "int"
  | TBool -> "bool"
  | TPoly name -> "'" ^ name
  | TTuple lst ->
    "("
    ^ (lst
       |> List.mapi (fun i tp -> if i <> 0 then " * " ^ rec_call tp else rec_call tp)
       |> String.concat "")
    ^ ")"
  | TFunction (tp_arg, tp_ret) -> "(" ^ rec_call tp_arg ^ " -> " ^ rec_call tp_ret ^ ")"
  | TList tp -> "(" ^ rec_call tp ^ " list)"
;;

let constant_to_string c =
  match c with
  | CInt i -> string_of_int i
  | CBool false -> "false"
  | CBool true -> "true"
  | CNil -> "[]"
  | CUnit -> "()"
;;

let list_to_string pp sep lst =
  let rec aux = function
    | [] -> ""
    | [ x ] -> pp x
    | x :: xs -> pp x ^ sep ^ aux xs
  in
  aux lst
;;

let rec imm_to_string c =
  match c with
  | ImmInt i -> string_of_int i
  | ImmBool false -> "false"
  | ImmBool true -> "true"
  | ImmNil -> "[]"
  | ImmIdentifier id -> id
  | ImmUnit -> "()"
  | ImmTuple tup -> "(" ^ list_to_string imm_to_string ", " tup ^ ")"
;;

let rec pattern_to_string pat =
  let rec_call = pattern_to_string in
  match pat with
  | PWildCard -> "_"
  | PCons (h_pat, t_pat) -> "(" ^ rec_call h_pat ^ " :: " ^ rec_call t_pat ^ ")"
  | PIdentifier x -> x
  | PTuple lst ->
    "("
    ^ (lst
       |> List.mapi (fun i pat -> if i <> 0 then ", " ^ rec_call pat else rec_call pat)
       |> String.concat "")
    ^ ")"
  | PConstant c -> constant_to_string c
  | PConstraint (pat, tp) -> "(" ^ rec_call pat ^ " : " ^ type_name_to_string tp ^ ")"
;;

let rec cexpr_to_string = function
  | CImmExpr imm -> imm_to_string imm
  | CIfThenElse (cond, then_branch, else_branch) ->
    "if "
    ^ imm_to_string cond
    ^ " then "
    ^ aexpr_to_string then_branch
    ^ " else "
    ^ aexpr_to_string else_branch
  | CMatch (pat_head, pat_exp_lst) ->
    "match "
    ^ pattern_to_string pat_head
    ^ " with\n"
    ^ (pat_exp_lst
       |> List.map (fun (pat, ae) ->
         "| " ^ pattern_to_string pat ^ " -> " ^ aexpr_to_string ae)
       |> String.concat "\n")
  | CApplication (left, right) -> cexpr_to_string left ^ " " ^ cexpr_to_string right
  | CConstraint (imm, typ) ->
    "(" ^ imm_to_string imm ^ " : " ^ type_name_to_string typ ^ ")"

and aexpr_to_string = function
  | ACExpr cexp -> cexpr_to_string cexp
  | ALetIn (pat, outer, inner) ->
    "let "
    ^ pattern_to_string pat
    ^ " = "
    ^ cexpr_to_string outer
    ^ " in\n"
    ^ aexpr_to_string inner
;;

let rec_flag_to_string = function
  | Rec -> "rec"
  | NotRec -> ""
;;

let anf_decl_to_string = function
  | ADSingleLet (rec_flag, ALet (pat, patterns, body)) ->
    "let "
    ^ rec_flag_to_string rec_flag
    ^ " "
    ^ pattern_to_string pat
    ^ " "
    ^ (patterns |> List.map pattern_to_string |> String.concat " ")
    ^ " = "
    ^ aexpr_to_string body
    ^ ";;"
  | ADMutualRecDecl (rec_flag, bindings) ->
    "let "
    ^ rec_flag_to_string rec_flag
    ^ " "
    ^ (bindings
       |> List.mapi (fun i binding ->
         if i <> 0
         then " and "
         else
           ""
           ^
           match binding with
           | ALet (pat, patterns, exp) ->
             pattern_to_string pat
             ^ " "
             ^ (patterns |> List.map pattern_to_string |> String.concat " ")
             ^ " = "
             ^ aexpr_to_string exp)
       |> String.concat "")
;;

let program_to_string declarations =
  declarations |> List.map anf_decl_to_string |> String.concat "\n"
;;
