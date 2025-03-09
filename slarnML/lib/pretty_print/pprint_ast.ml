(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast
open Parser
open Printf
open String
open Typedtree

let pp_const = function
  | CInt i -> if i < 0 then (concat "" ["("; string_of_int i; ")"]) else (string_of_int i)
  | CBool b -> string_of_bool b
  | CUnit -> "()"
;;
let expr_of_decl = function
  | Decl(id, args) -> concat " " (id :: args)
  | DeclRec(id, args) -> concat " " ("rec" :: (id :: args))
;;

let rec pp_expr expr =
  match expr with
  | Id (id) -> id
  | Const(c) -> pp_const c
  | Not (e) ->            concat "" ["not "; pp_expr e]
  | Or (e1, e2) ->        concat "" ["("; pp_expr e1; "||"; pp_expr e2; ")"]
  | And (e1, e2) ->       concat "" ["("; pp_expr e1; "&&"; pp_expr e2; ")"]
  | Eq (e1, e2) ->        concat "" ["("; pp_expr e1; "="; pp_expr e2; ")"]
  | Gt (e1, e2) ->        concat "" ["("; pp_expr e1; ">"; pp_expr e2; ")"]
  | Lt (e1, e2) ->        concat "" ["("; pp_expr e1; "<"; pp_expr e2; ")"]
  | Gte (e1, e2) ->       concat "" ["("; pp_expr e1; ">="; pp_expr e2; ")"]
  | Lte (e1, e2) ->       concat "" ["("; pp_expr e1; "<="; pp_expr e2; ")"]
  | Add (e1, e2) ->       concat "" ["("; pp_expr e1; "+"; pp_expr e2; ")"]
  | Sub (e1, e2) ->       concat "" ["("; pp_expr e1; "-"; pp_expr e2; ")"]
  | Mul (e1, e2) ->       concat "" ["("; pp_expr e1; "*"; pp_expr e2; ")"]
  | Div (e1, e2) ->       concat "" ["("; pp_expr e1; "/"; pp_expr e2; ")"]
  | If (e1, e2, e3) ->    concat "" ["if ("; pp_expr e1; ") then ("; pp_expr e2; ") else ("; pp_expr e3; ")"]
  | Let (d, e2) ->        concat "" ["(let "; expr_of_decl d; "="; pp_expr e2; ")"]
  | LetIn (d, e2, e3) ->  concat "" ["(let "; expr_of_decl d; "="; pp_expr e2; " in "; pp_expr e3; ")"]
  | Fun (args, e) ->      concat "" ["(fun ";  concat " " args; "->"; pp_expr e; ")"]
  | App (e, args) ->      concat "" ["("; pp_expr e; "->"; concat "->" (List.map pp_expr args); ")"]
;;

let pp_exprs exprs =
  concat "\t\n" @@ List.map pp_expr exprs
;;

let print_expr str =
  match parser str with
    | Ok(e) -> print_string @@ concat "\t\t\n" [pp_exprs e; ";;\n\n"]
    | Error(e) -> eprintf "there was an error: %s\n" e
;;

let pp_ast ast =
  print_string @@ pp_expr ast
;;

let pp_const_ty = function
| UnitTy -> "Unit"
| BoolTy -> "Bool"
| IntTy -> "Int"
;;

let rec pp_type = function
| PrimTy c -> pp_const_ty c
| VarTy n -> string_of_int n
| ArrowTy types -> concat "" ["("; concat "->" (List.map pp_type types); ")"]
;;



(*
" not (not ( not_1 ) + 1 *2 / 1 + 2)"
"let a = \nlet b = 1 in\n\t let c = b in\n\t c   "
"let t = let k = if a then (let rec a x = x) else (fun x b -> 2*b+x) in k;;"
"let t = let k = if a then (let rec a x = x) else (fun x b -> 2*b+x) in k;; let q = 0"
*)
(*
string " not (not ( not_1 ) + 1 *2 / 1 + 2)" (fun _ -> printf ";;\n"; ());;
*)
(* string "let a b c = 1;; a b c;; let b = a c" (fun _ -> printf ";;\n"; ());; *)
(* print_expr "a + 2 \n\n<= b * 3";
print_string "\n\n\n";
print_expr "let a b c = 1;; a b c;; let b = a c";;
print_string "\n\n\n";
print_expr "(a b) * 3 + (b 1 (a f 2))"
print_string "\n\n\n"; *)
(* print_expr "(a b 2 1+3 * b d + 2)" *)
(* print_expr "(a b 1)" *)
(* print_expr "(a b 2 1+3 * b d (-2) (r f) true) + 3 = 10 && false || 1" ;; *)
(* print_expr "let a = let b = 1 in let c = 2 in d" *)
(* print_expr "true && (a + (f false (g 3 y)) = 3  || 2)";; *)
(* + (b 1 (a f 2) (1 + a)) *)