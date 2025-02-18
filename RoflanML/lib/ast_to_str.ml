(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Common.Middleend_Common

let is_operator op =
  let is_operator_char = function
    | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | '%' | '&' | '|' | '^' | ':' -> true
    | _ -> false
  in
  String.fold op ~init:true ~f:(fun acc ch -> acc && is_operator_char ch)
;;

let ast_to_str =
  let open Stdlib.Format in
  let const_to_str = function
    | CInt x -> sprintf "%d" x
    | CBool x -> sprintf "%B" x
    | CUnit -> sprintf "()"
  in
  let rec type_to_str = function
    | TInt -> "int"
    | TBool -> "bool"
    | TUnit -> "unit"
    | TFun (l, r) ->
      (match l with
       | TFun _ -> sprintf "(%s) -> %s" (type_to_str l) (type_to_str r)
       | _ -> sprintf "%s -> %s" (type_to_str l) (type_to_str r))
    | TList ty ->
      (match ty with
       | TFun _ -> sprintf "(%s) list" (type_to_str ty)
       | _ -> sprintf "%s list" (type_to_str ty))
    | TTuple (ty1, ty2, tys) ->
      List.fold_left (ty1 :: ty2 :: tys) ~init:"" ~f:(fun acc ty ->
        if String.is_empty acc then type_to_str ty else acc ^ " * " ^ type_to_str ty)
  in
  let rec pattern_to_str = function
    | PWild -> "_"
    | PEmpty -> "[]"
    | PConst c -> const_to_str c
    | PVar x -> x
    | (PCons (p1, p2, ps) | POr (p1, p2, ps) | PTuple (p1, p2, ps)) as p ->
      let delim =
        match p with
        | PCons _ -> " :: "
        | POr _ -> " | "
        | PTuple _ -> ", "
        | _ -> failwith "Unreachable"
      in
      let init =
        match p1 with
        | PWild | PEmpty | PConst _ | PVar _ -> sprintf "%s" (pattern_to_str p1)
        | _ -> sprintf "(%s)" (pattern_to_str p1)
      in
      List.fold_left (p2 :: ps) ~init ~f:(fun acc p ->
        match p with
        | PWild | PEmpty | PConst _ | PVar _ ->
          sprintf "%s%s%s" acc delim (pattern_to_str p)
        | _ -> sprintf "%s%s(%s)" acc delim (pattern_to_str p))
  in
  let rec expr_to_str = function
    | EConst c -> const_to_str c
    | EVar x -> if is_operator x then sprintf "( %s )" x else x
    | ETuple (e1, e2, es) ->
      let init = expr_to_str e1 in
      List.fold_left (e2 :: es) ~init ~f:(fun acc e -> acc ^ ", " ^ expr_to_str e)
    | EList es ->
      let inner =
        List.fold_left es ~init:"" ~f:(fun acc e ->
          if String.is_empty acc then expr_to_str e else acc ^ "; " ^ expr_to_str e)
      in
      sprintf "[ %s ]" inner
    | EBranch (cond, t, f) ->
      sprintf "if %s then %s else %s" (expr_to_str cond) (expr_to_str t) (expr_to_str f)
    | EMatch (e, cases) ->
      let cases =
        List.fold_left cases ~init:"" ~f:(fun acc (p, e) ->
          acc ^ sprintf "\n| %s -> %s" (pattern_to_str p) (expr_to_str e))
      in
      sprintf "match %s with %s" (expr_to_str e) cases
    | ELetIn (is_rec, id, (EFun _ as e1), e2) ->
      let args, e = uncurry e1 in
      let args =
        List.fold_left args ~init:"" ~f:(fun acc (id, ty) ->
          match ty with
          | Some ty -> acc ^ sprintf "(%s : %s) " id (type_to_str ty)
          | None -> acc ^ sprintf "%s " id)
      in
      let rec_flag =
        match is_rec with
        | Rec -> " rec"
        | NonRec -> ""
      in
      sprintf "\nlet%s %s %s= %s in %s" rec_flag id args (expr_to_str e) (expr_to_str e2)
    | ELetIn (is_rec, id, e1, e2) ->
      let rec_flag =
        match is_rec with
        | Rec -> " rec"
        | NonRec -> ""
      in
      sprintf "\nlet%s %s = %s in %s" rec_flag id (expr_to_str e1) (expr_to_str e2)
    | EFun _ as e ->
      let args, e = uncurry e in
      let args =
        List.fold_left args ~init:"" ~f:(fun acc (id, ty) ->
          match ty with
          | Some ty -> acc ^ sprintf "(%s : %s) " id (type_to_str ty)
          | None -> acc ^ sprintf "%s " id)
      in
      sprintf "(fun %s-> %s)" args (expr_to_str e)
    | EApp (e1, e2) ->
      (match e2 with
       | EApp _ -> sprintf "%s (%s)" (expr_to_str e1) (expr_to_str e2)
       | _ -> sprintf "%s %s" (expr_to_str e1) (expr_to_str e2))
  in
  let decl_to_str = function
    | DLet (is_rec, id, (EFun _ as e)) ->
      let args, e = uncurry e in
      let args =
        List.fold_left args ~init:"" ~f:(fun acc (id, ty) ->
          match ty with
          | Some ty -> acc ^ sprintf "(%s : %s) " id (type_to_str ty)
          | None -> acc ^ sprintf "%s " id)
      in
      let rec_flag =
        match is_rec with
        | Rec -> " rec"
        | NonRec -> ""
      in
      sprintf "\nlet%s %s %s= %s" rec_flag id args (expr_to_str e)
    | DLet (is_rec, id, e) ->
      let rec_flag =
        match is_rec with
        | Rec -> " rec"
        | NonRec -> ""
      in
      sprintf "\nlet%s %s = %s" rec_flag id (expr_to_str e)
    | DMutualLet _ -> failwith "Not Implemented"
  in
  decl_to_str
;;
