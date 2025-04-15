(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type immexpr =
  | ImmInt of int
  | ImmIdentifier of string
  | ImmBool of bool
  | ImmUnit
  | ImmNill
  | ImmTuple of immexpr list

type cexpr =
  | CEApply of string * immexpr list
  | CEIf of immexpr * aexpr * aexpr
  | CECons of immexpr * immexpr
  | CImmExpr of immexpr

and aexpr =
  | ALetIn of string * cexpr * aexpr
  | ACExpr of cexpr

type anf_binding = ALet of string * string list * aexpr

type anf_decl =
  | Based_value of string * aexpr
  | ADNoRec of anf_binding list
  | ADREC of anf_binding list

type anf_prog = anf_decl list

let imm_id id = ImmIdentifier id
let cimmexpr immexpr = CImmExpr immexpr

let rec atom_to_str = function
  | ImmInt i -> Int.to_string i
  | ImmBool b -> Bool.to_string b
  | ImmUnit -> "()"
  | ImmIdentifier v -> v
  | ImmNill -> "[]"
  | ImmTuple l ->
    Format.sprintf
      "(%s)"
      (atom_to_str (Base.List.hd_exn l)
       ^ Base.List.fold_left
           ~f:(fun acc e -> acc ^ Format.sprintf ", %s" (atom_to_str e))
           ~init:""
           (Base.List.tl_exn l))
;;

let rec cexp_to_str = function
  | CImmExpr a -> atom_to_str a
  | CEApply (a1, a_list) -> String.concat " " (a1 :: List.map atom_to_str a_list)
  | CEIf (e1, e2, e3) ->
    Format.sprintf
      "if %s\nthen %s\nelse %s"
      (atom_to_str e1)
      (exp_to_str e2)
      (exp_to_str e3)
  | CECons (e1, e2) -> Format.sprintf "(%s::%s)" (atom_to_str e1) (atom_to_str e2)

and exp_to_str = function
  | ALetIn (name, c, e) ->
    Format.sprintf "let %s = %s in\n%s" name (cexp_to_str c) (exp_to_str e)
  | ACExpr e -> cexp_to_str e
;;

let fun_to_str = function
  | ALet (name, args, body) ->
    Format.sprintf "%s = %s" (String.concat " " (name :: args)) (exp_to_str body)
;;

let declaration_to_str = function
  | Based_value (name, e) -> Format.sprintf "let %s = %s\n;;" name (exp_to_str e)
  | ADNoRec func_list ->
    let funs = List.map fun_to_str func_list in
    "let " ^ String.concat "\nand " funs ^ "\n;;"
  | ADREC func_list ->
    let funs = List.map fun_to_str func_list in
    "let rec " ^ String.concat "\nand " funs ^ "\n;;"
;;

let pp_anf_program ppf p =
  let len = List.length p in
  Base.List.iteri
    ~f:(fun i a ->
      if i = len - 1
      then Format.fprintf ppf "%s" (declaration_to_str a)
      else Format.fprintf ppf "%s\n\n" (declaration_to_str a))
    p
;;
