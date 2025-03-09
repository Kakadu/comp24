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
  | CEApply (a1, a_list) ->
    Base.List.fold_left
      ~init:a1
      ~f:(fun acc a -> "(" ^ acc ^ " " ^ atom_to_str a ^ ")")
      a_list
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
    Format.sprintf
      "%s = %s"
      (Base.List.fold args ~init:name ~f:(fun acc arg -> acc ^ " " ^ arg))
      (exp_to_str body)
;;

let str_item_to_str = function
  | ADNoRec func_list ->
    let fun1 = Base.List.hd_exn func_list in
    let tl = Base.List.tl_exn func_list in
    Base.List.fold_left
      tl
      ~init:(Format.sprintf "let %s" (fun_to_str fun1))
      ~f:(fun acc fun1 -> acc ^ "\nand " ^ fun_to_str fun1)
  | ADREC func_list ->
    let fun1 = Base.List.hd_exn func_list in
    let tl = Base.List.tl_exn func_list in
    Base.List.fold_left
      tl
      ~init:(Format.sprintf "let rec %s" (fun_to_str fun1))
      ~f:(fun acc fun1 -> acc ^ "\nand " ^ fun_to_str fun1)
;;

let pp_anf_structure ppf p =
  let len = List.length p in
  Base.List.iteri
    ~f:(fun i a ->
      if i = len - 1
      then Format.fprintf ppf "%s" (str_item_to_str a)
      else Format.fprintf ppf "%s\n\n" (str_item_to_str a))
    p
;;