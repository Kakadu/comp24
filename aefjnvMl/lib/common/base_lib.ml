(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module LibF = struct
  type rt_alias = Alias of string

  type vararg =
    | Varargs of core_type
    | NoVarargs

  type env_func =
    | StdF of string * rt_alias * (int * Ast.core_type * vararg)
    | SysF of string * rt_alias * (int * Ast.core_type * vararg)

  let get_row_func = function
    | StdF (name, alias, tp) | SysF (name, alias, tp) -> name, alias, tp
  ;;

  let get_name f =
    let name, _, _ = get_row_func f in
    name
  ;;

  let get_type f =
    let _, _, tp = get_row_func f in
    tp
  ;;

  let get_rt_alias f =
    let _, Alias al, _ = get_row_func f in
    al
  ;;

  let get_row_type =
    let rec to_row_tp = function
      | Ptyp_arrow (tp1, tp2) ->
        let args1, lst1 = to_row_tp tp1 in
        let args2, lst2 = to_row_tp tp2 in
        args1 @ [ lst1 ] @ args2, lst2
      | x -> [], x
    in
    function
    | StdF (_, _, (_, tp, var_tp)) | SysF (_, _, (_, tp, var_tp)) -> var_tp, to_row_tp tp
  ;;

  let rt_pref = "rt"
  let vararg = "_varargs"
  let rt nm = rt_pref ^ "_" ^ nm

  let rt' (Alias rt_name) =
    let prefix_n = String.length rt_pref + 1 in
    String.sub rt_name prefix_n (String.length rt_name - prefix_n)
  ;;

  let alias nm = Alias (rt nm)
  let func1 argt1 ~ret = 1, Ptyp_arrow (argt1, ret), NoVarargs
  let func2 argt1 argt2 ~ret = 2, Ptyp_arrow (argt1, Ptyp_arrow (argt2, ret)), NoVarargs
  let un op = "~" ^ op

  (* FUNCS *)
  let op_mul = StdF ("*", alias "mul", func2 Ptyp_int Ptyp_int ~ret:Ptyp_int)
  let op_div = StdF ("/", alias "div", func2 Ptyp_int Ptyp_int ~ret:Ptyp_int)
  let op_plus = StdF ("+", alias "add", func2 Ptyp_int Ptyp_int ~ret:Ptyp_int)
  let op_minus = StdF ("-", alias "sub", func2 Ptyp_int Ptyp_int ~ret:Ptyp_int)

  let op_less_eq =
    StdF ("<=", alias "leq", func2 (Ptyp_var "'_a") (Ptyp_var "'_a") ~ret:Ptyp_bool)
  ;;

  let op_more_eq =
    StdF (">=", alias "meq", func2 (Ptyp_var "'_a") (Ptyp_var "'_a") ~ret:Ptyp_bool)
  ;;

  let op_less =
    StdF ("<", alias "less", func2 (Ptyp_var "'_a") (Ptyp_var "'_a") ~ret:Ptyp_bool)
  ;;

  let op_more =
    StdF (">", alias "more", func2 (Ptyp_var "'_a") (Ptyp_var "'_a") ~ret:Ptyp_bool)
  ;;

  let op_eq =
    StdF ("=", alias "eq", func2 (Ptyp_var "'_a") (Ptyp_var "'_a") ~ret:Ptyp_bool)
  ;;

  let op_eq2 =
    StdF ("==", alias "eq2", func2 (Ptyp_var "'_a") (Ptyp_var "'_a") ~ret:Ptyp_bool)
  ;;

  let op_not_eq =
    StdF ("<>", alias "neq", func2 (Ptyp_var "'_a") (Ptyp_var "'_a") ~ret:Ptyp_bool)
  ;;

  let op_and = StdF ("&&", alias "and", func2 Ptyp_bool Ptyp_bool ~ret:Ptyp_bool)
  let op_or = StdF ("||", alias "or", func2 Ptyp_bool Ptyp_bool ~ret:Ptyp_bool)
  let f'sub' = "-"
  let un_op_minus = StdF (un f'sub', alias "unminus", func1 Ptyp_int ~ret:Ptyp_int)
  let func_print_int = StdF ("print_int", alias "print_int", func1 Ptyp_int ~ret:Ptyp_unit)

  (* INTERN *)
  let get_by_idx =
    SysF
      ( "get_by_idx"
      , alias "get_by_idx"
      , func2 (Ptyp_var "'_a") Ptyp_int ~ret:(Ptyp_var "'_b") )
  ;;

  let get_list_len =
    SysF ("get_list_len", alias "get_list_len", func1 (Ptyp_var "'_a") ~ret:Ptyp_int)
  ;;

  let get_list_tail =
    SysF
      ( "get_list_tail"
      , alias "get_list_tail"
      , func2 (Ptyp_var "'_a") Ptyp_int ~ret:(Ptyp_var "'_a") )
  ;;

  let part_match_fail =
    SysF ("part_match_fail", alias "fail", func1 Ptyp_unit ~ret:(Ptyp_var "'_a"))
  ;;

  let create_empty_closure =
    SysF
      ( "rt_create_empty_closure"
      , alias "alloc_closure"
      , func2 Ptyp_int Ptyp_int ~ret:Ptyp_int )
  ;;

  let func1_varargs argt1 ~ret ~va_tp = 2, Ptyp_arrow (argt1, ret), Varargs va_tp

  let func2_varargs argt1 argt2 ~ret ~va_tp =
    2, Ptyp_arrow (argt1, Ptyp_arrow (argt2, ret)), Varargs va_tp
  ;;

  let apply_arguments =
    SysF
      ( "rt_apply"
      , alias "apply_to_closure"
      , func2_varargs Ptyp_int Ptyp_int ~ret:Ptyp_int ~va_tp:Ptyp_int )
  ;;

  (** [append_to_list(n, tl, __reversed_args__)]
      - [tl] -- it is pointer to parrent
      - struct list <=> llist *)
  let append_to_list =
    SysF
      ( "rt_append_to_list"
      , alias "append_to_list"
      , func2_varargs Ptyp_int Ptyp_int ~ret:Ptyp_int ~va_tp:Ptyp_int )
  ;;

  let alloc_tuple =
    SysF
      ( "rt_alloc_tuple"
      , alias "alloc_tuple"
      , func1_varargs Ptyp_int ~ret:Ptyp_int ~va_tp:Ptyp_int )
  ;;
end

open LibF

let sys_funcs =
  [ get_by_idx
  ; get_list_tail
  ; get_list_len
  ; part_match_fail
  ; create_empty_closure
  ; apply_arguments
  ; append_to_list
  ; alloc_tuple
  ]
;;

let user_funcs =
  let math_funcs = [ un_op_minus; op_mul; op_div; op_plus; op_minus ] in
  let eq_funcs = [ op_less_eq; op_less; op_more_eq; op_more; op_eq2; op_eq; op_not_eq ] in
  let bool_funcs = [ op_and; op_or ] in
  let spec_funcs = [ func_print_int ] in
  List.concat [ math_funcs; eq_funcs; bool_funcs; spec_funcs; sys_funcs ]
;;

let base_lib_decls = sys_funcs @ user_funcs

module DeclMap = Stdlib.Map.Make (String)

let get_op_decls =
  let decls =
    List.filter
      (function
        | StdF (name, _, _) ->
          (match String.get name 0 with
           | 'a' .. 'z' | 'A' .. 'Z' | '_' -> false
           | _ -> true)
        | _ -> false)
      user_funcs
  in
  List.map
    (function
      | StdF (nm, alias, tp) -> nm, alias, tp
      | SysF _ -> raise @@ Invalid_argument "Ops can be only from stdlib")
    decls
;;

let get_infix_ops =
  let ops = get_op_decls in
  List.filter
    (function
      | _, _, (2, _, _) -> true
      | _ -> false)
    ops
;;

let get_un_ops =
  let ops = get_op_decls in
  List.filter
    (function
      | _, _, (1, _, _) -> true
      | _ -> false)
    ops
;;

let create_named_map =
  List.fold_left (fun acc decl -> DeclMap.add (get_name decl) decl acc) DeclMap.empty
;;

let base_lib_map = create_named_map base_lib_decls
let find_by_name : string -> env_func = fun name -> DeclMap.find name base_lib_map

let is_binop name =
  match DeclMap.find_opt name base_lib_map with
  | Some (StdF (_, _, (2, _, _))) -> true
  | _ -> false
;;

let op_map =
  create_named_map @@ Base.List.map get_op_decls ~f:(fun (a, b, c) -> StdF (a, b, c))
;;

let converte_op op =
  match DeclMap.find_opt op op_map with
  | Some f -> rt' @@ Alias (get_rt_alias f)
  | None -> op
;;
