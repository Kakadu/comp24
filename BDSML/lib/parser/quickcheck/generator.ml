(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Parser.Ast
open QCheck.Gen
open Generator_utils

let gen_constant =
  oneof
    [ (let+ x = small_int in
       Const_int x)
    ; (let+ c = printable in
       match c with
       | '\'' -> Const_char ' '
       | _ -> Const_char c)
    ; (let+ s =
         map
           (fun s -> String.map (fun c -> if c = '"' then ' ' else c) s)
           (string_size @@ int_range 2 5)
       in
       Const_string s)
    ; (let+ b = bool in
       Const_bool b)
    ]
;;

let rec gen_typexpr = function
  | 0 ->
    let+ s = gen_ident in
    Type_single s
  | depth ->
    oneof
      [ (let+ l = gen_list_helper gen_typexpr depth in
         Type_fun l)
      ; (let+ l = gen_list_helper gen_typexpr depth in
         Type_tuple l)
      ; (let+ t = gen_typexpr (depth / 2)
         and+ id = gen_ident in
         Type_constructor_param (t, id))
      ]
;;

let gen_rec = oneof [ return Recursive; return Nonrecursive ]

let rec gen_pattern ?(top_level = false) = function
  | 0 ->
    let gen_list =
      [ return Pat_any
      ; (let+ c = gen_constant in
         Pat_constant c)
      ]
    in
    (match top_level with
     | true -> oneof gen_list
     | _ ->
       oneof
         ((let+ id = gen_ident in
           Pat_var id)
          :: gen_list))
  | depth ->
    oneof
      [ (let+ p = gen_pattern (depth / 2)
         and+ t = gen_typexpr (depth / 2) in
         Pat_type (p, t))
      ; (let+ l = gen_list_helper gen_pattern depth in
         Pat_tuple l)
      ; (let* p1 = gen_pattern (depth / 2) in
         let+ p2 = gen_pattern (depth / 2) in
         Pat_or (p1, p2))
      ; gen_construct
          gen_pattern
          (fun (st, p) -> Pat_construct (st, p))
          depth
          (fun p -> Pat_tuple p)
      ]
;;

let rec gen_expr = function
  | 0 ->
    oneof
      [ (gen_ident >|= fun s -> Exp_ident s); (gen_constant >|= fun c -> Exp_constant c) ]
  | depth ->
    oneof
      [ (let* e = gen_expr (depth / 2) in
         let+ t = gen_typexpr (depth / 2) in
         Exp_type (e, t))
      ; (let* lb_list = gen_list_helper gen_let_binding depth in
         let* r = gen_rec in
         let+ e = gen_expr (depth / 2) in
         Exp_let (r, lb_list, e))
      ; (let* pl = gen_list_helper gen_pattern (depth / 2) in
         let+ e = gen_expr (depth / 2) in
         Exp_fun (pl, e))
      ; (let+ cl = gen_list_helper gen_case depth in
         Exp_function cl)
      ; (let* e1 = gen_expr (depth / 2) in
         let+ e2 = gen_expr (depth / 2) in
         Exp_apply (e1, e2))
      ; (let* e = gen_expr (depth / 2) in
         let+ cl = gen_list_helper gen_case (depth / 2) in
         Exp_match (e, cl))
      ; (let+ el = gen_list_helper gen_expr depth in
         Exp_tuple el)
      ; gen_construct
          gen_expr
          (fun (st, e) -> Exp_construct (st, e))
          depth
          (fun e -> Exp_tuple e)
      ; (let* e1 = gen_expr (depth / 3) in
         let* e2 = gen_expr (depth / 3) in
         let+ e3 = opt ~ratio:0.5 (gen_expr (depth / 3)) in
         Exp_if (e1, e2, e3))
        (* ; (let+ e1 = gen_expr (depth / 2)
           and+ e2 = gen_expr (depth / 2) in
           Exp_sequence (e1, e2)) *)
      ]

and gen_case depth =
  let* e = gen_expr (depth / 2) in
  let+ p = gen_pattern (depth / 2) in
  { left = p; right = e }

and gen_let_binding depth =
  let* e = gen_expr (depth / 2) in
  oneof
    [ (let+ p = gen_pattern ~top_level:true (depth / 2) in
       Pat_binding (p, e))
    ; (let* id = gen_ident in
       let+ pl = gen_list_helper gen_pattern (depth / 2) in
       Val_binding (id, pl, e))
    ]
;;

let gen_structure depth =
  let gen_structure_item depth =
    oneof
      [ (let+ e = gen_expr depth in
         Str_eval e)
      ; (let* r = gen_rec in
         let+ lb_list = gen_list_helper gen_let_binding depth in
         Str_value (r, lb_list))
      ]
  in
  gen_list_helper gen_structure_item depth
;;
