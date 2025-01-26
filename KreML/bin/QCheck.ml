(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Kreml_lib.Ast
open Kreml_lib.Parser
open QCheck.Gen

let match_cases_limit = 4
let tuple_elems_limit = 4
let mutual_rec_decls_limit = 2
let structure_decls_limit = 3

let ident =
  let helper =
    fix (fun self _ ->
      let letter_or_underscore =
        frequency [ 1, char_range '_' '_'; 1, char_range 'A' 'Z'; 8, char_range 'a' 'z' ]
      in
      let* head = char_range 'a' 'z' in
      let* tail = string_size ~gen:letter_or_underscore (int_range 1 15) in
      let id = Base.String.of_char_list [ head ] ^ tail in
      if is_keyword id then self () else return id)
  in
  helper ()
;;

let const =
  frequency
    [ 4, map (fun i -> Const_int i) nat
    ; 1, map (fun b -> Const_bool b) bool
    ; 1, return Const_unit
    ; 1, return Const_nil
    ]
;;

let tuple e =
  let* fst = e in
  let* snd = e in
  let range = int_range 0 (tuple_elems_limit - 2) in
  let* rest = list_size range e in
  (fst, snd, rest) |> return
;;

let typ =
  sized
  @@ fix (fun self n ->
    let s = self @@ (n / 64) in
    match n with
    | 0 ->
      frequency
        [ 1, return Typ_bool
        ; 1, return Typ_int
        ; 1, return Typ_unit (* 2, map (fun id -> Typ_var id) small_nat *)
        ]
    | _ ->
      frequency
        [ 1, map (fun t -> Typ_list t) s
        ; 1, map (fun (fst, snd, rest) -> Typ_tuple (fst, snd, rest)) (tuple s)
        ; 1, map2 (fun arg body -> Typ_fun (arg, body)) s s
        ])
;;

let rec_flag = frequency [ 1, return Recursive; 1, return NonRecursive ]

let pattern =
  sized
  @@ fix (fun self n ->
    let s = self @@ (n / 64) in
    match n with
    | 0 ->
      frequency
        [ 5, map (fun id -> Pat_var id) ident
        ; 1, return Pat_wildcard
        ; 2, map (fun c -> Pat_const c) const
        ]
    | _ ->
      frequency
        [ 2, map (fun (fst, snd, rest) -> Pat_tuple (fst, snd, rest)) (tuple s)
        ; 2, map2 (fun x xs -> Pat_cons (x, xs)) s s
        ; 1, map2 (fun p t -> Pat_constrained (p, t)) s typ
        ])
;;

let expr =
  sized
  @@ fix (fun self n ->
    let s = self @@ (n / 64) in
    match n with
    | 0 ->
      frequency
        [ 2, map (fun c -> Expr_const c) const; 2, map (fun id -> Expr_var id) ident ]
    | _ ->
      frequency
        [ 1, map2 (fun x xs -> Expr_cons (x, xs)) s s
        ; 1, map (fun (fst, snd, rest) -> Expr_tuple (fst, snd, rest)) (tuple s)
        ; ( 3
          , (fun rf pat value scope -> Expr_let (rf, (pat, value), scope))
            <$> rec_flag
            <*> pattern
            <*> s
            <*> s )
        ; 2, map2 (fun p body -> Expr_fun (p, body)) pattern s
        ; ( 1
          , map2
              (fun e cases -> Expr_match (e, cases))
              s
              (list_size (int_range 1 match_cases_limit) (pair pattern s)) )
        ; 1, map3 (fun c t e -> Expr_ite (c, t, e)) s s s
        ; 1, map2 (fun x xs -> Expr_app (x, xs)) s s
        ; 1, map2 (fun e t -> Expr_constrained (e, t)) s typ
        ])
;;

let structure_item =
  let* rf = rec_flag in
  match rf with
  | NonRecursive ->
    let* p = pattern in
    let* e = expr in
    Str_value (rf, [ p, e ]) |> return
  | Recursive ->
    let count = int_range 1 mutual_rec_decls_limit in
    let* decls = list_size count (pair pattern expr) in
    Str_value (rf, decls) |> return
;;

let structure = list_size (int_range 1 structure_decls_limit) structure_item

let rand =
  match Array.length Sys.argv with
  | 0 -> Random.State.make [| Sys.argv.(0) |> int_of_string |]
  | _ -> Random.State.make_self_init ()
;;

let check_ast_component ~parser ~to_code ~ast_printer expected =
  let code = to_code expected in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser code with
  | Ok actual when expected = actual -> true
  | Ok actual ->
    print_endline "parsing succeeded";
    (* let open Stdlib.Format in *)
    print_endline (ast_printer expected);
    print_endline (ast_printer actual);
    false
  | Error _ ->
    print_endline code;
    (* print_endline (ast_printer expected); *)
    false
;;

let arbitrary_pattern =
  let open QCheck.Iter in
  let rec shrink = function
    | Pat_wildcard | Pat_const _ | Pat_var _ -> empty
    | Pat_cons (x, xs) ->
      of_list [ x; xs ]
      <+> (shrink x >|= fun x' -> Pat_cons (x', xs))
      <+> (shrink xs >|= fun xs' -> Pat_cons (x, xs'))
      (* Todo rest *)
    | Pat_tuple (fst, snd, rest) ->
      of_list (fst :: snd :: rest)
      <+> (shrink fst >|= fun fst' -> Pat_tuple (fst', snd, rest))
      <+> (shrink snd >|= fun snd' -> Pat_tuple (fst, snd', rest))
    | Pat_constrained (p, _) -> shrink p
  in
  let print = show_pattern in
  QCheck.make ~print ~shrink pattern
;;

let check_pattern =
  let property =
    check_ast_component
      ~parser:Kreml_lib.Parser.pattern
      ~to_code:Kreml_lib.Ast_printer.pattern_to_code
      ~ast_printer:show_pattern
  in
  QCheck.Test.make
    arbitrary_pattern
    property
    ~count:100
    ~name:"pattern = parse @@ to_code @@ pattern"
;;

QCheck_runner.run_tests ~rand [ check_pattern ]

let arbitrary_expr =
  let open QCheck.Iter in
  let rec shrink = function
    | Expr_const _ | Expr_var _ -> empty
    | Expr_cons (x, xs) ->
      of_list [ x; xs ]
      <+> (shrink x >|= fun x' -> Expr_cons (x', xs))
      <+> (shrink xs >|= fun xs' -> Expr_cons (x, xs'))
    | Expr_tuple (fst, snd, rest) -> of_list (fst :: snd :: rest)
    | Expr_let (_, (_, e), scope) -> of_list [ e; scope ]
    | Expr_ite (c, t, e) -> of_list [ c; t; e ]
    | Expr_fun (_, e) -> return e
    | Expr_match (e, cases) -> Base.List.cons e (List.map snd cases) |> of_list
    | Expr_app (f, arg) ->
      of_list [ f; arg ]
      <+> (shrink f >|= fun f' -> Expr_app (f', arg))
      <+> (shrink arg >|= fun arg' -> Expr_app (f, arg'))
    | Expr_constrained (e, _) -> return e
  in
  let print = Kreml_lib.Ast_printer.expr_to_code in
  QCheck.make ~shrink ~print expr
;;

let check_expr =
  let property =
    check_ast_component
      ~parser:Kreml_lib.Parser.expr
      ~to_code:Kreml_lib.Ast_printer.expr_to_code
      ~ast_printer:show_expr
  in
  QCheck.Test.make
    arbitrary_expr
    property
    ~count:100
    ~name:"expr = parse @@ to code @@ expr"
;;

QCheck_runner.run_tests ~rand [ check_expr ]

let arbitary_structure =
  let print = Kreml_lib.Ast_printer.structure_to_code in
  QCheck.make ~print structure
;;

let check_structure =
  let property =
    check_ast_component
      ~parser:Kreml_lib.Parser.program
      ~to_code:Kreml_lib.Ast_printer.structure_to_code
      ~ast_printer:show_structure
  in
  QCheck.Test.make
    arbitary_structure
    property
    ~count:10
    ~name:"structure = parse @@ pp @@ structure"
;;

QCheck_runner.run_tests ~rand [ check_structure ]
