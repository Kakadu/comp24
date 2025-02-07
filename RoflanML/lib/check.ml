(** Copyright 2025, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open QCheck

open Ast

module Generator = struct
  open Gen

  let is_keyword s =
    match s with
    | "let"
    | "rec"
    | "if"
    | "then"
    | "else"
    | "true"
    | "false"
    | "match"
    | "with"
    | "in"
    | "fun"
    | "type"
    | "int"
    | "string"
    | "bool" -> true
    | _ -> false
  ;;

  let gen_name =
    fix
      (fun self () ->
        let* nm =
          string_size ~gen:(oneof [ char_range 'a' 'z'; return '_' ]) (int_range 1 10)
        in
        if is_keyword nm then self () else return nm)
      ()
  ;;

  let gen_const =
    frequency
      [ (1, small_int >|= fun i -> CInt i)
      ; (1, bool >|= fun b -> CBool b)
      ; 1, return CUnit
      ]
  ;;

  let rec gen_pattern n =
    if n <= 0
    then
      frequency
        [ (1, gen_const >|= fun c -> PConst c)
        ; ( 1
          , gen_name
            >|= fun id ->
            if String.length id > 0 && id.[0] = '_' then PVar "_" else PVar id )
        ; 1, return (PVar "_")
        ; 1, return PEmpty
        ]
    else
      frequency
        [ (1, gen_const >|= fun c -> PConst c)
        ; ( 1
          , gen_name
            >|= fun id ->
            if String.length id > 0 && id.[0] = '_' then PVar "_" else PVar id )
        ; 1, return (PVar "_")
        ; 1, return PEmpty
        ; ( 1
          , let* p1 = gen_pattern (n / 2) in
            let* p2 = gen_pattern (n / 2) in
            let* ps = list_size (int_range 0 3) (gen_pattern (n / 2)) in
            return (PCons (p1, p2, ps)) )
        ; ( 1
          , let* p1 = gen_pattern (n / 2) in
            let* p2 = gen_pattern (n / 2) in
            let* ps = list_size (int_range 0 3) (gen_pattern (n / 2)) in
            return (POr (p1, p2, ps)) )
        ]
  ;;

  let rec gen_type n =
    if n <= 0
    then frequency [ 1, return TInt; 1, return TBool; 1, return TUnit ]
    else
      frequency
        [ 1, return TInt
        ; 1, return TBool
        ; 1, return TUnit
        ; ( 1
          , let* t1 = gen_type (n / 2) in
            let* t2 = gen_type (n / 2) in
            return (TFun (t1, t2)) )
        ; ( 1
          , let* t = gen_type (n / 2) in
            return (TList t) )
        ; ( 1
          , let* t1 = gen_type (n / 3) in
            let* t2 = gen_type (n / 3) in
            let* ts = list_size (int_range 0 3) (gen_type (n / 3)) in
            return (TTuple (t1, t2, ts)) )
        ]
  ;;

  let gen_typed_arg n =
    let* id = gen_name in
    frequency
      [ 1, return (id, None)
      ; ( 1
        , let* ty = gen_type n in
          return (id, Some ty) )
      ]
  ;;

  let rec gen_expr n =
    if n <= 0
    then
      frequency
        [ (1, gen_const >|= fun c -> EConst c); (1, gen_name >|= fun id -> EVar id) ]
    else
      frequency
        [ (1, gen_const >|= fun c -> EConst c)
        ; (1, gen_name >|= fun id -> EVar id)
        ; ( 1
          , let* lst = list_size (int_range 0 3) (gen_expr (n / 2)) in
            return (EList lst) )
        ; ( 1
          , let* e1 = gen_expr (n / 2) in
            let* e2 = gen_expr (n / 2) in
            let* es = list_size (int_range 0 3) (gen_expr (n / 2)) in
            return (ETuple (e1, e2, es)) )
        ; ( 1
          , let* cond = gen_expr (n / 2) in
            let* e_then = gen_expr (n / 2) in
            let* e_else = gen_expr (n / 2) in
            return (EBranch (cond, e_then, e_else)) )
        ; ( 1
          , let* e = gen_expr (n / 2) in
            let* cases =
              list_size (int_range 1 3) (pair (gen_pattern (n / 2)) (gen_expr (n / 2)))
            in
            return (EMatch (e, cases)) )
        ; ( 1
          , let* rec_flag = frequency [ 1, return Rec; 1, return NonRec ] in
            let* id = gen_name in
            let* e_val = gen_expr (n / 2) in
            let* e_body = option (gen_expr (n / 2)) in
            return (ELet (rec_flag, id, e_val, e_body)) )
        ; ( 1
          , let* arg = gen_typed_arg (n / 2) in
            let* body = gen_expr (n / 2) in
            return (EFun (arg, body)) )
        ; ( 1
          , let* e1 = gen_expr (n / 2) in
            let* e2 = gen_expr (n / 2) in
            return (EApp (e1, e2)) )
        ]
  ;;

  (* Generating a list of expressions program *)
  let gen_program n = list_size (int_range 1 10) (gen_expr n)
end

module Shrinker = struct
  open QCheck.Iter

  let shrink_pat = function
    | PCons (p1, p2, ps) -> of_list (p1 :: p2 :: ps)
    | POr (p1, p2, ps) -> of_list (p1 :: p2 :: ps)
    | _ -> empty
  ;;

  let shrink_exp = function
    | EBranch (cond, et, ef) -> of_list [ cond; et; ef ]
    | EMatch (e, cases) -> of_list (e :: List.map snd cases)
    | ELet (_, _id, e1, Some e2) -> of_list [ e1; e2 ]
    | ELet (_, _id, e1, None) -> of_list [ e1 ]
    | EFun (_arg, e) -> of_list [ e ]
    | EApp (e1, e2) -> of_list [ e1; e2 ]
    | ETuple (e1, e2, es) -> of_list (e1 :: e2 :: es)
    | EList es -> of_list es
    | _ -> empty
  ;;

  let shrink_program prog = QCheck.Shrink.list ~shrink:shrink_exp prog
end

let arbitrary_ast =
  QCheck.make
    (QCheck.Gen.sized Generator.gen_program)
    ~print:(fun prog -> Unparse.unparse_program prog)
    ~shrink:Shrinker.shrink_program
;;
