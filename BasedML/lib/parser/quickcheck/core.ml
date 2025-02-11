(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

module Generator = struct
  open QCheck.Gen

  let gen_rec_flag = frequency [ 1, return Ast.NotRec; 1, return Ast.Rec ]

  let gen_name =
    fix
      (fun self () ->
        let* nm =
          string_size ~gen:(oneof [ char_range 'a' 'z'; return '_' ]) (int_range 1 10)
        in
        if Parser.is_keyword nm then self () else return nm)
      ()
  ;;

  let rec gen_type = function
    | 0 ->
      frequency
        [ 1, return Ast.TUnit
        ; 1, return Ast.TInt
        ; 1, return Ast.TBool
        ; ( 1
          , let* nm = gen_name in
            return (Ast.TPoly nm) )
        ]
    | n ->
      frequency
        [ ( 1
          , let* len = int_range 2 5 in
            let sub_n = n / len in
            let* tp_lst = list_repeat len (gen_type sub_n) in
            return (Ast.TTuple tp_lst) )
        ; ( 1
          , let* tp1 = gen_type (n / 2) in
            let* tp2 = gen_type (n / 2) in
            return (Ast.TFunction (tp1, tp2)) )
        ; ( 1
          , let* tp1 = gen_type (n / 2) in
            return (Ast.TList tp1) )
        ]
  ;;

  let gen_constant =
    frequency
      [ (1, small_int >|= fun x -> Ast.CInt x)
      ; (1, bool >|= fun x -> Ast.CBool x)
      ; 1, return Ast.CNil
      ; 1, return Ast.CUnit
      ]
  ;;

  let rec gen_pat = function
    | 0 ->
      frequency
        [ (1, gen_constant >|= fun x -> Ast.PConstant x)
        ; 1, return Ast.PWildCard
        ; ( 1
          , let* nm = gen_name in
            return (Ast.PIdentifier nm) )
        ]
    | n ->
      frequency
        [ ( 1
          , let* p1 = gen_pat (n / 2) in
            let* p2 = gen_pat (n / 2) in
            return (Ast.PCons (p1, p2)) )
        ; ( 1
          , let* len = int_range 2 5 in
            let sub_n = n / len in
            let* p_lst = list_repeat len (gen_pat sub_n) in
            return (Ast.PTuple p_lst) )
        ; ( 1
          , let* p = gen_pat (n / 2) in
            let* t = gen_type (n / 2) in
            return (Ast.PConstraint (p, t)) )
        ]
  ;;

  let rec gen_exp = function
    | 0 ->
      frequency
        [ (1, gen_constant >|= fun x -> Ast.EConstant x)
        ; ( 1
          , let* nm = gen_name in
            return (Ast.EIdentifier nm) )
        ]
    | n ->
      frequency
        [ ( 1
          , let* p = gen_pat (n / 2) in
            let* e = gen_exp (n / 2) in
            return (Ast.EFunction (p, e)) )
        ; ( 1
          , let* e1 = gen_exp (n / 2) in
            let* e2 = gen_exp (n / 2) in
            return (Ast.EApplication (e1, e2)) )
        ; ( 1
          , let* e1 = gen_exp (n / 3) in
            let* e2 = gen_exp (n / 3) in
            let* e3 = gen_exp (n / 3) in
            return (Ast.EIfThenElse (e1, e2, e3)) )
        ; ( 1
          , let* rf = gen_rec_flag in
            let* p = gen_pat (n / 3) in
            let* he = gen_exp (n / 3) in
            let* be = gen_exp (n / 3) in
            return (Ast.ELetIn (rf, p, he, be)) )
        ; ( 1
          , let* len = int_range 2 5 in
            let sub_n = n / len in
            let* e_lst = list_repeat len (gen_exp sub_n) in
            return (Ast.ETuple e_lst) )
        ; ( 1
          , let* len = int_range 2 5 in
            let sub_n = n / ((len * 2) + 1) in
            let* hp = gen_pat sub_n in
            let* p_e_lst =
              list_repeat
                len
                (let* p = gen_pat sub_n in
                 let* e = gen_exp sub_n in
                 return (p, e))
            in
            return (Ast.EMatch (hp, p_e_lst)) )
        ; ( 1
          , let* e = gen_exp (n / 2) in
            let* t = gen_type (n / 2) in
            return (Ast.EConstraint (e, t)) )
        ]
  ;;

  let gen_slet n =
    let* pat = gen_pat (n / 2) in
    let* exp = gen_exp (n / 2) in
    return (Ast.DLet (pat, exp))
  ;;

  let gen_let_decl n =
    let* rec_flag = gen_rec_flag in
    frequency
      [ ( 1
        , let* slet = gen_slet n in
          return (Ast.DSingleLet (rec_flag, slet)) )
      ; ( 1
        , let* len = int_range 2 (max 2 n) in
          let sub_n = n / len in
          let* let_lst = list_repeat len (gen_slet sub_n) in
          return (Ast.DMutualRecDecl (rec_flag, let_lst)) )
      ]
  ;;

  let gen_decls n =
    let* len = int_range 1 10 in
    let sub_n = n / len in
    list_repeat len (gen_let_decl sub_n)
  ;;
end

module Shrinker = struct
  open QCheck.Iter

  let shrink_pat = function
    | Ast.PCons (p1, p2) -> of_list [ p1; p2 ]
    | Ast.PTuple p_lst -> of_list p_lst
    | Ast.PConstraint (p, _tp) -> return p
    | _ -> empty
  ;;

  let shrink_exp = function
    | Ast.EFunction (pat, exp) ->
      let* p = shrink_pat pat in
      of_list [ Ast.EFunction (p, exp); exp ]
    | Ast.EApplication (e1, e2) -> of_list [ e1; e2 ]
    | Ast.EIfThenElse (e1, e2, e3) -> of_list [ e1; e2; e3 ]
    | Ast.ELetIn (rf, pat, exp1, exp2) ->
      let* p = shrink_pat pat in
      of_list [ exp1; exp2; Ast.ELetIn (rf, p, exp1, exp2) ]
    | Ast.EMatch (_p, p_e_lst) -> of_list (List.map (fun (_p, e) -> e) p_e_lst)
    | Ast.EConstraint (exp, _tp) -> return exp
    | _ -> empty
  ;;

  let shrink_slet (Ast.DLet (pat, exp)) =
    let* e = shrink_exp exp in
    let* p = shrink_pat pat in
    of_list [ Ast.DLet (pat, e); Ast.DLet (p, exp) ]
  ;;

  let shrink_let_decl = function
    | Ast.DSingleLet (rf, slet) ->
      let* new_slet = shrink_slet slet in
      return (Ast.DSingleLet (rf, new_slet))
    | Ast.DMutualRecDecl (rf, let_lst) ->
      of_list (List.map (fun x -> Ast.DSingleLet (rf, x)) let_lst)
  ;;

  let shrink_decls = function
    | [] -> empty
    | [ x ] ->
      let* n = shrink_let_decl x in
      return [ n ]
    | _ as decls -> of_list (List.map (fun x -> [ x ]) decls)
  ;;
end

let arbitrary_ast =
  QCheck.make
    (QCheck.Gen.sized Generator.gen_decls)
    ~print:Restore_src.RestoreSrc.restore_declarations
    ~shrink:Shrinker.shrink_decls
;;
