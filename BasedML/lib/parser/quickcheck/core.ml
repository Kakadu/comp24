(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

module Generator = struct
  open QCheck.Gen

  let gen_rec_flag = frequency [ 1, return Ast.NotRec; 1, return Ast.Rec ]
  (* let gen_name = string_of (char_range 'a' 'z')
     let gen_type = frequency [ 1, return Ast.TUnit; 1, return Ast.TInt; 1, return Ast.TBool ] *)

  let gen_constatnt =
    frequency
      [ (1, small_int >|= fun x -> Ast.CInt x)
      ; (1, bool >|= fun x -> Ast.CBool x)
      ; 1, return Ast.CNil
      ; 1, return Ast.CUnit
      ]
  ;;

  let rec gen_pat = function
    | 0 -> frequency [ (1, gen_constatnt >|= fun x -> Ast.PConstant x) ]
    | n -> gen_pat (n - 1)
  ;;

  let rec gen_exp = function
    | 0 -> frequency [ (1, gen_constatnt >|= fun x -> Ast.EConstant x) ]
    | n -> gen_exp (n - 1)
  ;;

  let rec gen_slet = function
    | 0 ->
      frequency
        [ ( 1
          , let* pat = gen_pat 0 in
            let* exp = gen_exp 0 in
            return (Ast.DLet (pat, exp)) )
        ]
    | n -> gen_slet (n - 1)
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

  let shrink_slet (Ast.DLet (_pat, _exp)) = empty
  (* return (Ast.DLet (pat, exp)) *)

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
