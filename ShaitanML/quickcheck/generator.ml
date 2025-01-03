module Generator = struct
  open QCheck.Gen

  let gen_rec_flag =
    frequency [ 1, return Shaitanml_lib.Ast.Nonrec; 1, return Shaitanml_lib.Ast.Rec ]
  ;;

  let gen_name =
    fix
      (fun self () ->
         let* nm =
           string_size
             ~gen:(frequency [ 20, char_range 'a' 'z'; 1, char_range '_' '_' ])
             (int_range 1 10)
         in
         if Shaitanml_lib.Parser.is_keyword nm then self () else return nm)
      ()
  ;;

  let gen_typename =
    fix
      (fun self () ->
         let* first_char = char_range 'a' 'z' in
         let* suf =
           string_size
             ~gen:(frequency [ 20, char_range 'a' 'z'; 1, char_range '_' '_' ])
             (int_range 1 10)
         in
         let nm = String.make 1 first_char ^ suf in
         if Shaitanml_lib.Parser.is_keyword nm then self () else return nm)
      ()
  ;;

  let rec gen_type = function
    | 0 ->
      frequency
        [ 1, return Shaitanml_lib.Ast.AUnit
        ; 1, return Shaitanml_lib.Ast.ABool
        ; 1, return Shaitanml_lib.Ast.AInt
        ; 1, return Shaitanml_lib.Ast.AString
        (* ; ( 1
          , let* nm = gen_typename in
            return (Ast.TPoly nm) ) *)
        ]
        (* TODO check Poly type, why here needed string *)
    | n ->
      frequency
        [ ( 1
          , let* len = int_range 2 5 in
            let sub_n = n / len in
            let* tp_lst = list_repeat len (gen_type sub_n) in
            return (Shaitanml_lib.Ast.ATuple tp_lst) )
        ; ( 1
          , let* tp1 = gen_type (n / 2) in
            let* tp2 = gen_type (n / 2) in
            return (Shaitanml_lib.Ast.AFun (tp1, tp2)) )
        ; ( 1
          , let* tp1 = gen_type (n / 2) in
            return (Shaitanml_lib.Ast.AList tp1) )
        ]
  ;;
  let gen_constatnt =
    frequency
      [ (1, small_int >|= fun x -> Shaitanml_lib.Ast.CInt x)
      ; (1, bool >|= fun x -> Shaitanml_lib.Ast.CBool x)
      ; 1, return Shaitanml_lib.Ast.CNil
      ; 1, return Shaitanml_lib.Ast.CUnit
      ]
  ;;
  let rec gen_pat = function
    | 0 ->
      frequency
        [ (1, gen_constatnt >|= fun x -> Shaitanml_lib.Ast.PConst x)
        ; 1, return Shaitanml_lib.Ast.PAny
        ; ( 1
          , let* nm = gen_name in
            return (Shaitanml_lib.Ast.PVar nm) )
        ]
    | n ->
      frequency
        [ ( 1
          , let* p1 = gen_pat (n / 2) in
            let* p2 = gen_pat (n / 2) in
            return (Shaitanml_lib.Ast.PCons (p1, p2)) )
        ; ( 1
          , let* len = int_range 2 5 in
            let sub_n = n / len in
            let* p_lst = list_repeat len (gen_pat sub_n) in
            return (Shaitanml_lib.Ast.PTuple p_lst) )
        ; ( 1
          , let* p = gen_pat (n / 2) in
            let* t = gen_type (n / 2) in
            return (Shaitanml_lib.Ast.PConstraint (p, t)) )
        ]
  ;;
  
end
