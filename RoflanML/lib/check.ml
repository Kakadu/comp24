open Ast

module Generator = struct
  open QCheck.Gen

  let gen_name =
    fix
      (fun self () ->
        let* nm =
          string_size ~gen:(oneof [ char_range 'a' 'z'; return '_' ]) (int_range 1 10)
        in
        if Base.String.for_all nm ~f:(fun c -> c = '_')
        then self ()
        else if Base.List.mem
                  [ "let"
                  ; "rec"
                  ; "if"
                  ; "then"
                  ; "else"
                  ; "true"
                  ; "false"
                  ; "match"
                  ; "with"
                  ; "in"
                  ; "and"
                  ; "fun"
                  ; "type"
                  ; "int"
                  ; "string"
                  ; "bool"
                  ]
                  nm
                  ~equal:String.equal
        then self ()
        else return nm)
      ()
  ;;

  let gen_const =
    frequency
      [ (1, small_int >|= fun i -> CInt i)
      ; (1, bool >|= fun b -> CBool b)
      ; 1, return CUnit
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
            let* et = gen_expr (n / 2) in
            let* ef = gen_expr (n / 2) in
            return (EBranch (cond, et, ef)) )
        ; ( 1
          , let* e = gen_expr (n / 2) in
            let* cases =
              list_size (int_range 1 3) (pair (gen_pattern (n / 2)) (gen_expr (n / 2)))
            in
            return (EMatch (e, cases)) )
        ; ( 1
          , let* rec_flag = oneof [ return Rec; return NonRec ] in
            let* id = gen_name in
            let* e_val = gen_expr (n / 2) in
            return (ELetIn (rec_flag, id, e_val, EConst (CInt 0))) )
        ; ( 1
          , let* arg = gen_typed_arg (n / 2) in
            let* body = gen_expr (n / 2) in
            return (EFun (arg, body)) )
        ; ( 1
          , let* e1 = gen_expr (n / 2) in
            let* e2 = gen_expr (n / 2) in
            return (EApp (e1, e2)) )
        ]

  and gen_typed_arg n =
    let* id = gen_name in
    frequency
      [ 1, return (id, None)
      ; ( 1
        , let* ty = gen_type n in
          return (id, Some ty) )
      ]

  and gen_type n =
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

  and gen_pattern n =
    if n <= 0
    then
      frequency
        [ (1, gen_const >|= fun c -> PConst c)
        ; ( 1
          , gen_name
            >|= fun id -> if String.length id > 0 && id.[0] = '_' then PWild else PVar id
          )
        ; 1, return PWild
        ; 1, return PEmpty
        ]
    else
      frequency
        [ (1, gen_const >|= fun c -> PConst c)
        ; ( 1
          , gen_name
            >|= fun id -> if String.length id > 0 && id.[0] = '_' then PWild else PVar id
          )
        ; 1, return PWild
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

  let gen_decl =
    frequency
      [ ( 1
        , let* rec_flag = oneof [ return Rec; return NonRec ] in
          let* id = gen_name in
          let* e = gen_expr 5 in
          return (DLet (rec_flag, id, e)) )
      ; ( 1
        , let* rec_flag = oneof [ return Rec; return NonRec ] in
          let* binds =
            list_size
              (int_range 2 4)
              (let* id = gen_name in
               let* e = gen_expr 5 in
               return (id, e))
          in
          return (DMutualLet (rec_flag, binds)) )
      ]
  ;;

  let gen_program = list_size (int_range 1 5) gen_decl
end

module Shrinker = struct
  open QCheck.Iter

  let shrink_decl = function
    | DLet (_, _, e) -> of_list [ DLet (NonRec, "_", e) ]
    | DMutualLet (_, binds) ->
      of_list (List.map (fun (id, e) -> DLet (NonRec, id, e)) binds)
  ;;

  let shrink_program prog = QCheck.Shrink.list ~shrink:shrink_decl prog
end

let arbitrary_ast =
  QCheck.make
    (QCheck.Gen.sized (fun _ -> Generator.gen_program))
    ~print:(fun prog -> Unparse.unparse_program prog)
    ~shrink:Shrinker.shrink_program
;;
