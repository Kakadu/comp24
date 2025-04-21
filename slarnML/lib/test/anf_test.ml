(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Res

(*==============================*)
(*======Closure conversion======*)
(*==============================*)

open Ast
open Cc_ast
open Clos_conv
open Pprint_cc

let cc_ok n res expected =
  match res with
  | Result c_ast when c_ast = expected -> true
  | Result c_ast ->
    print_string
    @@ String.concat
         ""
         [ n
         ; ":\n"
         ; String.concat "\n" (List.map pp_cc_expr c_ast)
         ; "\n"
         ; String.concat "\n" (List.map pp_cc_expr expected)
         ; "\n====\n"
         ];
    false
  | Error e ->
    Printf.printf "%s: %s\n" n e;
    false
;;

let ast1 =
  [ Let
      ( Decl ("fac", [ "n" ])
      , LetIn
          ( DeclRec ("fack", [ "n"; "k" ])
          , If
              ( Lte (Id "n", Const (CInt 1))
              , App (Id "n", [ Sub (Id "n", Const (CInt 1)) ])
              , Fun ([ "m" ], Mul (Id "k", Mul (Id "m", Id "n"))) )
          , App (Id "fack", [ Id "n"; Fun ([ "x" ], Id "x") ]) ) )
  ]
;;

let cc1 =
  [ CLet
      ( Decl ("fac", [ "n" ])
      , CLetIn
          ( DeclRec ("fack", [ "n"; "k" ])
          , CIf
              ( CLte (CId "n", CConst (CInt 1))
              , CApp (CId "n", [ CSub (CId "n", CConst (CInt 1)) ])
              , CApp
                  ( CFun ([ "n"; "k"; "n"; "m" ], CMul (CId "k", CMul (CId "m", CId "n")))
                  , [ CId "n"; CId "k"; CId "n" ] ) )
          , CApp
              (CId "fack", [ CId "n"; CApp (CFun ([ "n"; "x" ], CId "x"), [ CId "n" ]) ])
          ) )
  ]
;;

let%test _ = cc_ok "cc_1" (clos_conv ast1) cc1

let ast2 =
  [ Let
      ( Decl ("f", [ "a" ])
      , LetIn
          ( Decl ("g", [ "c"; "d" ])
          , LetIn
              ( Decl ("h", [ "e" ])
              , Mul (Id "a", Add (Id "c", Mul (Id "d", Id "e")))
              , App (Id "h", [ Const (CInt 4) ]) )
          , App (Id "g", [ Const (CInt 2); Const (CInt 3) ]) ) )
  ]
;;

let cc2 =
  [ CLet
      ( Decl ("f", [ "a" ])
      , CLetIn
          ( Decl ("g", [ "a"; "c"; "d" ])
          , CLetIn
              ( Decl ("h", [ "c"; "d"; "a"; "e" ])
              , CMul (CId "a", CAdd (CId "c", CMul (CId "d", CId "e")))
              , CApp (CApp (CId "h", [ CId "c"; CId "d"; CId "a" ]), [ CConst (CInt 4) ])
              )
          , CApp (CApp (CId "g", [ CId "a" ]), [ CConst (CInt 2); CConst (CInt 3) ]) ) )
  ]
;;

let%test _ = cc_ok "cc_2" (clos_conv ast2) cc2

let ast3 =
  [ Let
      ( Decl ("f", [ "a"; "b" ])
      , LetIn
          ( Decl ("g", [ "c" ])
          , LetIn
              ( Decl ("h", [])
              , Fun ([ "x" ], Mul (Id "x", App (Id "a", [ Mul (Id "c", Id "b") ])))
              , App (Id "h", [ Id "a" ]) )
          , App (Id "g", [ Const (CInt 3) ]) ) )
  ]
;;

let cc3 =
  [ CLet
      ( Decl ("f", [ "a"; "b" ])
      , CLetIn
          ( Decl ("g", [ "a"; "b"; "c" ])
          , CLetIn
              ( Decl ("h", [ "c"; "a"; "b" ])
              , CApp
                  ( CFun
                      ( [ "c"; "a"; "b"; "x" ]
                      , CMul (CId "x", CApp (CId "a", [ CMul (CId "c", CId "b") ])) )
                  , [ CId "c"; CId "a"; CId "b" ] )
              , CApp (CApp (CId "h", [ CId "c"; CId "a"; CId "b" ]), [ CId "a" ]) )
          , CApp (CApp (CId "g", [ CId "a"; CId "b" ]), [ CConst (CInt 3) ]) ) )
  ]
;;

let%test _ = cc_ok "cc_3" (clos_conv ast3) cc3

let ast4 =
  [ Let
      ( Decl ("f", [ "a" ])
      , LetIn
          ( Decl ("g", [ "a"; "b" ])
          , LetIn
              ( Decl ("h", [ "b"; "c" ])
              , Mul (Id "a", Div (Id "b", Id "c"))
              , App (Id "h", [ Const (CInt 2); Const (CInt 3) ]) )
          , App (Id "g", [ Add (Const (CInt 1), Const (CInt 0)); Id "a" ]) ) )
  ]
;;

let cc4 =
  [ CLet
      ( Decl ("f", [ "a" ])
      , CLetIn
          ( Decl ("g", [ "a"; "b" ])
          , CLetIn
              ( Decl ("h", [ "a"; "a"; "b"; "c" ])
              , CMul (CId "a", CDiv (CId "b", CId "c"))
              , CApp
                  ( CApp (CId "h", [ CId "a"; CId "a" ])
                  , [ CConst (CInt 2); CConst (CInt 3) ] ) )
          , CApp (CId "g", [ CAdd (CConst (CInt 1), CConst (CInt 0)); CId "a" ]) ) )
  ]
;;

let%test _ = cc_ok "cc_4" (clos_conv ast4) cc4

let ast5 =
  [ Let
      ( Decl ("f", [ "a" ])
      , LetIn
          ( Decl ("g", [ "b" ])
          , Div (Id "a", Id "b")
          , LetIn
              ( Decl ("h", [ "c" ])
              , Mul (Id "a", Id "c")
              , Add (App (Id "h", [ Const (CInt 1) ]), App (Id "g", [ Const (CInt 2) ]))
              ) ) )
  ]
;;

let cc5 =
  [ CLet
      ( Decl ("f", [ "a" ])
      , CLetIn
          ( Decl ("g", [ "a"; "b" ])
          , CDiv (CId "a", CId "b")
          , CLetIn
              ( Decl ("h", [ "a"; "c" ])
              , CMul (CId "a", CId "c")
              , CAdd
                  ( CApp (CApp (CId "h", [ CId "a" ]), [ CConst (CInt 1) ])
                  , CApp (CApp (CId "g", [ CId "a" ]), [ CConst (CInt 2) ]) ) ) ) )
  ]
;;

let%test _ = cc_ok "cc_5" (clos_conv ast5) cc5

let ast6 =
  [ Let
      ( Decl ("f", [ "a" ])
      , LetIn
          ( Decl ("g", [])
          , Fun ([ "x" ], Id "x")
          , LetIn
              ( Decl ("h", [])
              , Fun ([ "x" ], Mul (Id "a", Id "x"))
              , Add (App (Id "g", [ Id "a" ]), App (Id "h", [ Id "a" ])) ) ) )
  ]
;;

let cc6 =
  [ CLet
      ( Decl ("f", [ "a" ])
      , CLetIn
          ( Decl ("g", [ "a" ])
          , CApp (CFun ([ "a"; "x" ], CId "x"), [ CId "a" ])
          , CLetIn
              ( Decl ("h", [ "a" ])
              , CApp (CFun ([ "a"; "x" ], CMul (CId "a", CId "x")), [ CId "a" ])
              , CAdd
                  ( CApp (CApp (CId "g", [ CId "a" ]), [ CId "a" ])
                  , CApp (CApp (CId "h", [ CId "a" ]), [ CId "a" ]) ) ) ) )
  ]
;;

let%test _ = cc_ok "cc_6" (clos_conv ast6) cc6

(*==========================*)
(*======Lambda lifting======*)
(*==========================*)
open Ll_ast
open Lambda_lifting
open Pprint_ll

let ll_ok n res expected =
  match res with
  | Result l_ast when l_ast = expected -> true
  | Result l_ast ->
    print_string
    @@ String.concat
         ""
         [ n
         ; ":\n"
         ; String.concat "\n" (List.map pp_gl_expr l_ast)
         ; "\n---\n"
         ; String.concat "\n" (List.map pp_gl_expr expected)
         ; "\n====\n"
         ];
    false
  | Error e ->
    Printf.printf "%s: %s\n" n e;
    false
;;

let ll1 =
  [ LFun ("anon_1", [ "n"; "k"; "n"; "m" ], LMul (LId "k", LMul (LId "m", LId "n")))
  ; LFun
      ( "fack"
      , [ "n"; "k" ]
      , LIf
          ( LLte (LId "n", LConst (CInt 1))
          , LApp (LId "n", [ LSub (LId "n", LConst (CInt 1)) ])
          , LApp (LApp (LApp (LId "anon_1", [ LId "n" ]), [ LId "k" ]), [ LId "n" ]) ) )
  ; LFun ("anon_2", [ "n"; "x" ], LId "x")
  ; LFun
      ( "fac"
      , [ "n" ]
      , LIn
          ( "fack"
          , LIf
              ( LLte (LId "n", LConst (CInt 1))
              , LApp (LId "n", [ LSub (LId "n", LConst (CInt 1)) ])
              , LApp (LApp (LApp (LId "anon_1", [ LId "n" ]), [ LId "k" ]), [ LId "n" ])
              )
          , LApp (LApp (LId "fack", [ LId "n" ]), [ LApp (LId "anon_2", [ LId "n" ]) ]) )
      )
  ]
;;

ll_ok "ll_1" (lambda_lifting cc1) ll1

let ll2 =
  [ LFun
      ( "h"
      , [ "c"; "d"; "a"; "e" ]
      , LMul (LId "a", LAdd (LId "c", LMul (LId "d", LId "e"))) )
  ; LFun
      ( "g"
      , [ "a"; "c"; "d" ]
      , LIn
          ( "h"
          , LMul (LId "a", LAdd (LId "c", LMul (LId "d", LId "e")))
          , LApp
              ( LApp (LApp (LApp (LId "h", [ LId "c" ]), [ LId "d" ]), [ LId "a" ])
              , [ LConst (CInt 4) ] ) ) )
  ; LFun
      ( "f"
      , [ "a" ]
      , LIn
          ( "g"
          , LIn
              ( "h"
              , LMul (LId "a", LAdd (LId "c", LMul (LId "d", LId "e")))
              , LApp
                  ( LApp (LApp (LApp (LId "h", [ LId "c" ]), [ LId "d" ]), [ LId "a" ])
                  , [ LConst (CInt 4) ] ) )
          , LApp
              ( LApp (LApp (LId "g", [ LId "a" ]), [ LConst (CInt 2) ])
              , [ LConst (CInt 3) ] ) ) )
  ]
;;

ll_ok "ll_2" (lambda_lifting cc2) ll2

let ll3 =
  [ LFun
      ( "anon_1"
      , [ "c"; "a"; "b"; "x" ]
      , LMul (LId "x", LApp (LId "a", [ LMul (LId "c", LId "b") ])) )
  ; LFun
      ( "h"
      , [ "c"; "a"; "b" ]
      , LApp (LApp (LApp (LId "anon_1", [ LId "c" ]), [ LId "a" ]), [ LId "b" ]) )
  ; LFun
      ( "g"
      , [ "a"; "b"; "c" ]
      , LIn
          ( "h"
          , LApp (LApp (LApp (LId "anon_1", [ LId "c" ]), [ LId "a" ]), [ LId "b" ])
          , LApp
              ( LApp (LApp (LApp (LId "h", [ LId "c" ]), [ LId "a" ]), [ LId "b" ])
              , [ LId "a" ] ) ) )
  ; LFun
      ( "f"
      , [ "a"; "b" ]
      , LIn
          ( "g"
          , LIn
              ( "h"
              , LApp (LApp (LApp (LId "anon_1", [ LId "c" ]), [ LId "a" ]), [ LId "b" ])
              , LApp
                  ( LApp (LApp (LApp (LId "h", [ LId "c" ]), [ LId "a" ]), [ LId "b" ])
                  , [ LId "a" ] ) )
          , LApp (LApp (LApp (LId "g", [ LId "a" ]), [ LId "b" ]), [ LConst (CInt 3) ]) )
      )
  ]
;;

ll_ok "ll_3" (lambda_lifting cc3) ll3

let ll4 =
  [ LFun ("h", [ "a"; "a"; "b"; "c" ], LMul (LId "a", LDiv (LId "b", LId "c")))
  ; LFun
      ( "g"
      , [ "a"; "b" ]
      , LIn
          ( "h"
          , LMul (LId "a", LDiv (LId "b", LId "c"))
          , LApp
              ( LApp (LApp (LApp (LId "h", [ LId "a" ]), [ LId "a" ]), [ LConst (CInt 2) ])
              , [ LConst (CInt 3) ] ) ) )
  ; LFun
      ( "f"
      , [ "a" ]
      , LIn
          ( "g"
          , LIn
              ( "h"
              , LMul (LId "a", LDiv (LId "b", LId "c"))
              , LApp
                  ( LApp
                      ( LApp (LApp (LId "h", [ LId "a" ]), [ LId "a" ])
                      , [ LConst (CInt 2) ] )
                  , [ LConst (CInt 3) ] ) )
          , LApp (LApp (LId "g", [ LAdd (LConst (CInt 1), LConst (CInt 0)) ]), [ LId "a" ])
          ) )
  ]
;;

ll_ok "ll_4" (lambda_lifting cc4) ll4

let ll5 =
  [ LFun ("g", [ "a"; "b" ], LDiv (LId "a", LId "b"))
  ; LFun ("h", [ "a"; "c" ], LMul (LId "a", LId "c"))
  ; LFun
      ( "f"
      , [ "a" ]
      , LIn
          ( "g"
          , LDiv (LId "a", LId "b")
          , LIn
              ( "h"
              , LMul (LId "a", LId "c")
              , LAdd
                  ( LApp (LApp (LId "h", [ LId "a" ]), [ LConst (CInt 1) ])
                  , LApp (LApp (LId "g", [ LId "a" ]), [ LConst (CInt 2) ]) ) ) ) )
  ]
;;

ll_ok "ll_5" (lambda_lifting cc5) ll5

let ll6 =
  [ LFun ("anon_1", [ "a"; "x" ], LId "x")
  ; LFun ("g", [ "a" ], LApp (LId "anon_1", [ LId "a" ]))
  ; LFun ("anon_2", [ "a"; "x" ], LMul (LId "a", LId "x"))
  ; LFun ("h", [ "a" ], LApp (LId "anon_2", [ LId "a" ]))
  ; LFun
      ( "f"
      , [ "a" ]
      , LIn
          ( "g"
          , LApp (LId "anon_1", [ LId "a" ])
          , LIn
              ( "h"
              , LApp (LId "anon_2", [ LId "a" ])
              , LAdd
                  ( LApp (LApp (LId "g", [ LId "a" ]), [ LId "a" ])
                  , LApp (LApp (LId "h", [ LId "a" ]), [ LId "a" ]) ) ) ) )
  ]
;;

ll_ok "ll_6" (lambda_lifting cc6) ll6

(*=========================*)
(*===========ANF===========*)
(*=========================*)
open Anf_ast
open Anf_conv
open Pprint_anf

let anf_ok n ll expected =
  let _ = clear_free () in
  match anf ll with
  | l_ast when l_ast = expected -> true
  | l_ast ->
    print_string
    @@ String.concat
         ""
         [ n
         ; ":\n"
         ; String.concat "\n" (List.map pp_anf_afun l_ast)
         ; "\n---\n"
         ; String.concat "\n" (List.map pp_anf_afun expected)
         ; "\n====\n"
         ];
    false
;;

let anf1 =
  [ AFun
      ( "anon_1"
      , [ "n"; "k"; "n"; "m" ]
      , ALet
          ( "anf_op_1"
          , AMul (AId "m", AId "n")
          , ALet
              ( "anf_op_2"
              , AMul (AId "k", AId "anf_op_1")
              , ACExpr (CImmExpr (AId "anf_op_2")) ) ) )
  ; AFun
      ( "fack"
      , [ "n"; "k" ]
      , ALet
          ( "anf_op_3"
          , ALte (AId "n", AInt 1)
          , ALet
              ( "anf_if_4"
              , AIf
                  ( AId "anf_op_3"
                  , ALet
                      ( "anf_op_5"
                      , ASub (AId "n", AInt 1)
                      , ALet
                          ( "anf_app_6"
                          , AApp (AId "n", [ AId "anf_op_5" ])
                          , ACExpr (CImmExpr (AId "anf_app_6")) ) )
                  , ALet
                      ( "anf_app_7"
                      , AApp (AId "anon_1", [ AId "n" ])
                      , ALet
                          ( "anf_app_8"
                          , AApp (AId "anf_app_7", [ AId "k" ])
                          , ALet
                              ( "anf_app_9"
                              , AApp (AId "anf_app_8", [ AId "n" ])
                              , ACExpr (CImmExpr (AId "anf_app_9")) ) ) ) )
              , ACExpr (CImmExpr (AId "anf_if_4")) ) ) )
  ; AFun ("anon_2", [ "n"; "x" ], ACExpr (CImmExpr (AId "x")))
  ; AFun
      ( "fac"
      , [ "n" ]
      , ALet
          ( "anf_op_10"
          , ALte (AId "n", AInt 1)
          , ALet
              ( "anf_if_11"
              , AIf
                  ( AId "anf_op_10"
                  , ALet
                      ( "anf_op_12"
                      , ASub (AId "n", AInt 1)
                      , ALet
                          ( "anf_app_13"
                          , AApp (AId "n", [ AId "anf_op_12" ])
                          , ACExpr (CImmExpr (AId "anf_app_13")) ) )
                  , ALet
                      ( "anf_app_14"
                      , AApp (AId "anon_1", [ AId "n" ])
                      , ALet
                          ( "anf_app_15"
                          , AApp (AId "anf_app_14", [ AId "k" ])
                          , ALet
                              ( "anf_app_16"
                              , AApp (AId "anf_app_15", [ AId "n" ])
                              , ACExpr (CImmExpr (AId "anf_app_16")) ) ) ) )
              , ALet
                  ( "fack"
                  , CImmExpr (AId "anf_if_11")
                  , ALet
                      ( "anf_app_17"
                      , AApp (AId "fack", [ AId "n" ])
                      , ALet
                          ( "anf_app_18"
                          , AApp (AId "anon_2", [ AId "n" ])
                          , ALet
                              ( "anf_app_19"
                              , AApp (AId "anf_app_17", [ AId "anf_app_18" ])
                              , ACExpr (CImmExpr (AId "anf_app_19")) ) ) ) ) ) ) )
  ]
;;

let%test _ = anf_ok "anf_1" ll1 anf1

let anf4 =
  [ AFun
      ( "h"
      , [ "a"; "a"; "b"; "c" ]
      , ALet
          ( "anf_op_1"
          , ADiv (AId "b", AId "c")
          , ALet
              ( "anf_op_2"
              , AMul (AId "a", AId "anf_op_1")
              , ACExpr (CImmExpr (AId "anf_op_2")) ) ) )
  ; AFun
      ( "g"
      , [ "a"; "b" ]
      , ALet
          ( "anf_op_3"
          , ADiv (AId "b", AId "c")
          , ALet
              ( "anf_op_4"
              , AMul (AId "a", AId "anf_op_3")
              , ALet
                  ( "h"
                  , CImmExpr (AId "anf_op_4")
                  , ALet
                      ( "anf_app_5"
                      , AApp (AId "h", [ AId "a" ])
                      , ALet
                          ( "anf_app_6"
                          , AApp (AId "anf_app_5", [ AId "a" ])
                          , ALet
                              ( "anf_app_7"
                              , AApp (AId "anf_app_6", [ AInt 2 ])
                              , ALet
                                  ( "anf_app_8"
                                  , AApp (AId "anf_app_7", [ AInt 3 ])
                                  , ACExpr (CImmExpr (AId "anf_app_8")) ) ) ) ) ) ) ) )
  ; AFun
      ( "f"
      , [ "a" ]
      , ALet
          ( "anf_op_9"
          , ADiv (AId "b", AId "c")
          , ALet
              ( "anf_op_10"
              , AMul (AId "a", AId "anf_op_9")
              , ALet
                  ( "h"
                  , CImmExpr (AId "anf_op_10")
                  , ALet
                      ( "anf_app_11"
                      , AApp (AId "h", [ AId "a" ])
                      , ALet
                          ( "anf_app_12"
                          , AApp (AId "anf_app_11", [ AId "a" ])
                          , ALet
                              ( "anf_app_13"
                              , AApp (AId "anf_app_12", [ AInt 2 ])
                              , ALet
                                  ( "anf_app_14"
                                  , AApp (AId "anf_app_13", [ AInt 3 ])
                                  , ALet
                                      ( "g"
                                      , CImmExpr (AId "anf_app_14")
                                      , ALet
                                          ( "anf_op_15"
                                          , AAdd (AInt 1, AInt 0)
                                          , ALet
                                              ( "anf_app_16"
                                              , AApp (AId "g", [ AId "anf_op_15" ])
                                              , ALet
                                                  ( "anf_app_17"
                                                  , AApp (AId "anf_app_16", [ AId "a" ])
                                                  , ACExpr (CImmExpr (AId "anf_app_17"))
                                                  ) ) ) ) ) ) ) ) ) ) ) )
  ]
;;

let%test _ = anf_ok "anf_4" ll4 anf4

let anf5 =
  [ AFun
      ( "g"
      , [ "a"; "b" ]
      , ALet ("anf_op_1", ADiv (AId "a", AId "b"), ACExpr (CImmExpr (AId "anf_op_1"))) )
  ; AFun
      ( "h"
      , [ "a"; "c" ]
      , ALet ("anf_op_2", AMul (AId "a", AId "c"), ACExpr (CImmExpr (AId "anf_op_2"))) )
  ; AFun
      ( "f"
      , [ "a" ]
      , ALet
          ( "anf_op_3"
          , ADiv (AId "a", AId "b")
          , ALet
              ( "g"
              , CImmExpr (AId "anf_op_3")
              , ALet
                  ( "anf_op_4"
                  , AMul (AId "a", AId "c")
                  , ALet
                      ( "h"
                      , CImmExpr (AId "anf_op_4")
                      , ALet
                          ( "anf_app_5"
                          , AApp (AId "h", [ AId "a" ])
                          , ALet
                              ( "anf_app_6"
                              , AApp (AId "anf_app_5", [ AInt 1 ])
                              , ALet
                                  ( "anf_app_7"
                                  , AApp (AId "g", [ AId "a" ])
                                  , ALet
                                      ( "anf_app_8"
                                      , AApp (AId "anf_app_7", [ AInt 2 ])
                                      , ALet
                                          ( "anf_op_9"
                                          , AAdd (AId "anf_app_6", AId "anf_app_8")
                                          , ACExpr (CImmExpr (AId "anf_op_9")) ) ) ) ) )
                      ) ) ) ) )
  ]
;;

let%test _ = anf_ok "anf_5" ll5 anf5

let anf6 =
  [ AFun ("anon_1", [ "a"; "x" ], ACExpr (CImmExpr (AId "x")))
  ; AFun
      ( "g"
      , [ "a" ]
      , ALet
          ( "anf_app_1"
          , AApp (AId "anon_1", [ AId "a" ])
          , ACExpr (CImmExpr (AId "anf_app_1")) ) )
  ; AFun
      ( "anon_2"
      , [ "a"; "x" ]
      , ALet ("anf_op_2", AMul (AId "a", AId "x"), ACExpr (CImmExpr (AId "anf_op_2"))) )
  ; AFun
      ( "h"
      , [ "a" ]
      , ALet
          ( "anf_app_3"
          , AApp (AId "anon_2", [ AId "a" ])
          , ACExpr (CImmExpr (AId "anf_app_3")) ) )
  ; AFun
      ( "f"
      , [ "a" ]
      , ALet
          ( "anf_app_4"
          , AApp (AId "anon_1", [ AId "a" ])
          , ALet
              ( "g"
              , CImmExpr (AId "anf_app_4")
              , ALet
                  ( "anf_app_5"
                  , AApp (AId "anon_2", [ AId "a" ])
                  , ALet
                      ( "h"
                      , CImmExpr (AId "anf_app_5")
                      , ALet
                          ( "anf_app_6"
                          , AApp (AId "g", [ AId "a" ])
                          , ALet
                              ( "anf_app_7"
                              , AApp (AId "anf_app_6", [ AId "a" ])
                              , ALet
                                  ( "anf_app_8"
                                  , AApp (AId "h", [ AId "a" ])
                                  , ALet
                                      ( "anf_app_9"
                                      , AApp (AId "anf_app_8", [ AId "a" ])
                                      , ALet
                                          ( "anf_op_10"
                                          , AAdd (AId "anf_app_7", AId "anf_app_9")
                                          , ACExpr (CImmExpr (AId "anf_op_10")) ) ) ) ) )
                      ) ) ) ) )
  ]
;;

let%test _ = anf_ok "anf_6" ll6 anf6
