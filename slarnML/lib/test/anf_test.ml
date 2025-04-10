(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(* open Res *)

(*==============================*)
(*======Closure conversion======*)
(*==============================*)
(* open Ast
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
              , CApp (CApp (CId "n", []), [ CSub (CId "n", CConst (CInt 1)) ])
              , CApp
                  ( CFun ([ "k"; "n"; "m" ], CMul (CId "k", CMul (CId "m", CId "n")))
                  , [ CId "k"; CId "n" ] ) )
          , CApp (CApp (CId "fack", []), [ CId "n"; CApp (CFun ([ "x" ], CId "x"), []) ])
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
              ( Decl ("h", [ "a"; "c"; "d"; "e" ])
              , CMul (CId "a", CAdd (CId "c", CMul (CId "d", CId "e")))
              , CApp (CApp (CId "h", [ CId "a"; CId "c"; CId "d" ]), [ CConst (CInt 4) ])
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
          ( Decl ("g", [ "b"; "a"; "c" ])
          , CLetIn
              ( Decl ("h", [ "c"; "b"; "a" ])
              , CApp
                  ( CFun
                      ( [ "c"; "b"; "a"; "x" ]
                      , CMul
                          (CId "x", CApp (CApp (CId "a", []), [ CMul (CId "c", CId "b") ]))
                      )
                  , [ CId "c"; CId "b"; CId "a" ] )
              , CApp (CApp (CId "h", [ CId "c"; CId "b"; CId "a" ]), [ CId "a" ]) )
          , CApp (CApp (CId "g", [ CId "b"; CId "a" ]), [ CConst (CInt 3) ]) ) )
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
              ( Decl ("h", [ "a"; "b"; "c" ])
              , CMul (CId "a", CDiv (CId "b", CId "c"))
              , CApp (CApp (CId "h", [ CId "a" ]), [ CConst (CInt 2); CConst (CInt 3) ])
              )
          , CApp (CApp (CId "g", []), [ CAdd (CConst (CInt 1), CConst (CInt 0)); CId "a" ])
          ) )
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
          ( Decl ("g", [])
          , CApp (CFun ([ "x" ], CId "x"), [])
          , CLetIn
              ( Decl ("h", [ "a" ])
              , CApp (CFun ([ "a"; "x" ], CMul (CId "a", CId "x")), [ CId "a" ])
              , CAdd
                  ( CApp (CApp (CId "g", []), [ CId "a" ])
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
  [ LFun ("anon$1", [ "k"; "n"; "m" ], LMul (LId "k", LMul (LId "m", LId "n")))
  ; LFun
      ( "fack"
      , [ "n"; "k" ]
      , LIf
          ( LLte (LId "n", LConst (CInt 1))
          , LApp ("n", [ LSub (LId "n", LConst (CInt 1)) ])
          , LApp ("anon$1", [ LApp ("k", []); LApp ("n", []) ]) ) )
  ; LFun ("anon$2", [ "x" ], LId "x")
  ; LFun ("fac", [ "n" ], LApp ("fack", [ LApp ("n", []); LApp ("anon$2", []) ]))
  ]
;;

ll_ok "ll_1" (lambda_lifting cc1) ll1

let ll2 =
  [ LFun
      ( "h"
      , [ "a"; "c"; "d"; "e" ]
      , LMul (LApp ("a", []), LAdd (LApp ("c", []), LMul (LApp ("d", []), LApp ("e", []))))
      )
  ; LFun
      ( "g"
      , [ "a"; "c"; "d" ]
      , LApp ("h", [ LApp ("a", []); LApp ("c", []); LApp ("d", []); LConst (CInt 4) ]) )
  ; LFun ("f", [ "a" ], LApp ("g", [ LApp ("a", []); LConst (CInt 2); LConst (CInt 3) ]))
  ]
;;

ll_ok "ll_2" (lambda_lifting cc2) ll2

let ll3 =
  [ LFun
      ( "anon$1"
      , [ "c"; "b"; "a"; "x" ]
      , LMul (LId "x", LApp ("a", [ LMul (LId "c", LId "b") ])) )
  ; LFun
      ( "h"
      , [ "c"; "b"; "a" ]
      , LApp ("anon$1", [ LApp ("c", []); LApp ("b", []); LApp ("a", []) ]) )
  ; LFun
      ( "g"
      , [ "b"; "a"; "c" ]
      , LApp ("h", [ LApp ("c", []); LApp ("b", []); LApp ("a", []); LApp ("a", []) ]) )
  ; LFun
      ("f", [ "a"; "b" ], LApp ("g", [ LApp ("b", []); LApp ("a", []); LConst (CInt 3) ]))
  ]
;;

ll_ok "ll_3" (lambda_lifting cc3) ll3

let ll4 =
  [ LFun
      ( "h"
      , [ "a"; "b"; "c" ]
      , LMul (LApp ("a", []), LDiv (LApp ("b", []), LApp ("c", []))) )
  ; LFun
      ("g", [ "a"; "b" ], LApp ("h", [ LApp ("a", []); LConst (CInt 2); LConst (CInt 3) ]))
  ; LFun
      ( "f"
      , [ "a" ]
      , LApp ("g", [ LAdd (LConst (CInt 1), LConst (CInt 0)); LApp ("a", []) ]) )
  ]
;;

ll_ok "ll_4" (lambda_lifting cc4) ll4

let ll5 =
  [ LFun ("g", [ "a"; "b" ], LDiv (LApp ("a", []), LApp ("b", [])))
  ; LFun ("h", [ "a"; "c" ], LMul (LId "a", LApp ("c", [])))
  ; LFun
      ( "f"
      , [ "a" ]
      , LAdd
          ( LApp ("h", [ LApp ("a", []); LConst (CInt 1) ])
          , LApp ("g", [ LApp ("a", []); LConst (CInt 2) ]) ) )
  ]
;;

ll_ok "ll_5" (lambda_lifting cc5) ll5

let ll6 =
  [ LFun ("anon$1", [ "x" ], LId "x")
  ; LFun ("g", [], LApp ("anon$1", []))
  ; LFun ("anon$2", [ "a"; "x" ], LMul (LId "a", LId "x"))
  ; LFun ("h", [ "a" ], LApp ("anon$2", [ LApp ("a", []) ]))
  ; LFun
      ( "f"
      , [ "a" ]
      , LAdd
          (LApp ("g", [ LApp ("a", []) ]), LApp ("h", [ LApp ("a", []); LApp ("a", []) ]))
      )
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
      ( "anon$1"
      , [ "k"; "n"; "m" ]
      , ALet
          ( "anf_op#1"
          , AMul (AId "m", AId "n")
          , ALet
              ( "anf_op#2"
              , AMul (AId "k", AId "anf_op#1")
              , ACExpr (CImmExpr (AId "anf_op#2")) ) ) )
  ; AFun
      ( "fack"
      , [ "n"; "k" ]
      , ALet
          ( "anf_op#3"
          , ALte (AId "n", AInt 1)
          , ALet
              ( "anf_if#4"
              , AIf
                  ( AId "anf_op#3"
                  , ALet
                      ( "anf_op#5"
                      , ASub (AId "n", AInt 1)
                      , ALet
                          ( "anf_app#6"
                          , AApp (AId "n", [ AId "anf_op#5" ])
                          , ACExpr (CImmExpr (AId "anf_app#6")) ) )
                  , ALet
                      ( "anf_app#7"
                      , AApp (AId "k", [])
                      , ALet
                          ( "anf_app#8"
                          , AApp (AId "n", [])
                          , ALet
                              ( "anf_app#9"
                              , AApp (AId "anon$1", [ AId "anf_app#7"; AId "anf_app#8" ])
                              , ACExpr (CImmExpr (AId "anf_app#9")) ) ) ) )
              , ACExpr (CImmExpr (AId "anf_if#4")) ) ) )
  ; AFun ("anon$2", [ "x" ], ACExpr (CImmExpr (AId "x")))
  ; AFun
      ( "fac"
      , [ "n" ]
      , ALet
          ( "anf_app#10"
          , AApp (AId "n", [])
          , ALet
              ( "anf_app#11"
              , AApp (AId "anon$2", [])
              , ALet
                  ( "anf_app#12"
                  , AApp (AId "fack", [ AId "anf_app#10"; AId "anf_app#11" ])
                  , ACExpr (CImmExpr (AId "anf_app#12")) ) ) ) )
  ]
;;

let%test _ = anf_ok "anf_1" ll1 anf1

let anf4 =
  [ AFun
      ( "h"
      , [ "a"; "b"; "c" ]
      , ALet
          ( "anf_app#1"
          , AApp (AId "a", [])
          , ALet
              ( "anf_app#2"
              , AApp (AId "b", [])
              , ALet
                  ( "anf_app#3"
                  , AApp (AId "c", [])
                  , ALet
                      ( "anf_op#4"
                      , ADiv (AId "anf_app#2", AId "anf_app#3")
                      , ALet
                          ( "anf_op#5"
                          , AMul (AId "anf_app#1", AId "anf_op#4")
                          , ACExpr (CImmExpr (AId "anf_op#5")) ) ) ) ) ) )
  ; AFun
      ( "g"
      , [ "a"; "b" ]
      , ALet
          ( "anf_app#6"
          , AApp (AId "a", [])
          , ALet
              ( "anf_app#7"
              , AApp (AId "h", [ AId "anf_app#6"; AInt 2; AInt 3 ])
              , ACExpr (CImmExpr (AId "anf_app#7")) ) ) )
  ; AFun
      ( "f"
      , [ "a" ]
      , ALet
          ( "anf_op#8"
          , AAdd (AInt 1, AInt 0)
          , ALet
              ( "anf_app#9"
              , AApp (AId "a", [])
              , ALet
                  ( "anf_app#10"
                  , AApp (AId "g", [ AId "anf_op#8"; AId "anf_app#9" ])
                  , ACExpr (CImmExpr (AId "anf_app#10")) ) ) ) )
  ]
;;

let%test _ = anf_ok "anf_4" ll4 anf4

let anf5 =
  [ AFun
      ( "g"
      , [ "a"; "b" ]
      , ALet
          ( "anf_app#1"
          , AApp (AId "a", [])
          , ALet
              ( "anf_app#2"
              , AApp (AId "b", [])
              , ALet
                  ( "anf_op#3"
                  , ADiv (AId "anf_app#1", AId "anf_app#2")
                  , ACExpr (CImmExpr (AId "anf_op#3")) ) ) ) )
  ; AFun
      ( "h"
      , [ "a"; "c" ]
      , ALet
          ( "anf_app#4"
          , AApp (AId "c", [])
          , ALet
              ( "anf_op#5"
              , AMul (AId "a", AId "anf_app#4")
              , ACExpr (CImmExpr (AId "anf_op#5")) ) ) )
  ; AFun
      ( "f"
      , [ "a" ]
      , ALet
          ( "anf_app#6"
          , AApp (AId "a", [])
          , ALet
              ( "anf_app#7"
              , AApp (AId "h", [ AId "anf_app#6"; AInt 1 ])
              , ALet
                  ( "anf_app#8"
                  , AApp (AId "a", [])
                  , ALet
                      ( "anf_app#9"
                      , AApp (AId "g", [ AId "anf_app#8"; AInt 2 ])
                      , ALet
                          ( "anf_op#10"
                          , AAdd (AId "anf_app#7", AId "anf_app#9")
                          , ACExpr (CImmExpr (AId "anf_op#10")) ) ) ) ) ) )
  ]
;;

let%test _ = anf_ok "anf_5" ll5 anf5

let anf6 =
  [ AFun ("anon$1", [ "x" ], ACExpr (CImmExpr (AId "x")))
  ; AFun
      ( "g"
      , []
      , ALet ("anf_app#1", AApp (AId "anon$1", []), ACExpr (CImmExpr (AId "anf_app#1")))
      )
  ; AFun
      ( "anon$2"
      , [ "a"; "x" ]
      , ALet ("anf_op#2", AMul (AId "a", AId "x"), ACExpr (CImmExpr (AId "anf_op#2"))) )
  ; AFun
      ( "h"
      , [ "a" ]
      , ALet
          ( "anf_app#3"
          , AApp (AId "a", [])
          , ALet
              ( "anf_app#4"
              , AApp (AId "anon$2", [ AId "anf_app#3" ])
              , ACExpr (CImmExpr (AId "anf_app#4")) ) ) )
  ; AFun
      ( "f"
      , [ "a" ]
      , ALet
          ( "anf_app#5"
          , AApp (AId "a", [])
          , ALet
              ( "anf_app#6"
              , AApp (AId "g", [ AId "anf_app#5" ])
              , ALet
                  ( "anf_app#7"
                  , AApp (AId "a", [])
                  , ALet
                      ( "anf_app#8"
                      , AApp (AId "a", [])
                      , ALet
                          ( "anf_app#9"
                          , AApp (AId "h", [ AId "anf_app#7"; AId "anf_app#8" ])
                          , ALet
                              ( "anf_op#10"
                              , AAdd (AId "anf_app#6", AId "anf_app#9")
                              , ACExpr (CImmExpr (AId "anf_op#10")) ) ) ) ) ) ) )
  ]
;;

let%test _ = anf_ok "anf_6" ll6 anf6 *)
