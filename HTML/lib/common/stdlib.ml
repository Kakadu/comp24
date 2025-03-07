open AstLib.Ast

let stdlib_typed =
  [ "print_int", TArr (TGround GInt, TGround GUnit)
  ; "print_bool", TArr (TGround GBool, TGround GUnit)
  ; "print_newline", TArr (TGround GUnit, TGround GUnit)
  ; "base +", TArr (TGround GInt, TGround GInt)
  ; "base -", TArr (TGround GInt, TGround GInt)
  ; "+", TArr (TGround GInt, TArr (TGround GInt, TGround GInt))
  ; "-", TArr (TGround GInt, TArr (TGround GInt, TGround GInt))
  ; "*", TArr (TGround GInt, TArr (TGround GInt, TGround GInt))
  ; "/", TArr (TGround GInt, TArr (TGround GInt, TGround GInt))
  ; "<=", TArr (TVar "_a", TArr (TVar "_a", TGround GBool))
  ; "<", TArr (TVar "_a", TArr (TVar "_a", TGround GBool))
  ; ">=", TArr (TVar "_a", TArr (TVar "_a", TGround GBool))
  ; ">", TArr (TVar "_a", TArr (TVar "_a", TGround GBool))
  ; "=", TArr (TVar "_a", TArr (TVar "_a", TGround GBool))
  ; "!=", TArr (TVar "_a", TArr (TVar "_a", TGround GBool))
  ; "&&", TArr (TGround GBool, TArr (TGround GBool, TGround GBool))
  ; "||", TArr (TGround GBool, TArr (TGround GBool, TGround GBool))
  ; "RTE_ERROR_MATCH_FAILURE", TArr (TGround GUnit, TGround GUnit)
  ; "GET_HEAD", TArr (TList (TVar "_a"), TVar "_a")
  ; "GET_TALE", TArr (TList (TVar "_a"), TList (TVar "_a"))
  ; "GET_NTH", TArr (TVar "_a", TVar "_b")
  ; "( = )", TArr (TVar "_a", TArr (TVar "_a", TGround GBool))
  ; "( != )", TArr (TVar "_a", TArr (TVar "_a", TGround GBool))
  ; "( && )", TArr (TGround GBool, TArr (TGround GBool, TGround GBool))
  ; "leq", TArr (TVar "_a", TArr (TVar "_a", TGround GBool))
  ; "eq", TArr (TVar "_a", TArr (TVar "_a", TGround GBool))
  ]
;;

let stdlib = List.map fst stdlib_typed
