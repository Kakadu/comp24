open Ast

type llexpr =
  | LLConst of const 
  | LLBinOp of bin_op * llexpr * llexpr

  | LLVar of id 
  | LLIf of llexpr * llexpr * llexpr 
  | LLApp of llexpr * llexpr 
  | LLLetIn of id * llexpr * llexpr 
[@@deriving show { with_path = false }]

type llbinding = LLLet of bool * id * pat list * llexpr
[@@deriving show { with_path = false }]

type llprogram = llbinding list [@@deriving show { with_path = false }]