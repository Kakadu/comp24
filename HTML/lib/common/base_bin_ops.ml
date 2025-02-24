type bin_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Leq (** <= *)
  | Less (** < *)
  | Geq (** >= *)
  | Gre (** > *)
  | Eq (** == *)
  | Neq (** != *)
  | And (** && *)
  | Or (** || *)
[@@deriving eq, show { with_path = false }]

let bin_op_to_str bo =
  let map = function
    | Add -> "+"
    | Sub -> "_"
    | Mul -> "*"
    | Div -> "/"
    | Leq -> "<="
    | Less -> "<"
    | Geq -> ">="
    | Gre -> ">"
    | Eq -> "="
    | Neq -> "!="
    | And -> "&&"
    | Or -> "||"
  in
  let res = [ "( "; map bo; " )" ] in
  String.concat "" res
;;
