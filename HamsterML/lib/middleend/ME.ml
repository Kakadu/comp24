open Ast

type me_expr =
  | MEConst of value
  | MEVar of id
  | MEOperation of op
  | METuple of me_expr * me_expr * me_expr list
  | MEList of me_expr list
  | MEListConcat of me_expr * me_expr
  | MEConstraint of me_expr * dataType
  | MEApplication of me_expr * me_expr
  | MELet of funType * me_bind list * me_expr option
  | MEIf of me_expr * me_expr * me_expr option
[@@deriving show]

and me_bind = pattern * args * me_expr
and me_prog = me_expr list

module ListConverter : sig
  val tail : me_expr -> me_expr
  val len : me_expr -> me_expr
  val get : me_expr -> int -> me_expr
end = struct
  let ensure_list (lst : me_expr) =
    match lst with
    | MEList _ -> lst
    | _ -> failwith "Attempt to perform a list operation on a non-list"
  ;;

  let len lst = MEApplication (MEVar "length", ensure_list lst)

  let get lst i =
    MEApplication (MEApplication (MEVar "get", ensure_list lst), MEConst (Int i))
  ;;

  let head lst = MEApplication (MEVar "head", ensure_list lst)
  let tail lst = MEApplication (MEVar "tail", ensure_list lst)
end
