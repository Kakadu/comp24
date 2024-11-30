open Ast
open Base

module Varset = struct
  include Stdlib.Set.Make(Int)
  let pp fmt s =
    let open Stdlib.Format in
    fprintf fmt "[ ";
    iter (fprintf fmt "%d; ") s;
    fprintf fmt "]"
end

module Type = struct
  let rec occurs id = function
  | Typ_bool | Typ_int -> false
  | Typ_var v -> id = v
  | Typ_fun(x, y)
  | Typ_cons(x, y) -> occurs id x || occurs id y
  | Typ_tuple(x, y, rest) -> Base.List.exists (x::y::rest) ~f:(fun e -> occurs id e)


  let free_vars t =
    let rec helper acc = function
    | Typ_bool | Typ_int -> acc
    | Typ_var v -> Varset.add v acc
    | Typ_fun(x, y)
    | Typ_cons(x, y) -> let acc = helper acc x in helper acc y
    | Typ_tuple(x, y, rest) -> List.fold_left (x::y::rest) ~init:acc ~f:(fun acc e -> helper acc e)
    in helper Varset.empty t
end

 
module Subst  = struct
  type t = (type_id, typ, Int.comparator_witness) Base.Map.t  
  let empty : t =  Base.Map.empty(module Base.Int)
  let singleton k v = Base.Map.singleton(module Base.Int) k v

  let find key s : typ = Map.find_exn s key
  let apply t s = t
  let rec unify_one t1 t2 =
    match t1, t2 with
    | Typ_bool, Typ_bool | Typ_bool, Typ_int | Typ_int, Typ_bool | Typ_int, Typ_int -> empty
    | Typ_var x, Typ_var y when x = y -> empty
    | Typ_var x, (_ as y)
    | (_ as y), Typ_var x -> singleton x y
    | Typ_fun(x, xs), Typ_fun(y, ys)
    | Typ_cons(x, xs), Typ_cons(y, ys) -> unify [x, y; xs, ys]
    | Typ_tuple(x1, x2, xs), Typ_tuple(y1, y2, ys) ->
        let s = match List.zip xs ys with
        | Base.List.Or_unequal_lengths.Ok res -> (x1, y1)::(x2, y2)::res |> unify
        | Base.List.Or_unequal_lengths.Unequal_lengths -> failwith "unification failed"
        in s
    | _-> failwith "unification failed"
  and unify types =
    match types with
    | [] -> empty
    | (x, y)::rest ->
      let s = unify rest in
      unify_one (apply x s) (apply y s) 
  let compose s1 s2 = s1
  let pp fmt s =
    let open Stdlib.Format in
    fprintf fmt "[ ";
    Map.iteri s ~f:(fun ~key ~data -> fprintf fmt "%d -> %a;" key data fmt);
    fprintf  fmt "]";

end