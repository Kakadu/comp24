open Ast
open Base

type error =
  | Occurs_check of type_id * typ
  | Unification_failed of typ * typ
  | Tuple_unequal_lens of typ * typ

(* type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of typ * typ
  ]

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Base.Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end *)


module R = struct
  type 'a t = type_id -> type_id * ('a, error) Result.t

  let return v st = st, Result.return v
  let fail e st = st, Result.Error e
  let bind : 'a t -> ('a -> 'b t) -> 'b t  = fun m f st ->
    let last, r = m st in
    match r with
    | Error err -> last, Error(err)
    | Ok v -> f v last

    module Syntax = struct
      let (let*) = bind
      let (>>=) = bind
    end

    let foldl l ~init ~f =
      let open Syntax in
      List.fold_left l ~init ~f:(fun acc e ->
        let* acc = acc in
        f acc e)
    let fresh = fun id -> id + 1, Result.Ok(id)
end

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
  | Typ_tuple(x, y, rest) -> Base.List.exists (x::y::rest) ~f:(occurs id)


  let free_vars t =
    let rec helper acc = function
    | Typ_bool | Typ_int -> acc
    | Typ_var v -> Varset.add v acc
    | Typ_fun(x, y)
    | Typ_cons(x, y) -> let acc = helper acc x in helper acc y
    | Typ_tuple(x, y, rest) -> List.fold_left (x::y::rest) ~init:acc ~f:helper
    in helper Varset.empty t
end

 
module Subst  = struct
  open R
  open R.Syntax

  type t = (type_id, typ, Int.comparator_witness) Base.Map.t  
  let empty : t =  Base.Map.empty(module Base.Int)
  let singleton k v = Base.Map.singleton(module Base.Int) k v
  let singleton_checked id t = if Type.occurs id t
     then Occurs_check(id, t) |> R.fail
     else singleton id t |> R.return
  let find key s = Map.find s key
  let remove k s = Map.remove s k
  let apply t s =
    let rec helper = function
    | Typ_var v -> 
      let r = match find v s with
      | Some v -> v
      | _ -> t
      in r  
    | Typ_fun(x, e) -> Typ_fun(helper x, helper e)
    | Typ_cons(x, xs) -> Typ_cons(helper x, helper xs)
    | Typ_tuple(x, y, rest) -> Typ_tuple(helper x, helper y, List.map ~f:helper rest)
    | _ -> t
    in helper t

  let rec unify_one t1 t2 =
    match t1, t2 with
    | Typ_bool, Typ_bool | Typ_int, Typ_int -> return empty
    | Typ_var x, Typ_var y when x = y -> return empty
    | Typ_var x, (_ as y)
    | (_ as y), Typ_var x -> singleton_checked x y
    | Typ_fun(x, xs), Typ_fun(y, ys)
    | Typ_cons(x, xs), Typ_cons(y, ys) -> unify [x, y; xs, ys]
    | Typ_tuple(x1, x2, xs), Typ_tuple(y1, y2, ys) ->
        let s = match List.zip xs ys with
        | Base.List.Or_unequal_lengths.Ok res -> (x1, y1)::(x2, y2)::res |> unify
        | Base.List.Or_unequal_lengths.Unequal_lengths -> Tuple_unequal_lens(t1, t2) |> fail
        in s
    | _-> Unification_failed(t1, t2) |> fail
  and unify types =
    match types with
    | [] -> return empty
    | (x, y)::rest ->
      let* rest_s = unify rest in
      let* curr_s = unify_one (apply x rest_s) (apply y rest_s) in
      compose rest_s curr_s
     
  and extend (id, t) s =
    match find id s with
    | Some t2 ->
      let t1 = apply t s in
      let* unified = unify_one t1 t2 in
      compose s unified
    | None ->
        let* new_s = singleton_checked id t in
        Map.fold s ~init:(return new_s) ~f:(fun ~key ~data acc ->
          let* acc = acc in
          let data = apply data acc in
          let* _ = singleton_checked key data in
          Map.add_exn acc ~key ~data |> return)

  and compose before after = Map.fold after ~init:(return before) ~f:(fun ~key ~data acc ->
    let* acc = acc in 
    extend (key, data) acc)

  let compose_all ss =
    List.fold ss ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)

  let pp fmt s =
    let open Stdlib.Format in
    fprintf fmt "[ ";
    Map.iteri s ~f:(fun ~key ~data -> fprintf fmt "%d -> %a;" key data fmt);
    fprintf  fmt "]";
end

type scheme = Scheme of Varset.t * typ (** Forall quantified vars * [typ] *)


module Scheme = struct
  type t = scheme
  
  let free_vars (Scheme(bs, t)) =
    Varset.diff (Type.free_vars t) bs
    
  let apply_subst subst (Scheme(bs, t)) =
    let refined_s = Varset.fold (fun bounded_v acc -> Subst.remove bounded_v acc) bs subst in
    Scheme(bs, Subst.apply t refined_s)
end

type var_name = string

module TypeEnv = struct
  type t = (var_name, Scheme.t, String.comparator_witness) Base.Map.t

  let free_vars e =
     Map.fold e ~init:(Varset.empty) 
     ~f:(fun ~key:_ ~data:scheme acc -> Varset.union acc (Scheme.free_vars scheme))
  let extend name scheme e =
    Map.add_exn e ~key:name ~data: scheme
end
  let find key s = Map.find s key

let fresh_var() =
  let open R.Syntax in
  let open R in
  fresh >>= fun n -> Typ_var(n) |> return


(* let generalize (Scheme(bs, typ)) =
  let
  Scheme() *)


