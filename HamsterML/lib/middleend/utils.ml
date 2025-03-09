open Base
module Format = Stdlib.Format

module R = struct
  type 'a t = int -> 'a * int

  let fresh st = st, st + 1

  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
    fun m f state ->
    let value, st = m state in
    (f value) st
  ;;

  let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t =
    fun m f state ->
    let value, st = m state in
    f value, st
  ;;

  let ( let* ) = ( >>= )
  let bind m f = m >>= f
  let return value st = value, st
  let run monad = fst @@ monad 0

  let rec map_list list ~f =
    match list with
    | [] -> return []
    | x :: xs ->
      let* y = f x in
      let* ys = map_list xs ~f in
      return (y :: ys)
  ;;

  let rec fold_list list ~init ~f =
    match list with
    | [] -> return init
    | x :: xs ->
      let* acc = f init x in
      fold_list xs ~init:acc ~f
  ;;
end

type id = string

module NameSet = struct
  type t = id list

  let empty : t = []

  let extend (name : id) (set : t) : t =
    if List.mem set name ~equal:String.equal then set else set @ [ name ]
  ;;

  let union (s1 : t) (s2 : t) : t =
    List.fold s2 ~init:s1 ~f:(fun acc name -> extend name acc)
  ;;

  let find (name : id) (set : t) = List.find set ~f:(String.equal name)
  let pp fmt (set : t) = Format.fprintf fmt "[%s]" (String.concat ~sep:", " set)
end

module NameEnv = struct
  type t = (id, id, String.comparator_witness) Map.t

  let empty : t = Map.empty (module String)
  let find key (env : t) = Map.find env key

  let find_exn key (env : t) = Map.find_exn env key
  let extend (k, v) (env : t) = Map.set env ~key:k ~data:v

  let pp fmt (env : t) =
    let pp_names fmt (old_name, new_name) =
      Format.fprintf fmt "%s -> %s" old_name new_name
    in
    if Map.is_empty env
    then Format.fprintf fmt "NameEnv is empty!\n"
    else
      Map.iteri env ~f:(fun ~key ~data -> Format.fprintf fmt "%a\n" pp_names (key, data))
  ;;
end
