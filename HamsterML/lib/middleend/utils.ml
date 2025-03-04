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
end

type id = string

module NameSet = struct
  type t = (id, String.comparator_witness) Set.t

  let empty : t = Set.empty (module String)
  let extend (name : id) (set : t) : t = Set.add set name
  let union : t -> t -> t = Set.union
  let find (name : id) (set : t) = Set.find set ~f:(fun x -> String.equal x name)
end

module NameEnv = struct
  type t = (id, id, String.comparator_witness) Map.t

  let empty : t = Map.empty (module String)
  let find key (env : t) = Map.find env key
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
