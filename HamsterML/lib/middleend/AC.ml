open Ast
open Base
module Format = Stdlib.Format
open Utils.R

module NameSet = struct
  type t = (id, String.comparator_witness) Set.t

  let empty : t = Set.empty (module String)
  let extend (name : id) (set : t) : t = Set.add set name
  let union : t -> t -> t = Set.union
  let find (name : id) (set : t) = Set.find set ~f:(fun x -> String.equal x name)

  let to_args (set : t) : args =
    List.rev @@ Set.fold set ~init:[] ~f:(fun acc id -> Var id :: acc)
  ;;

  let rec generate_name (set : t) =
    let* fresh_num = fresh in
    let varname = "LL_arg_" ^ Int.to_string fresh_num in
    match find varname set with
    | None -> return (extend varname set, varname)
    | Some _ -> generate_name set
  ;;
end
