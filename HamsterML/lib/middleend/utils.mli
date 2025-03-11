module Format = Format

(* State monad for generating fresh variable names *)
module R : sig
  type 'a t = int -> 'a * int

  val fresh : int -> int * int
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'b -> 'a * 'b
  val run : (int -> 'a * 'b) -> 'a
  val map_list : 'a list -> f:('a -> 'b t) -> 'b list t
  val fold_list : 'a list -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t
end

type id = string

(* Represents a set of unique identifiers *)
module NameSet : sig
  type t = id list

  val empty : t
  val extend : id -> t -> t
  val union : t -> t -> t
  val find : id -> t -> id option
  val pp : Format.formatter -> t -> unit
end

(* Represents a mapping from identifiers to other identifiers *)
module NameEnv : sig
  type t = (id, id, Base.String.comparator_witness) Base.Map.t

  val empty : t
  val find : id -> t -> id option
  val find_exn : id -> t -> id
  val extend : id * id -> t -> (id, id, Base.String.comparator_witness) Base.Map.t
  val pp : Format.formatter -> t -> unit
end
