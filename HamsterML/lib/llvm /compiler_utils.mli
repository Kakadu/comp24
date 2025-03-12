type ('st, 'a) t = 'st -> 'st * ('a, string) result

val return : 'a -> 'b -> 'b * ('a, 'c) result
val fail : 'a -> 'b -> 'b * ('c, 'a) result
val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
val read : ('st, 'st) t
val write : 'st -> ('st, unit) t
val run : ('st, 'a) t -> 'st -> 'st * ('a, string) result
val ( let* ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
val ( *> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'c) t
val ( >>| ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
val ( <* ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b) t
val map_list : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t

module MapString : sig
  type key = string
  type 'a t = 'a Map.Make(String).t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val min_binding_opt : 'a t -> (key * 'a) option
  val max_binding : 'a t -> key * 'a
  val max_binding_opt : 'a t -> (key * 'a) option
  val choose : 'a t -> key * 'a
  val choose_opt : 'a t -> (key * 'a) option
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val find_first : (key -> bool) -> 'a t -> key * 'a
  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val find_last : (key -> bool) -> 'a t -> key * 'a
  val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_rev_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
  val of_seq : (key * 'a) Seq.t -> 'a t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

type local_map = Llvm.llvalue MapString.t
type glob_funs = (Llvm.lltype * Llvm.llvalue) MapString.t
type state = string * glob_funs * local_map

val read_curr_fun : (state, string) t
val write_curr_fun : string -> (state, unit) t
val read_glob_funs : (state, glob_funs) t
val write_glob_funs : glob_funs -> (state, unit) t
val findopt_glob_fun : string -> (state, (Llvm.lltype * Llvm.llvalue) option) t
val add_glob_fun : string -> Llvm.lltype -> Llvm.llvalue -> (state, unit) t
val read_local_vars : (state, local_map) t
val write_local_vars : local_map -> (state, unit) t
val findopt_loc_var : string -> (state, Llvm.llvalue option) t
val add_loc_var : string -> Llvm.llvalue -> (state, unit) t
val new_fun_scope : string -> (state, 'a) t -> (state, 'a) t
