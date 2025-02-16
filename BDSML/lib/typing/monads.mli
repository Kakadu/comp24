open Types

type 'a t = VarId.t -> VarId.t * ('a, error) Result.t

val return : 'a -> 'a t
val fail : error -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
val fresh : VarId.t t
val run : 'a t -> VarId.t -> ('a, error) Result.t
