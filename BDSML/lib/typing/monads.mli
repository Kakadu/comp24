open Types

type 'a t = TVarId.t -> TVarId.t * ('a, error) Result.t

val return : 'a -> 'a t
val fail : error -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
val map : ('a -> 'b t) -> 'a list -> 'b list t
val fold_left : ('a -> 'b -> 'a t) -> 'a t -> 'b list -> 'a t
val fresh : TVarId.t t
val run : 'a t -> TVarId.t -> ('a, error) Result.t
