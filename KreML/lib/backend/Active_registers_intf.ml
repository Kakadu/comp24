module type S = sig
  type 'a t
  val remove : 'a t -> 'a -> 'a t
  val add :  'a t -> 'a -> 'a t
  val size : 'a t -> int
end
