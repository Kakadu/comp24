module R = struct
  open Base.Result

  type 'a t = int -> int * ('a, string) Result.t

  let fail error state = state, fail error
  let return value last = last, return value

  let ( >>= ) (monad : 'a t) f state =
    let last, result = monad state in
    match result with
    | Error e -> last, Error e
    | Ok value -> f value last
  ;;

  let ( let* ) = ( >>= )
  let ( >>| ) m f = m >>= fun x -> return @@ f x
  let fresh last = last + 1, Ok last
  let run m = snd (m 0)

  let map f xs =
    let* res =
      List.fold_left
        (fun acc x ->
          let* acc = acc in
          let* res = f x in
          return (res :: acc))
        (return [])
        xs
    in
    return (List.rev res)
  ;;

  let sequence lst =
    let rec aux acc = function
      | [] -> return (List.rev acc)
      | x :: xs -> x >>= (fun y -> aux (y :: acc) xs)
    in
    aux [] lst 
end