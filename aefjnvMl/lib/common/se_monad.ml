(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Base_SE_Monad = struct
  type ('st, 'a, 'err) t = 'st -> 'st * ('a, 'err) Result.t

  let run at new_st = at new_st
  let fail e st = st, Base.Result.fail e
  let return x st = st, Base.Result.return x
  let read st = (return st) st
  let save new_ctx _ = new_ctx, Result.ok ()

  let ( >>= ) a f st =
    let st', res = a st in
    match res with
    | Error x -> st', Error x
    | Ok a -> f a st'
  ;;

  let ( let* ) x f = x >>= f

  let ( >>| ) a f st =
    let st', res = a st in
    match res with
    | Error e -> st', Error e
    | Ok a' -> st', Ok (f a')
  ;;

  let fold_left_t xs ~init ~f =
    Base.List.fold_left xs ~init ~f:(fun acc x ->
      let* acc = acc in
      f acc x)
  ;;
end
