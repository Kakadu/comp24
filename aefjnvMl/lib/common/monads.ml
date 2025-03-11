(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module BaseSEMonad (StateT : Base.T) (ErrorT : Base.T) = struct
  type 'a t = StateT.t -> StateT.t * ('a, ErrorT.t) Result.t

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

  let ( let+ ) x f = x >>| f

  let fold_left_t xs ~init ~f =
    Base.List.fold_left xs ~init ~f:(fun acc x ->
      let* acc = acc in
      f acc x)
  ;;

  let revt lt =
    let* l = lt in
    return @@ List.rev l
  ;;

  let mapt xs f =
    revt
    @@ fold_left_t xs ~init:(return []) ~f:(fun acc x ->
      let+ x' = f x in
      x' :: acc)
  ;;

  let ignore_t f =
    let* f_res = f in
    return @@ ignore f_res
  ;;
end

module GenericCounterMonad (StateT : Base.T) (ErrorT : Base.T) = struct
  include
    BaseSEMonad
      (struct
        type t = int * StateT.t
      end)
      (ErrorT)

  let run at start_id new_st = run at (start_id, new_st)

  let fresh : int t =
    let* num, st = read in
    let+ () = save (num + 1, st) in
    num
  ;;

  let save st =
    let* num, _ = read in
    save (num, st)
  ;;

  let read : StateT.t t =
    let+ _, st = read in
    st
  ;;
end

module CounterMonad (ErrorT : Base.T) = struct
  include
    GenericCounterMonad
      (struct
        type t = unit
      end)
      (ErrorT)

  let run at id = run at id ()
end
