open Base
open Ast
open Utils

type ctx =
  { count : (string, int, String.comparator_witness) Map.t
  ; reserved : (string, String.comparator_witness) Set.t
  }

type remaps = (string, string, String.comparator_witness) Map.t

module State = struct
  open State
  include StateM
  include StateM.Syntax

  type 'a t = (ctx, 'a) StateM.t
end

open State
open State.Syntax

let default_ctx =
  let reserved = List.map Std.stdlib ~f:(fun x -> x.name) in 
  { count = reserved |> List.map ~f:(fun x -> (x, 0)) |> Map.of_alist_exn (module String)
  ; reserved = reserved |> Set.of_list (module String)
  }
;;

let remaps_empty = Map.empty (module String)
let remaps_merge l r = Map.merge_skewed l r ~combine:(fun ~key _ r -> r)

let remaps_to_str r =
  Map.to_alist r |> List.map ~f:(fun (k, v) -> k ^ " -> " ^ v) |> String.concat ~sep:", "
;;

let reserved_prefixes = [ "__"; "anf"; "ll_"; "_start"]
let renamed_prefix = "__var_"

let is_internal id =
  List.exists reserved_prefixes ~f:(fun p -> String.is_prefix id ~prefix:p)
;;

let alpha_id ?(is_def=false) (remaps : remaps) id =
  let rec gen_new_id ctx id =
    match Map.find ctx.count id with
    | None ->
      let new_id = id in
      let ctx = { ctx with count = Map.set ctx.count ~key:id ~data:0 } in
      new_id, ctx
    | Some cnt ->
      let new_id = id ^ "_" ^ string_of_int cnt in
      let ctx = { ctx with count = Map.set ctx.count ~key:id ~data:(cnt + 1) } in
      if not (Map.mem remaps new_id || Map.mem ctx.count new_id)
      then new_id, ctx
      else gen_new_id ctx id
  in
  let id = if is_internal id then renamed_prefix ^ id else id in
  match Map.find remaps id with
  | Some new_id -> return (new_id, remaps)
  | None ->
    let* ctx = get in
    dbg "%s %b %b@\n" id (Set.mem ctx.reserved id) is_def;
    if Set.mem ctx.reserved id && not is_def
    then return (id, remaps)
    else (
      let id_ext = if is_def then "__" ^ (Std.lookup_extern id |> Option.value_exn) else id in
      let new_id, ctx = gen_new_id ctx id_ext in
      let* () = put ctx in
      let remaps = Map.set remaps ~key:id ~data:new_id in
      return (new_id, remaps))
;;

let rec alpha_pattern remaps = function
  | PConst _ as c -> return (c, remaps)
  | PWild -> return (PWild, remaps)
  | PIdent id ->
    let* new_id, remaps = alpha_id remaps id in
    return (PIdent new_id, remaps)
  | PTuple (x1, x2, xs) ->
    let* x1, remaps = alpha_pattern remaps x1 in
    let* x2, remaps = alpha_pattern remaps x2 in
    let* xs, remaps =
      List.fold
        xs
        ~init:(return ([], remaps))
        ~f:(fun acc x ->
          let* xs, remaps = acc in
          let* x, remaps = alpha_pattern remaps x in
          return (x :: xs, remaps))
      >>| fun (xs, remaps) -> List.rev xs, remaps
    in
    return (PTuple (x1, x2, xs), remaps)
  | PList xs ->
    List.fold
      xs
      ~init:(return ([], remaps))
      ~f:(fun acc x ->
        let* xs, remaps = acc in
        let* x, remaps = alpha_pattern remaps x in
        return (x :: xs, remaps))
    >>| fun (xs, remaps) -> PList (List.rev xs), remaps
  | PCons (hd, tl) ->
    let* hd, remaps = alpha_pattern remaps hd in
    let* tl, remaps = alpha_pattern remaps tl in
    return (PCons (hd, tl), remaps)
  | PAnn (pat, ann) ->
    let* pat, remaps = alpha_pattern remaps pat in
    return (PAnn (pat, ann), remaps)
;;

let rec alpha_expr remaps = function
  | EConst c -> return (EConst c, remaps)
  | EVar v ->
    let* new_id, remaps' = alpha_id remaps v in
    return (EVar new_id, remaps')
  | EApp (l, r) ->
    let* l, _ = alpha_expr remaps l in
    let* r, _ = alpha_expr remaps r in
    return (EApp (l, r), remaps)
  | EIfElse (i, t, e) ->
    let* i, _ = alpha_expr remaps i in
    let* t, _ = alpha_expr remaps t in
    let* e, _ = alpha_expr remaps e in
    return (EIfElse (i, t, e), remaps)
  | EFun (p, ps, e) ->
    let* p, remaps' = alpha_pattern remaps_empty p in
    let* ps, remaps' =
      List.fold
        ps
        ~init:(return ([], remaps'))
        ~f:(fun acc p ->
          let* ps, remaps' = acc in
          let* p, remaps' = alpha_pattern remaps' p in
          return (p :: ps, remaps'))
      >>| fun (ps, remaps) -> List.rev ps, remaps
    in
    let remaps'' = remaps_merge remaps remaps' in
    let* e, _ = alpha_expr remaps'' e in
    return (EFun (p, ps, e), remaps)
  | ELetIn (d, e) ->
    let* d, remaps' = alpha_def remaps d in
    let* e, _ = alpha_expr remaps' e in
    return (ELetIn (d, e), remaps)
  | ETuple (x1, x2, xs) ->
    let* x1, _ = alpha_expr remaps x1 in
    let* x2, _ = alpha_expr remaps x2 in
    let* xs =
      mfold xs ~init:[] ~f:(fun xs x ->
        let* x, _ = alpha_expr remaps x in
        return (x :: xs))
      >>| List.rev
    in
    return (ETuple (x1, x2, xs), remaps)
  | EList xs ->
    let* xs =
      mfold xs ~init:[] ~f:(fun xs x ->
        let* x, _ = alpha_expr remaps x in
        return (x :: xs))
      >>| List.rev
    in
    return (EList xs, remaps)
  | EMatch (exp, cases) ->
    let* exp, _ = alpha_expr remaps exp in
    let* cases =
      mfold cases ~init:[] ~f:(fun acc (p, e) ->
        let* p, remaps' = alpha_pattern remaps_empty p in
        let remaps'' = remaps_merge remaps remaps' in
        let* e, _ = alpha_expr remaps'' e in
        return ((p, e) :: acc))
      >>| List.rev
    in
    return (EMatch (exp, cases), remaps)

and alpha_def remaps = function
  | DLet (r, PIdent(id), e) when Std.lookup_extern id |> Option.is_some ->
    let* id, remaps' = alpha_id remaps id ~is_def:true in
    let remaps' = remaps_merge remaps remaps' in
    dbg "remaps: %s@\n" (remaps_to_str remaps');
    let* e, _ =
      match r with
      | Rec -> alpha_expr remaps' e
      | NonRec -> alpha_expr remaps e
    in
    return (DLet (r, PIdent id, e), remaps')
  | DLet (r, p, e) ->
    let* p, remaps' = alpha_pattern remaps_empty p in
    let remaps' = remaps_merge remaps remaps' in
    let* e, _ =
      match r with
      | Rec -> alpha_expr remaps' e
      | NonRec -> alpha_expr remaps e
    in
    return (DLet (r, p, e), remaps')
;;

let alpha_program p =
  run
    (mfold p ~init:(remaps_empty, []) ~f:(fun acc x ->
       let remaps, defs = acc in
       let* def, remaps' = alpha_def remaps x in
       return (remaps', def :: defs)))
    default_ctx
  |> fst
  |> snd
  |> List.rev
;;
