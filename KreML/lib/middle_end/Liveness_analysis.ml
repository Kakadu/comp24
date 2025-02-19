open Flambda

type range = { var: string; s: int; e: int }
type analysis_result = (string * range list) list


let rec analyse_expr expr counter res_start res_end =
  let open Base in
  match expr with
  | Fl_const _ -> counter, res_start, res_end
  | Fl_closure { arrange; _} ->
    analyse_list (List.map arrange ~f:snd) counter res_start res_end
  | Fl_var id -> counter, res_start, Map.set res_end ~key:id ~data:counter
  | Fl_let (Some id, value, scope) ->
    let res_start = Map.set res_start ~key:id ~data:counter in
    let counter, res_start, res_end = analyse_expr value counter res_start res_end in
    analyse_expr scope (counter + 1) res_start res_end
  | Fl_let (None, value, scope) ->
    let counter, starts, ends = analyse_expr value counter res_start res_end in
    analyse_expr scope (counter + 1) starts ends
  | Fl_tuple elems -> analyse_list elems counter res_start res_end
  | Fl_app (f, args) -> analyse_list (f :: args) counter res_start res_end
  | Fl_binop (_, x, y) | Fl_cons (x, y) -> analyse_list [ x; y ] counter res_start res_end
  | Fl_ite (c, t, e) -> analyse_list [ c; t; e ] counter res_start res_end
  | Fl_getfield (_, o) -> analyse_expr o counter res_start res_end

and analyse_list list counter res_start res_end =
  List.fold_left
    (fun (c, s, e) expr -> analyse_expr expr c s e)
    (counter, res_start, res_end)
    list
;;

let analyse_fun = function
  | Fun_with_env { body; _ } | Fun_without_env { body; _ } ->
    let open Base in
    let res_start = Map.empty (module String)
    (* let res_start =
      List.fold param_names ~init:res_start ~f:(fun acc name ->
        Map.set acc ~key:name ~data:0) *)
    in
    let res_end = Map.empty (module String) in
    let _, ss, es = analyse_expr body 1 res_start res_end in
    let ranges = Map.fold ss ~init:[] ~f:(fun ~key:var ~data:s acc ->
      match Map.find es var with
      | Some e -> {s; e; var}::acc
      | None -> {s; e = s; var}::acc)
    in List.sort ranges ~compare:(fun r1 r2 -> Base.Int.compare r1.s r2.s)
;;

let analyse_program flstucture =
  List.map (fun (id, f) -> id, analyse_fun f) flstucture

let pp fmt analysis_result =
  let open Format in
  let print_vars fmt vars =
    List.iter (fun {var; s; e} -> fprintf fmt "@[%s: [%i, %i]@]@." var s e) vars in
  let print_fun (id, vars) =
    fprintf fmt "@[%s:@, %a @] @." id print_vars vars in
  List.iter print_fun analysis_result
    