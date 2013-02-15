open Term

type pattern = {
  p_pos : Error.pos;
  p_body : body}

and body =
  | PVar of string
  | PApp of pattern * pattern
  | PDot of term

let new_pattern p_body = {p_pos = Error.dummy_pos; p_body = p_body}

let rec term_of_pattern pattern =
  let term_body =
    match pattern.p_body with
    | PVar(x) -> Var(x)
    | PApp(p1, p2) -> App(term_of_pattern p1, term_of_pattern p2)
    | PDot(t) -> t.body
    in
  {pos = pattern.p_pos; body = term_body; value = false}

let rec free_pvars_acc fv pattern =
  match pattern.p_body with
  | PVar(x) ->
      if Hashtbl.mem declarations x || List.mem x fv
      then fv else x :: fv
  | PApp(p1, p2) -> free_pvars_acc (free_pvars_acc fv p1) p2
  | PDot(t) -> free_vars_acc fv t

let free_pvars pattern = free_pvars_acc [] pattern

let extract_spine pattern =
  let rec extract_spine pattern spine =
    match pattern.p_body with
    | PVar(x) -> x, spine
    | PApp(p1, p2) -> extract_spine p1 (p2 :: spine)
    | _ -> assert false in
  extract_spine pattern []

let rec app_spine head spine =
  List.fold_left (fun p1 p2 -> new_pattern (PApp(p1, p2))) (new_pattern (PVar(head))) spine

(* Check that a pattern is well-formed. A well-formed pattern consists of
   variables and constant applications. Variables and dot patterns cannot
   be applied. *)
let check_pattern env pattern =
  let rec check_head pattern =
    match pattern.p_body with
    | PVar(x) ->
        if not (List.mem_assoc x env) then () else
        Error.pattern_error pattern.p_pos "Variables are not allowed in head positions"
    | PApp(t1, t2) ->
        check_head t1;
        check_pattern t2
    | PDot(_) -> Error.pattern_error pattern.p_pos "Dot patterns are not allowed in head positions"
  and check_pattern pattern =
    match pattern.p_body with
    | PVar(_) -> ()
    | PDot(_) -> ()
    | _ -> check_head pattern in
  check_head pattern
