open Term
open Pattern
open Printer

(* Mapping from head constants to reduction rules. The rule
   [x1: a1, ..., xm: am] head u1 ... un -> right
   is stored in the entry for head as the triplet
   ([x1; ...; xm], [u1; ...; un], right)
   Rules are stored in the reverse order in which they are read. *)
let rules = Hashtbl.create 10007

exception MatchFailure

(* Matches spine against pattern. If successful, return a substitution theta
   such that (theta pattern) = spine, raising MatchFailure otherwise. *)
let match_spine env head pattern spine =
  let rec match_term pattern term theta =
    match pattern.p_body, term.body with
    | PVar(x), _ when List.mem x env ->
        begin try
          let t = List.assoc x theta in
          (* Terms in the spine are already in normal form,
             so we only need to check alpha-equivalence. *)
          if alpha_equiv t term then theta else raise MatchFailure
        with Not_found -> (x, term) :: theta end
    | PVar(x), Var(y) when x = y -> theta
    | PVar(_), _ -> raise MatchFailure
    | PApp(p1, p2), App(u1, u2) -> match_term p2 u2 (match_term p1 u1 theta)
    | PApp(_), _ -> raise MatchFailure
    | PDot(_), _ -> theta in
  let rec match_spine pattern spine theta =
    match pattern, spine with
    | [], _ ->
        theta, spine
    | _, [] -> raise MatchFailure
    | t1 :: pattern, t2 :: spine ->
        match_spine pattern spine (match_term t1 t2 theta) in
  match_spine pattern spine []

(* Try to match spine against some rule corresponding to the head constant. *)
let match_some_rule head spine =
  let rec match_some_rule rules =
    match rules with
    | [] -> None
    | (env, left, right) :: rules ->
        begin try
          let theta, spine = match_spine env head left spine in
          Error.print_verbose 3 "Matched rule %a --> %a" print_pattern (app_spine head left) print_term right;
          Some(right, theta, spine)
        with MatchFailure -> match_some_rule rules end in
  (* Reverse the order of the list to get the good order. *)
  match_some_rule (List.rev (Hashtbl.find_all rules head))

(* Evaluation functions. Fully normalize term using beta-reduction and the
   stored reduction rules. Evaluation is call-by-value, right-to-left. *)
let rec reduce term spine =
  match term.body with
  | Type | Kind -> assert (List.length spine = 0); term, spine
  | Var(x) ->
      begin match match_some_rule x spine with
      | None -> term, spine
      | Some(term, theta, spine) -> reduce (subst theta term) spine end
  | App(t1, t2) -> reduce t1 (normalize t2 :: spine)
  | Lam(x, a, t) ->
      begin match spine with
      | [] -> new_term (Lam(x, a, normalize t)), spine
      | u :: spine -> reduce (subst [x, u] t) spine end
  | Pi (x, a, b) ->
      assert (List.length spine = 0); (* Eliminated by type-checking. *)
      new_term (Pi(x, a, b)), spine
and normalize term =
  if term.value then term else
  let term, spine = reduce term [] in
  List.fold_left (fun t1 t2 -> new_value (App(t1, t2))) (evaluated term) spine

let equiv t u =
  let rec equiv t u env =
    match (normalize t).body, (normalize u).body with
    | Type, Type -> true
    | Kind, Kind -> true
    | Var(x), Var(y) ->
        begin try List.assoc x env = y
        with Not_found -> x = y && not (List.exists (fun (z, w) -> w = y) env) end
    | App(t1, t2), App(u1, u2) ->
        equiv t1 u1 env && equiv t2 u2 env
    | Lam(x, a, t), Lam(y, b, u) ->
        equiv a b env && equiv t u ((x, y) :: env)
    | Pi(x, a1, a2), Pi(y, b1, b2) ->
        equiv a1 b1 env && equiv a2 b2 ((x, y) :: env)
    | _ -> false in
  equiv t u []

