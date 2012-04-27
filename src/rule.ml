open Term
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
    match pattern.body, term.body with
    | Var(x), _ when List.mem x env ->
        begin try
          let t = List.assoc x theta in
          (* Terms in the spine are already in normal form,
             so we only need to check alpha-equivalence. *)
          if alpha_equiv t term then theta else raise MatchFailure
        with Not_found -> (x, term) :: theta end
    | Var(x), Var(y) when x = y -> theta
    | Var(_), _ -> raise MatchFailure
    | App(t1, t2), App(u1, u2) -> match_term t2 u2 (match_term t1 u1 theta)
    | App(_), _ -> raise MatchFailure
    | _ -> assert false in (* Invalid pattern, eliminated at parse time. *)
  let rec match_spine pattern spine theta =
    match pattern, spine with
    | [], _ -> 
        Error.print_verbose 3 "Matched rule for %a" print_term (app_spine head spine);
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
          Some(right, theta, spine)
        with MatchFailure -> match_some_rule rules end in
  (* Reverse the order of the list to get the good order. *)
  match_some_rule (List.rev (Hashtbl.find_all rules head))

(* Evaluation functions. Fully normalize term using beta-reduction and the
   stored reduction rules. Evaluation is call-by-value, left-to-right. *)
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
      | [] -> new_term (Lam(x, normalize a, normalize t)), spine
      | u :: spine -> reduce (subst [x, u] t) spine end
  | Pi (x, a, b) ->
      assert (List.length spine = 0); (* Eliminated by type-checking. *)
      new_term (Pi(x, normalize a, normalize b)), spine
and normalize term =
  let term, spine = reduce term [] in
  List.fold_left (fun t1 t2 -> new_term (App(t1, t2))) term spine

