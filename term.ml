(* Terms are annotated by an additional argument to keep track of their
   location, so that we can print useful error messages. *)
type term = {
  pos : Error.pos;
  body : body}
and body =
  | Type
  | Kind
  | Var of string
  | App of term * term
  | Lam of string * term * term
  | Pi  of string * term * term

type instruction =
  | Declaration of string * term
  | Rule of (string * term) list * term * term
  | Eof

(* Terms that are created outside of parsing have a dummy position. *)
let new_term body = {pos = Error.dummy_pos; body = body}

(* Table mapping all the currently declared constants to their type. *)
let declarations : (string, term) Hashtbl.t = Hashtbl.create 10007

let is_declared x = Hashtbl.mem declarations x

let extract_spine term =
  let rec extract_spine term spine =
    match term.body with
    | Var(x) -> x, spine
    | App(t1, t2) -> extract_spine t1 (t2 :: spine)
    | _ -> assert false in
  extract_spine term []

let rec app_spine head spine =
  List.fold_left (fun t1 t2 -> new_term (App(t1, t2))) (new_term (Var(head))) spine

let free_vars_acc fv term =
  let rec free_vars bound fv term =
    match term.body with
    | Type -> fv
    | Kind -> fv
    | Var(x) ->
        if Hashtbl.mem declarations x || List.mem x bound || List.mem x fv
        then fv else x :: fv
    | App(t1, t2) -> free_vars bound (free_vars bound fv t1) t2
    | Lam(x, a, t) -> free_vars (x :: bound) (free_vars bound fv a) t
    | Pi (x, a, b) -> free_vars (x :: bound) (free_vars bound fv a) b in
  free_vars [] fv term

let free_vars term = free_vars_acc [] term

let free_vars_terms terms = List.fold_left free_vars_acc [] terms

(* Return a variant of the variable x which does not appear in the list of
   variables avoid. Useful to avoid capture. *)
let rec variant x avoid =
  if not (List.mem x avoid) then x else variant (x ^ "'") avoid

(* Capture-avoiding parallel substitution. Subtitutions are give as a list
   [x1, u1; ...; xn, un]. *)
let subst theta term =
  let rec subst theta term =
    match term.body with
    | Type -> term
    | Kind -> term
    | Var(x) ->
        begin try List.assoc x theta
        with Not_found -> term end
    | App(t1, t2) ->
        new_term (App(subst theta t1, subst theta t2))
    | Lam(x, a, t) ->
        let x', theta' = rebind x term theta in
        new_term (Lam(x', subst theta a, subst theta' t))
    | Pi (x, a, b) ->
        let x', theta' = rebind x term theta in
        new_term (Pi (x', subst theta a, subst theta' b))
  and rebind x term theta = (* Rebind to avoid capture. *)
    let fv = free_vars_terms (term :: (snd (List.split theta))) in
    let x' = variant x fv in
    x', (x, new_term (Var(x'))) :: theta in
  subst theta term

let alpha_equiv t u =
  let rec alpha_equiv t u env =
    match t.body, u.body with
    | Type, Type -> true
    | Kind, Kind -> true
    | Var(x), Var(y) ->
        begin try List.assoc x env = y
        with Not_found -> x = y && not (List.exists (fun (z, w) -> w = y) env) end
    | App(t1, t2), App(u1, u2) ->
        alpha_equiv t1 u1 env && alpha_equiv t2 u2 env
    | Lam(x, a, t), Lam(y, b, u) ->
        alpha_equiv a b env && alpha_equiv t u ((x, y) :: env)
    | Pi(x, a1, a2), Pi(y, b1, b2) ->
        alpha_equiv a1 b1 env && alpha_equiv a2 b2 ((x, y) :: env)
    | _ -> false in
  alpha_equiv t u []

