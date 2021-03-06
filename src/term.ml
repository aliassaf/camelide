type sort =
  | Type
  | Kind

(* Terms are annotated by an additional argument to keep track of their
   location, so that we can print useful error messages. The value flag
   is true when the term has already been evaluated. *)
type term = {
  pos : Error.pos;
  body : body;
  value : bool}

and body =
  | Sort of sort
  | Var of string
  | App of term * term
  | Lam of string * term * term
  | Pi  of string * term * term

(* Terms that are created outside of parsing have a dummy position. *)
let new_term body = {pos = Error.dummy_pos; body = body; value = false}

(* Create a new value during evaluation. *)
let new_value body = {pos = Error.dummy_pos; body = body; value = true}

(* Flag a term that has already been evaluated. *)
let evaluated term = {term with value = true}

(* Table mapping all the currently declared constants to their type. *)
let declarations : (string, term) Hashtbl.t = Hashtbl.create 10007

let is_declared x = Hashtbl.mem declarations x

let free_vars_acc fv term =
  let rec free_vars bound fv term =
    match term.body with
    | Sort _ -> fv
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
let variant x avoid =
  let rec variant n =
    let x' = (x ^ string_of_int n) in
    if List.mem x' avoid then variant (n + 1) else x' in
  if x = "" then x else variant 0

(* Capture-avoiding parallel substitution. Subtitutions are give as a list
   [x1, u1; ...; xn, un]. *)
let subst theta term =
  let rec subst theta term =
    match term.body with
    | Sort _ -> term
    | Var(x) ->
        begin try List.assoc x theta
        with Not_found -> term end
    | App(t1, t2) ->
        new_term (App(subst theta t1, subst theta t2))
    | Lam(x, a, t) ->
        let x', theta' = rename x term theta in
        new_term (Lam(x', subst theta a, subst theta' t))
    | Pi (x, a, b) ->
        let x', theta' = rename x term theta in
        new_term (Pi (x', subst theta a, subst theta' b))
  and rename x term theta = (* Rename to avoid capture. *)
    let fv = free_vars_terms (term :: (snd (List.split theta))) in
    let x' = variant x fv in
    x', (x, new_term (Var(x'))) :: theta in
  subst theta term

let alpha_equiv t u =
  let rec alpha_equiv t u env =
    match t.body, u.body with
    | Sort s1, Sort s2 -> s1 = s2
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

let term_type = new_term (Sort Type)

let term_kind = new_term (Sort Kind)

