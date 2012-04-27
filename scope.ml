open Term

(* Maintain a stack of scope names. Useful for keeping track of the current
   scope name as modules are loaded. *)
let scopes = Stack.create ()

let current_scope () = Stack.top scopes

let push_scope name = Stack.push name scopes

let pop_scope () = ignore (Stack.pop scopes)

let is_qualified x = String.contains x '.'

(* Prefix the unqualified identifier x with the name of the current module. *)
let qualify x =
  current_scope () ^ "." ^ x

(* Split the qualified identifier x into module name and unqualified name. *)
let unqualify x =
  try
    let i = String.index x '.'
    in (String.sub x 0 i, String.sub x (i + 1) (String.length x - i - 1))
  with Not_found -> ("", x)

(* Check scoping and fully qualify all identifiers in term. An unqualified
   identifier can be qualified with the current module name if it is not bound
   (by a lambda or a rule environment) and it has already been declared. *)
let rec qualify_term term bound =
  let body =
    match term.body with
    | Type -> Type
    | Kind -> Kind
    | Var(x) ->
        if is_qualified x || List.mem x bound then Var(x) else
        let qx = qualify x in
        if is_declared qx then Var(qx) else
        Error.scope_error term.pos "Unbound variable %s" x
    | App(t1, t2) -> App(qualify_term t1 bound, qualify_term t2 bound)
    | Lam(x, a, t) -> Lam(x, qualify_term a bound, qualify_term t (x :: bound))
    | Pi (x, a, b) -> Pi (x, qualify_term a bound, qualify_term b (x :: bound))
  in { pos = term.pos; body = body }

let rec qualify_env env bound =
  match env with
  | [] -> []
  | (x, a) :: env -> (x, qualify_term a bound) :: (qualify_env env (x :: bound))

let qualify_rule env left right =
  let env = qualify_env env [] in
  let bound = (fst (List.split env)) in
  let left = qualify_term left bound in
  let right = qualify_term right bound in
  env, left, right
