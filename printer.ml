open Printf
open Term

(* Try to shorten fully qualified names by dropping the module prefix when
   it is the current scope. *)
let print_var out x =
  let (prefix, name) = Scope.unqualify x in
  if prefix = "" || prefix = Scope.current_scope ()
  then fprintf out "%s" name
  else fprintf out "%s" x

let print_term out term =
  let rec print_term out term =
    match term.body with
    | Lam(x, a, t) ->
        if x = ""
        then fprintf out "%a => %a" print_applicative a print_term t
        else fprintf out "%s : %a => %a" x print_applicative a print_term t
    | Pi (x, a, b) ->
        if x = ""
        then fprintf out "%a -> %a" print_applicative a print_term b
        else fprintf out "%s : %a -> %a" x print_applicative a print_term b
    | _ -> print_applicative out term
  and print_applicative out term =
    match term.body with
    | App(t1, t2) -> fprintf out "%a %a" print_applicative t1 print_simple t2
    | _ -> print_simple out term
  and print_simple out term =
    match term.body with
    | Type -> fprintf out "Type"
    | Kind -> fprintf out "Kind"
    | Var(x) -> fprintf out "%a" print_var x
    | _ -> fprintf out "(%a)" print_term term in
  print_term out term

let print_declaration out (x, a) =
  fprintf out "%s : %a.\n" x print_term a

let print_rule out (env, t1, t2) =
  let rec print_env out env =
    match env with
    | [] -> ()
    | [(x, a)] -> fprintf out "%s : %a" x print_term a
    | (x, a) :: t -> fprintf out "%s : %a, %a" x print_term a print_env t in
  fprintf out "[%a] %a --> %a.\n" print_env env print_term t1 print_term t2

