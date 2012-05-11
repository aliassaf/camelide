open Term
open Rule
open Type

let path = ref ""

let loading_modules = ref []

let loaded_modules = ref []

let current_module () = List.hd !loading_modules

let check_dependencies = ref true

(* Create a lexer buffer from the given filename and register the filename in
   the lexbuf to obtain useful error messages. *)
let create_lexbuf filename =
  let filename =
    if filename = "-" then filename else
    Filename.concat !path (filename ^ ".dk") in
  let in_channel =
    if filename = "-" then stdin else
    try open_in filename 
    with Sys_error(msg) -> Error.module_error "Cannot open file %s" msg in
  let lexbuf = Lexing.from_channel in_channel in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p
      with Lexing.pos_fname = filename };
  lexbuf

let rec load_dependencies term =
  match term.body with
  | Type | Kind -> ()
  | Var(x) ->
      if not (Scope.is_qualified x) then () else
      let prefix, _ = Scope.unqualify x in
      load_module prefix
  | App(t1, t2) -> load_dependencies t1; load_dependencies t2
  | Lam(x, a, t) -> load_dependencies a; load_dependencies t
  | Pi (x, a, b) -> load_dependencies a; load_dependencies b

and load_dependencies_terms terms =
  List.iter load_dependencies terms

and process_declaration pos x a =
  load_dependencies a;
  Error.print_verbose 2 "Checking declaration %s..." x;
  let a = Scope.qualify_term a [] in
  if not !check_dependencies && List.length !loading_modules > 1 then () else
  check_declaration pos x a;
  Hashtbl.add declarations (Scope.qualify x) (normalize a)

and process_rule pos env left right =
  load_dependencies_terms (left :: right :: (snd (List.split env)));
  let head, _ = extract_spine left in (* To get the name of the rule *)
  Error.print_verbose 2 "Checking rule for %s..." head;
  let env, left, right = Scope.qualify_rule env left right in
  if not !check_dependencies && List.length !loading_modules > 1 then () else
  check_rule pos env left right;
  let _, spine = extract_spine left in (* To get the qualified spine *)
  Hashtbl.add rules (Scope.qualify head) (fst (List.split env), spine, right)

and process_instructions lexbuf =
  let instruction = Parser.toplevel Lexer.token lexbuf in
  match instruction with
    | Declaration(pos, x, a) ->
        process_declaration pos x a;
        process_instructions lexbuf
    | Rule(pos, env, left, right) ->
        process_rule pos env left right;
        process_instructions lexbuf
    | Eof -> ()

(* Modules are loaded, parsed, and executed on the fly, as needed. *)
and load_module name =
  if List.mem name !loaded_modules then () else
  if List.mem name !loading_modules
  then Error.module_error "Circular dependency between %s.dk and %s.dk" (current_module ()) name
  else begin
    Error.print_verbose 1 "Loading module %s..." name;
    loading_modules := name :: !loading_modules;
    Scope.push_scope name;
    let lexbuf = create_lexbuf name in
    process_instructions lexbuf;
    Scope.pop_scope ();
    loading_modules := List.tl !loading_modules;
    loaded_modules := name :: !loaded_modules;
    Error.print_verbose 1 "Finished loading %s!" name
  end

let load_file filename =  
  path := Filename.dirname filename;
  if Filename.check_suffix filename ".dk" then () else
  Error.module_error "Invalid file extension %s" filename;
  let module_name = Filename.chop_extension (Filename.basename filename) in
  load_module module_name;
  Error.print_verbose 0 "OK!"

let load_stdin () =
  path := ".";
  load_module "-";
  Error.print_verbose 0 "OK!"
