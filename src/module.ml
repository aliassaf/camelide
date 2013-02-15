open Term
open Pattern
open Rule
open Instruction
open Type

let path = ref ""

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

let process_declaration pos x a =
  Error.print_verbose 2 "Checking declaration %s..." x;
  let a = Scope.qualify_term a [] in
  check_declaration pos x a;
  Hashtbl.add declarations (Scope.qualify x) (normalize a)

let process_definition pos x a t =
  Error.print_verbose 2 "Checking definition %s..." x;
  let a = Scope.qualify_term a [] in
  let t = Scope.qualify_term t [] in
  check_definition pos x a t;
  Hashtbl.add declarations (Scope.qualify x) (normalize a);
  Hashtbl.add rules (Scope.qualify x) ([], [], t)

let process_opaque_def pos x a t =
  Error.print_verbose 2 "Checking opaque definition %s..." x;
  let a = Scope.qualify_term a [] in
  let t = Scope.qualify_term t [] in
  check_definition pos x a t;
  Hashtbl.add declarations (Scope.qualify x) (normalize a)

let process_rule pos env left right =
  let head, _ = extract_spine left in (* To get the name of the rule *)
  Error.print_verbose 2 "Checking rule for %s..." head;
  let env, left, right = Scope.qualify_rule env left right in
  check_rule pos env left right;
  let _, spine = extract_spine left in (* To get the qualified spine *)
  Hashtbl.add rules (Scope.qualify head) (fst (List.split env), spine, right)

let process_rules rules =
  List.iter (fun (pos, env, left, right) -> process_rule pos env left right) rules

let process_instruction instruction =
  match instruction with
    | Declaration(pos, x, a) ->
        process_declaration pos x a
    | Definition(pos, x, a, t) ->
        process_definition pos x a t
    | OpaqueDef(pos, x, a, t) ->
        process_opaque_def pos x a t
    | Rules(rules) ->
        process_rules rules
    | _ -> ()

let rec process_instructions lexbuf =
  let instruction = Parser.instruction Lexer.token lexbuf in
  match instruction with
  | Eof -> ()
  | _ ->
      process_instruction instruction;
      process_instructions lexbuf

(* Modules are loaded, parsed, and executed on the fly, as needed. *)
let load_module name =
  Error.print_verbose 1 "Loading module %s..." name;
  Scope.current_scope := name;
  let lexbuf = create_lexbuf name in
  process_instructions lexbuf;
  Error.print_verbose 1 "Finished loading %s!" name

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
