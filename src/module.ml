open Term
open Pattern
open Reduction
open Instruction
open Typing
open Printer

let path = ref ""

let check_dependencies = ref true

let loading_modules = ref []

let loaded_modules = ref []

let check_current = ref true

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
  if !check_current
    then Error.print_verbose 2 "Checking declaration %s..." x
    else Error.print_verbose 2 "Loading declaration %s (no checking)..." x;
  let a = Scoping.qualify_term a [] in
  if !check_current then check_declaration pos x a;
  Hashtbl.add declarations (Scoping.qualify x) (normalize a)

let process_definition pos x a t =
  if !check_current
    then Error.print_verbose 2 "Checking definition %s..." x
    else Error.print_verbose 2 "Loading definition %s (no checking)..." x;
  match a with
  | Some(a) -> 
      let a = Scoping.qualify_term a [] in
      let t = Scoping.qualify_term t [] in
      if !check_current then check_definition pos x a t;
      Hashtbl.add declarations (Scoping.qualify x) (normalize a);
      Hashtbl.add rules (Scoping.qualify x) ([], [], t)
  | None ->
      let t = Scoping.qualify_term t [] in
      let a = type_of [] t in
      Hashtbl.add declarations (Scoping.qualify x) a;
      Hashtbl.add rules (Scoping.qualify x) ([], [], t)

let process_opaque_def pos x a t =
  if !check_current
    then Error.print_verbose 2 "Checking opaque definition %s..." x
    else Error.print_verbose 2 "Loading opaque definition %s (no checking)..." x;
  match a with
  | Some(a) -> 
      let a = Scoping.qualify_term a [] in
      let t = Scoping.qualify_term t [] in
      if !check_current then check_definition pos x a t;
      Hashtbl.add declarations (Scoping.qualify x) (normalize a)
  | None ->
      let t = Scoping.qualify_term t [] in
      let a = type_of [] t in
      Hashtbl.add declarations (Scoping.qualify x) a

let process_rule pos env left right =
  let head, _ = extract_spine left in (* To get the name of the rule *)
  if !check_current
    then Error.print_verbose 2 "Checking rule for %s..." head
    else Error.print_verbose 2 "Loading rule for %s (no checking)..." head;
  let env, left, right = Scoping.qualify_rule env left right in
  if !check_current then check_rule pos env left right;
  let _, spine = extract_spine left in (* To get the qualified spine *)
  Hashtbl.add rules (Scoping.qualify head) (fst (List.split env), spine, right)

let process_rules rules =
  List.iter (fun (pos, env, left, right) -> process_rule pos env left right) rules

let process_normalize t =
  Error.print_verbose 2 "Normalizing %a" print_term t;
  let t = Scoping.qualify_term t [] in
  let a = type_of [] t in
  let t = normalize t in
  Error.print_verbose 2 "%a" print_term t

let rec process_instruction instruction =
  match instruction with
    | Name(pos, name) -> ()
    | Import(pos, name) ->
        let backup_check = !check_current in
        let backup_scope = !Scoping.current_scope in
        check_current := !check_dependencies;
        load_module name;
        check_current := backup_check;
        Scoping.current_scope := backup_scope
    | Normalize(pos, t) ->
        process_normalize t
    | Declaration(pos, x, a) ->
        process_declaration pos x a
    | Definition(pos, x, a, t) ->
        process_definition pos x a t
    | OpaqueDef(pos, x, a, t) ->
        process_opaque_def pos x a t
    | Rules(rules) ->
        process_rules rules
    | Eof -> ()

and process_instructions lexbuf =
  let instruction = Parser.instruction Lexer.token lexbuf in
  match instruction with
  | Eof -> ()
  | _ ->
      process_instruction instruction;
      process_instructions lexbuf

(* Modules are loaded, parsed, and executed on the fly, as needed. *)
and load_module name =
  if not (List.mem name !loaded_modules) then (
    if List.mem name !loading_modules then
      Error.module_error "Circular dependency in module %s" name;
    loading_modules := name :: !loading_modules;
    if not (List.mem name !loaded_modules) then
      if !check_current
        then Error.print_verbose 1 "Loading module %s..." name
        else Error.print_verbose 1 "Loading module %s (no checking)..." name;
    Scoping.current_scope := name;
    let lexbuf = create_lexbuf name in
    process_instructions lexbuf;
    loading_modules := List.tl !loading_modules;
    loaded_modules := name :: !loaded_modules;
    Error.print_verbose 1 "Finished loading %s!" name)

let load_file filename =
  if filename = "-" then (
    path := ".";
    load_module "-")
  else (
    path := Filename.dirname filename;
    if Filename.check_suffix filename ".dk" then () else
    Error.module_error "Invalid file extension %s" filename;
    let module_name = Filename.chop_extension (Filename.basename filename) in
    load_module module_name)    
