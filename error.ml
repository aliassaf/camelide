open Lexing

let verbose_level = ref 0

(* Datatype to keep track of the location of the terms *)
type pos = Lexing.position * Lexing.position

let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)

exception NotImplemented

(* Printf-like function for printing information. *)
let print_verbose level =
  if level > !verbose_level then Printf.ifprintf stdout else
  Printf.kfprintf (fun _ -> print_newline ()) stdout

let print_pos (start_pos, end_pos) =
  Printf.eprintf "File %s, line %d, characters %d-%d:\n"
    start_pos.pos_fname start_pos.pos_lnum
    (start_pos.pos_cnum - start_pos.pos_bol + 1)
    (end_pos.pos_cnum - start_pos.pos_bol + 1)

(* Printf-like function for reporting an error before exiting. *)
let print_error_and_exit error_name =
  flush stdout;
  Printf.eprintf "%s: " error_name;
  Printf.kfprintf (fun _ -> prerr_newline (); exit 1) stderr

let syntax_error pos format =  
  print_pos pos;
  print_error_and_exit "Syntax Error" format

let scope_error pos format =
  print_pos pos;
  print_error_and_exit "Scope Error" format

let type_error pos format =
  print_pos pos;
  print_error_and_exit "Type Error" format

let pattern_error pos format =  
  print_pos pos;
  print_error_and_exit "Pattern Error" format

let module_error format =
  print_error_and_exit "Module Error" format

