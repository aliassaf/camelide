(* Parse the command line arguments and execute the given file. *)

let files = ref []

let from_stdin = ref false

let add_file filename =
  files := filename :: !files

let usage =
  Printf.sprintf "Usage:  %s <options> <files>\n" Sys.argv.(0)

let options = Arg.align [
  "--coc", Arg.Set(Type.use_coc), " Use the full Calculus of Construction modulo (experimental)";
  "--nocheck", Arg.Clear(Module.check_dependencies), " Do not type-check dependencies (unsound)";
  "--stdin", Arg.Set(from_stdin), " Read from stdin instead of a file";
  "-v", Arg.Set_int(Error.verbose_level), "<level> Set verbosity level" ]

let argument_error () =
  Arg.usage options usage;
  exit 1

let () =
  Sys.catch_break true;
  Arg.parse options add_file usage;
  if not !from_stdin && !files = [] then argument_error() else
  if !from_stdin then Module.load_stdin () else
  List.iter Module.load_file (List.rev !files)
