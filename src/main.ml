(* Parse the command line arguments and execute the given file. *)

let version = "1.2"

let print_version () =
  Format.printf "Camelide version %s@." version;
  exit 0

let files = ref []

let add_file filename =
  files := filename :: !files

let usage =
  Printf.sprintf "Usage:  %s <options> <files>\n" Sys.argv.(0)

let options = Arg.align [
  "-", Arg.Unit(fun () -> add_file "-"), " Read from stdin instead of a file";
  "--coc", Arg.Set(Type.use_coc), " Use the full Calculus of Construction modulo (experimental)";
  "--nocheckdep", Arg.Clear(Module.check_dependencies), " Load dependencies without checking (unsafe)";
  "-v", Arg.Set_int(Error.verbose_level), "<level> Set verbosity level";
  "-version", Arg.Unit(print_version), " Print version and exit";
  ]

let argument_error () =
  Arg.usage options usage;
  exit 1

let () =
(*  Sys.catch_break true;*)
  Arg.parse options add_file usage;
  List.iter Module.load_file (List.rev !files);
  prerr_endline "OK"
