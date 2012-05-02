(* Parse the command line arguments and execute the given file. *)

type input_type =
  | File of string
  | Stdin
  | Nothing

let input = ref Nothing

let set_filename name =
  input := File(name)

let set_stdin () =
  input := Stdin

let options = Arg.align [
  "--stdin", Arg.Unit(set_stdin), " Read from stdin instead of a file";
  "-v", Arg.Set_int(Error.verbose_level), "<level> Set verbosity level" ]

let usage =
  Printf.sprintf "Usage: %s <options> <file>" Sys.argv.(0)

let () =
  Arg.parse options set_filename usage;
  match !input with
  | File(filename) -> Module.load_file filename
  | Stdin -> Module.load_stdin ()
  | Nothing -> Arg.usage options usage; exit 1
