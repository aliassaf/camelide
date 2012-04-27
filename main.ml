(* Parse the command line arguments and execute the given file. *)

let filename = ref ""

let set_filename name =
  filename := name

let options = Arg.align [
  "-v", Arg.Set_int(Error.verbose_level), "<level> Set verbosity level" ]

let usage =
  Printf.sprintf "Usage: %s <options> <file>" Sys.argv.(0)

let () =
  Arg.parse options set_filename usage;
  match !filename with
  | "" -> Arg.usage options usage; exit 1
  | filename -> Module.load_file filename
