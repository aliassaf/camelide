open Term
open Pattern

type instruction =
  | Declaration of Error.pos * string * term
  | Rules of (Error.pos * (string * term) list * pattern * term) list
  | Eof
