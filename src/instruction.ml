open Term
open Pattern

type instruction =
  | Name of Error.pos * string
  | Import of Error.pos * string
  | Declaration of Error.pos * string * term
  | Definition of Error.pos * string * term option * term
  | OpaqueDef of Error.pos * string * term option * term
  | Rules of (Error.pos * (string * term) list * pattern * term) list
  | Eof
