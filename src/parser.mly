%{
open Term

let pos () = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())

let term body = {pos = pos (); body = body}
%}

%token <string> ID QID
%token TYPE COLON COMMA DOT ARROW DOUBLE_ARROW LONG_ARROW
%token LPAREN RPAREN LBRACK RBRACK EOF
%start toplevel
%type <Term.instruction> toplevel
%%

qid:
  | QID { $1 }
  | ID { $1 }

toplevel:
  | declaration { $1 }
  | rule { $1 }
  | EOF { Eof }
  | error { Error.syntax_error (pos ()) "Invalid expression" }

declaration:
  | binding DOT { let x, a = $1 in Declaration(x, a) }

rule:
  | env pattern LONG_ARROW term DOT { Rule($1, $2, $4) }

binding:
  | ID COLON term { ($1, $3) }

env:
  | LBRACK RBRACK { [] }
  | LBRACK env_nonempty RBRACK { $2 }

env_nonempty:
  | binding { [$1] }
  | binding COMMA env_nonempty { $1 :: $3 }

term:
  | domain ARROW term { let (x, a) = $1 in term (Pi(x, a, $3)) }
  | domain DOUBLE_ARROW term { let (x, a) = $1 in term (Lam(x, a, $3)) }
  | applicative { $1 }

domain:
  | ID COLON applicative { ($1, $3) }
  | applicative { ("", $1) }

applicative:
  | simple { $1 }
  | applicative simple { term (App($1, $2)) }

simple:
  | TYPE { term (Type) }
  | qid { term (Var($1)) }
  | LPAREN term RPAREN { $2 }

pattern:
  | simple_pattern { $1 }
  | pattern simple_pattern { term (App($1, $2)) }

simple_pattern:
  | qid { term (Var($1)) }
  | LPAREN pattern RPAREN { $2 }
