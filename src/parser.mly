%{
open Term
open Pattern
open Instruction

let pos () = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())

let term body = {pos = pos (); body = body; value = false}

let pattern body = {p_pos = pos(); p_body = body}
%}

%token <string> ID QID
%token TYPE COLON COMMA DOT ARROW DOUBLE_ARROW LONG_ARROW
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE EOF
%start toplevel
%type <Instruction.instruction> toplevel
%%

qid:
  | QID { $1 }
  | ID { $1 }

toplevel:
  | declaration { $1 }
  | rules { Rules($1) }
  | EOF { Eof }
  | error { Error.syntax_error (pos ()) "Invalid expression" }

declaration:
  | binding DOT { let x, a = $1 in Declaration(pos (), x, a) }

rules:
  | rule DOT { [$1] }
  | rule rules { $1 :: $2 }

rule:
  | env pattern LONG_ARROW term { (pos (), $1, $2, $4) }

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
  | pattern simple_pattern { pattern (PApp($1, $2)) }

simple_pattern:
  | qid { pattern (PVar($1)) }
  | LPAREN pattern RPAREN { $2 }
  | LBRACE term RBRACE { pattern (PDot($2)) }
