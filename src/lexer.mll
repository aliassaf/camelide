{
open Parser

let pos lexbuf = (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
}

let id = (['_' '\'' '0'-'9' 'a'-'z' 'A'-'Z'])+
let qid = id '.' id
let special = "#" [^'\n']*
let open_comment = "(*" | "(;"
let close_comment = "*)" | ";)"

rule token = parse
  | [' ' '\t']   { token lexbuf } (* skip blanks *)
  | '\n'         { Lexing.new_line lexbuf; token lexbuf }
  | open_comment { comment (pos lexbuf) [] lexbuf }
  | "#IMPORT "   { IMPORT }
  | special      { token lexbuf }
  | "Type"       { TYPE }
  | id as id     { ID(id) }
  | qid as qid   { QID(qid) }
  | ":"   { COLON }
  | ","   { COMMA }
  | "."   { DOT }
  | "->"  { ARROW }
  | "=>"  { DOUBLE_ARROW }
  | "-->" { LONG_ARROW }
  | ":="  { DEF }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | "["   { LBRACK }
  | "]"   { RBRACK }
  | "{"   { LBRACE }
  | "}"   { RBRACE }
  | eof   { EOF }
  | _     { Error.syntax_error (pos lexbuf) "Invalid character" }

and comment current stack = parse
  | open_comment  { comment (pos lexbuf) (current :: stack) lexbuf }
  | close_comment { match stack with [] -> token lexbuf | h :: t -> comment h t lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment current stack lexbuf }
  | eof  { Error.syntax_error current "This comment is not closed" }
  | _    { comment current stack lexbuf }

