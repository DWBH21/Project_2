{
  open Parser
}

let var = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*    (* to allow variable names to contain digits and underscore. But a variable name have to start with letters*)

rule token = parse
    [' ' '\t' '\r'] { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | ['0'-'9']+      { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | "int"           { TINT }
  | "bool"          { TBOOL }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "fun"           { FUN }
  | "is"            { IS }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "let"           { LET }  
  | ";;"            { SEMISEMI }
  | '='             { EQUAL }
  | '<'             { LESS }
  | "->"            { TARROW }
  | ':'             { COLON }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  (* Defining new tokens that will be identified by the lexer *)
  | '/'             { DIVIDE }  
  | '|'             { PIPE }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | "try"           { TRY }
  | "with"          { WITH }
  | "DivisionByZero" {DIVISIONBYZERO}
  | "GenericException" {GENERICEXCEPTION}
  | "raise" {RAISE} 
  | "exptn" { TEXP }
  | var             { VAR (Lexing.lexeme lexbuf) }
  | eof             { EOF }
  

{
}
