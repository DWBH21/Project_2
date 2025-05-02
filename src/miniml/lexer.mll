{
  open Parser
}

let var = ['a'-'z' 'A'-'Z']+

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
