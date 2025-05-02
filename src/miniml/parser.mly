%{
  open Syntax
%}

%token TINT
%token TBOOL
%token TEXP   // Add a new variable type for exceptions
%token TARROW
%token <Syntax.name> VAR
%token <int> INT
%token TRUE FALSE
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE    // Declares a new token DIVIDE i.e a keyword which cannot be simplified further
%token EQUAL LESS
%token IF THEN ELSE
%token FUN IS
%token COLON
%token LPAREN RPAREN
%token LBRACE RBRACE // Declares tokens for curly braces
%token LET
%token SEMISEMI
%token EOF
%token TRY WITH     // Declares tokens for handling exceptions
%token DIVISIONBYZERO GENERICEXCEPTION // Declares the tokens for the two exceptions
%token PIPE // Declares token for | 
%token RAISE // token for raising exceptions for testing our try with blocks

// The %start keywords indicate the two different types of input that may be passed to the parser for parsing. 
// It could be multiple lines of code in a file
%start file
// in which case a list of command would be produced as output
%type <Syntax.command list> file

// or it could be a single line at a time if an interpreter type one line by one line execution of the code is happening  
%start toplevel
// in which case only a single command would be produced as output
%type <Syntax.command> toplevel

// the rules below are the associativity and precedence rules for the grammar
%nonassoc IS
%nonassoc ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES DIVIDE // Saying that divide has left associativity and higher precedence than plus and minus
%right TARROW

%%

// this defines that if the input to the parser is a file i.e multiple lines of code, what formats could this file have
file:
  | EOF
    { [] }
  | e = expr EOF
    { [Expr e] }
  | e = expr SEMISEMI lst = file
    { Expr e :: lst }
  | ds = nonempty_list(def) SEMISEMI lst = file
    { ds @ lst }
  | ds = nonempty_list(def) EOF
    { ds }

// an input from a top level could either be a definition followed by ;; or an expression followed by ;;
toplevel:
  | d = def SEMISEMI
    { d }
  | e = expr SEMISEMI
    { Expr e }

// syntax for a definition in this language is let variable_name = expression. It extracts the variable name and the expression and calls a constructor Def mentioned in syntax.ml
def:
  | LET x = VAR EQUAL e = expr
    { Def (x, e) }

expr: mark_position(plain_expr) { $1 }
plain_expr:
  | e = plain_app_expr
    { e }
  | MINUS n = INT
    { Int (-n) }
  | e1 = expr PLUS e2 = expr
    { Plus (e1, e2) }
  | e1 = expr MINUS e2 = expr
    { Minus (e1, e2) }
  | e1 = expr TIMES e2 = expr
    { Times (e1, e2) }
  | e1 = expr DIVIDE e2 = expr  // Adding the parsing rule for the divide operation
    { Divide(e1, e2) }          // Calling constructor defined in syntax.ml
  | e1 = expr EQUAL e2 = expr
    { Equal (e1, e2) }
  | e1 = expr LESS e2 = expr
    { Less (e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { If (e1, e2, e3) }
  | FUN x = VAR LPAREN f = VAR COLON t1 = ty RPAREN COLON t2 = ty IS e = expr
    { Fun (x, f, t1, t2, e) }

  | RAISE e = exception_type      // parsing rules for raising exceptions explicitly in the program
    { Raise (e) }
  | TRY LBRACE try_e = expr RBRACE WITH LBRACE handlers = exception_handlers RBRACE    // Adding the exception parsing rule -> try {} with {exception_handler}, where exception handler is the type(s) of the exception(s)
    { TryWith (try_e , handlers) }                            // Calling constructor defined in syntax.ml
  
  // here handlers is of type (exception_type * exp) list where exp is the result in case the given exception_type is encountered

// Exception Handlers is a non empty list of tuples of an exception type and the expression to be evaluated in case of the expression being caught
exception_handlers: 
  | PIPE ex = exception_type TARROW e = expr    // base case
    { [(ex , e)] }
  | PIPE ex = exception_type TARROW e = expr rest = exception_handlers
    { (ex, e) :: rest }
  
// Defining different exception types
exception_type:
  | DIVISIONBYZERO
    { DivisionByZero }
  | GENERICEXCEPTION e_int = INT          // GENERICEXCEPTION takes an integer argument 
    { GenericException (e_int) }          // Calls the constructor Generic Exception with appropriate value in Syntax.ml
  | GENERICEXCEPTION MINUS e_int = INT    // The parsing rule when the integer argument is negative. 
    { GenericException (-e_int) }

//expressions that deal with function application
app_expr: mark_position(plain_app_expr) { $1 }
plain_app_expr:
  | e = plain_simple_expr             // base case of an application that results in an variable or a literal ( which may be a function)
    { e }
  | e1 = app_expr e2 = simple_expr        // recursive case -> app_expr may be another functional application and simple_expr is the argument 
    { Apply (e1, e2) }                    // E.g. f a b -> (f a) 

simple_expr: mark_position(plain_simple_expr) { $1 }
plain_simple_expr:
  | x = exception_type
    { Exception x }       // Defining the exception_type just like a variable type.
  | x = VAR
    { Var x }
  | TRUE    
    { Bool true }
  | FALSE
    { Bool false }
  | n = INT
    { Int n }
  | LPAREN e = plain_expr RPAREN	
    { e }    
  | LBRACE e = plain_expr RBRACE
    { e } 
ty:
  | TBOOL
    { TBool }
  | TINT
    { TInt }
  | TEXP
    { TExp }
  | t1 = ty TARROW t2 = ty
    { TArrow (t1, t2) }
  | LPAREN t = ty RPAREN
    { t }

// it wraps the result of parsing an expression with the start and endpos of that expression in the original source code
mark_position(X):
  x = X
  { Zoo.locate ~loc:(Zoo.make_location $startpos $endpos) x }

%%