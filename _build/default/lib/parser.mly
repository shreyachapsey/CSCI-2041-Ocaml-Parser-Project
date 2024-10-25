%{
  open Ast
%}
%token <string> IDENT
%token COLON
%token LPAREN
%token RPAREN
%token LET
%token PROVE
%token EQUAL
%token HINT
%token AXIOM
%token EOF
%token TYPE
%token MATCH
%token VARIANT
%token OF
%token STAR
%token REC
%token COMMA
%token WITH
%token ARROW
%token INDUCTION
%token ENDCOMMENT

%start main
%type <Ast.declaration list> main

%%

main: 
  | list(declaration) EOF { $1 }

pattern:
  | VARIANT ; variant_name = IDENT {Constructor (variant_name,[])} 
  | VARIANT ; variant_name = IDENT ; OF ; LPAREN ; expr2 = separated_list(STAR,IDENT);RPAREN { Constructor (variant_name, expr2)}

matching:
  | VARIANT ; match_name = IDENT ; ARROW ; matched_name = expression {Branch(match_name, [], matched_name)}
  | VARIANT ; match_name = IDENT ; LPAREN ; var = separated_nonempty_list(COMMA, variable) ; RPAREN ; ARROW ; matched_name = expression {Branch(match_name, var, matched_name)}
  // | VARIANT ; match_name = IDENT ; ARROW ; matched_name = expression {Branch(match_name, [], matched_name)}
  // | VARIANT ; match_name = IDENT ; var = separated_list(COMMA,variable) ; ARROW ; matched_name = expression {Branch(match_name, var, matched_name)}
(*matchname = expression*)
declaration:
  | LET ; PROVE ; name = IDENT ; var = list(variable) ; EQUAL; equality = equality; hint=option(hint) { Definition (name, var, equality, hint)}
  (*to do: rec match*)
  | TYPE ; type_name = IDENT ; EQUAL ; variants = list(pattern) { DVariant (type_name, variants) }
  | LET ; REC ; name = IDENT ; var = list(variable) ; COLON ; returntype = IDENT ; EQUAL;  
    MATCH ; matcher = IDENT ; WITH ; matches = list(matching) { DMatch (name, var, returntype, matcher, matches) } 

hint:
  | HINT ; AXIOM ; ENDCOMMENT { Axiom }
  | HINT ; INDUCTION ; induction = IDENT ; ENDCOMMENT { Induction (induction) }

variable:
  | arg = IDENT ; COLON ; datatype = IDENT {Variable (arg, datatype) }
  | LPAREN ; arg = variable ; RPAREN { arg }

// matchingg:
//   | name = IDENT ; var = list(variable) ; e = expression {Matchingg (String(name), var, e)}

expression: 
  | expr = IDENT { String (expr)}
  | LPAREN ; expr = expression ; RPAREN { expr }
  | expr1 = expression ; expr2 = IDENT { Expr (expr1, [String expr2]) } 
  | expr1 = expression ; LPAREN ; expr2 = separated_nonempty_list(COMMA,expression) ; RPAREN { Expr (expr1, expr2) }

equality:
  | LPAREN ; eq = equality ; RPAREN { eq }
  | expr1 = expression ; EQUAL ; expr2 = expression { Equality (expr1, expr2)}

