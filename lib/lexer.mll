{
    open Parser
    exception SyntaxError of string
}

let newline = "\r" | "\n" | "\r\n"

rule token = parse
    | ":" { COLON }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "let" { LET }
    | "(*prove*)" { PROVE }
    | "=" { EQUAL }
    | "(*hint:" { HINT }
    | "(*" { comment 0 lexbuf }
    | "*)" { ENDCOMMENT }
    | "axiom" { AXIOM }
    | "type" { TYPE }
    | "|" { VARIANT }
    | "of" { OF }
    | "match" { MATCH }
    | "rec" { REC }
    | "*" { STAR }
    | "," { COMMA }
    | "with" { WITH }
    | "->" { ARROW }
    | "induction" { INDUCTION }
    | newline { Lexing.new_line lexbuf; token lexbuf }
    | ['\t' ' ']+ { token lexbuf }
    | ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']+ as word { IDENT(word) }
    | _ { raise (SyntaxError ("Unexpected char: "^Lexing.lexeme lexbuf))}
    | eof {EOF}
    
and comment level = parse 
| "*)" { if level = 0 then token lexbuf else comment (level - 1) lexbuf } 
| "(*" { comment (level + 1) lexbuf } 
| newline { Lexing.new_line lexbuf;comment level lexbuf } 
| _{ comment level lexbuf } 
| eof { (print_endline"reached 25") ;raise (SyntaxError "Unclosed comment") }