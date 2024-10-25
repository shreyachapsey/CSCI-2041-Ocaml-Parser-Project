include Ast
module Parser = Parser
module Lexer = Lexer

let parse (s : string) : declaration list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
  ast

let rec string_of_expression (e : expression) : string =
  match e with
  | String i -> i
  | Expr (expr1, []) -> string_of_expression expr1
  | Expr (expr1, expr2) -> string_of_expression expr1 ^ " (" ^ String.concat ", " (List.map string_of_expression expr2) ^ ")"

let string_of_equality (eq : equality) : string =
  match eq with
  | Equality (expr1, expr2) -> string_of_expression expr1 ^ " = " ^ string_of_expression expr2

let string_of_variable (Variable (name, datatype): variable) : string =
"(" ^ name ^ " : " ^ datatype ^ ")"

let string_of_hint (h : hint option) : string =
  match h with 
  | Some Axiom -> "\n(*hint: axiom*)"
  | None -> ""
  | Some Induction i -> "\n(*hint: induction " ^ i ^ " *)"

let string_of_pattern (Constructor(name, lst)) : string =
  match lst with
  | [] -> name
  | lst ->  " | " ^ name ^ " of (" ^ String.concat "*" lst ^ ")"
  
let string_of_match (m : matching) : string =
  match m with
  | Branch(match_name, [], matched_name) -> match_name ^ " -> " ^ string_of_expression matched_name
  | Branch(match_name, var, matched_name) ->  let arg = List.map string_of_variable var in 
                                              " | " ^  match_name ^ "(" ^ (String.concat ", " arg) ^ ") -> " ^ string_of_expression matched_name
  (* | _ ->  " | " ^ " of (" ^ String.concat " -> " (List.map string_of_match m) ^ ")" *)

let string_of_declaration (d : declaration) : string = (*match some name and empty list and the other matches pattern*)
  match d with
  | Definition (name, var, eq, h) ->
  let arg = List.map string_of_variable var in
  "let (*prove*) " ^ name ^ " " ^ (String.concat " " arg) ^ " = (" ^ string_of_equality eq ^ ")" ^ string_of_hint h 
  | DVariant (type_name, variants) -> "type " ^ type_name ^ " = " ^ String.concat "" (List.map string_of_pattern variants)
  | DMatch (name, var, ret, matcher, matches) -> let arg = List.map string_of_variable var in
  "let rec " ^ name ^ " " ^ (String.concat " " arg) ^ " : " ^ ret ^ " = match " ^ matcher ^ " with " ^ String.concat "" (List.map string_of_match matches)
