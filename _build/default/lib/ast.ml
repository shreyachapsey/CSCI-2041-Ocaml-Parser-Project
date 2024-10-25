type hint =
| Axiom
| Induction of string

(* type pattern =
| Constructor of string * pattern list
| Variable of string * string *)

type variable = Variable of string * string

type expression =
| String of string
| Expr of expression * expression list
(* | Matchings of expression * (matchingg list)
and matchingg = Matchingg of expression * variable list * expression *)

type type_variant = Variant of expression * expression list

type pattern = Constructor of string * string list

type matching = Branch of string * variable list * expression

(*when do we create new data type vs new constructor under existing data type*)
type equality =
| Equality of expression * expression
  
type declaration =
| Definition of (string * variable list * equality * hint option)
| DVariant of string * pattern list
| DMatch of string * variable list * string * string * matching list