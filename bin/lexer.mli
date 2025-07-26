type token =
  | TRUE
  | FALSE
  | FLOAT of float
  | INT of int
  | ADD
  | SUB
  | MUL
  | DIV
  | LPAREN
  | RPAREN
  | NOT
  | OR
  | AND
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | OEF
val tokenise : char list -> token list
