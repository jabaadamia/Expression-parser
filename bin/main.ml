open Lexer

let string_of_token = function
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | FLOAT f -> "FLOAT(" ^ string_of_float f ^ ")"
  | INT n -> "INT(" ^ string_of_int n ^ ")"
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | NOT -> "NOT"
  | OR -> "OR"
  | AND -> "AND"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | GT -> "GT"
  | LEQ -> "LEQ"
  | GEQ -> "GEQ"
  | OEF -> "OEF"

let () = 
  let input = "(12/3+4)*10>=130-2" in
  let chars = List.of_seq (String.to_seq input) in
  let tokens = Lexer.tokenise chars in
  let token_strs = List.map string_of_token tokens in
  let output = String.concat " " token_strs in
  print_endline output
