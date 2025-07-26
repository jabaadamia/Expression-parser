  open Print_ast
(* open Lexer *)
(* let string_of_token = function *)
(*   | TRUE -> "TRUE" *)
(*   | FALSE -> "FALSE" *)
(*   | FLOAT f -> "FLOAT(" ^ string_of_float f ^ ")" *)
(*   | INT n -> "INT(" ^ string_of_int n ^ ")" *)
(*   | ADD -> "ADD" *)
(*   | SUB -> "SUB" *)
(*   | MUL -> "MUL" *)
(*   | DIV -> "DIV" *)
(*   | LPAREN -> "LPAREN" *)
(*   | RPAREN -> "RPAREN" *)
(*   | NOT -> "NOT" *)
(*   | OR -> "OR" *)
(*   | AND -> "AND" *)
(*   | EQ -> "EQ" *)
(*   | NEQ -> "NEQ" *)
(*   | LT -> "LT" *)
(*   | GT -> "GT" *)
(*   | LEQ -> "LEQ" *)
(*   | GEQ -> "GEQ" *)
(*   | OEF -> "OEF" *)


let () = 
  let input = "(12/3+4)*10>=130-2" in
  let chars = List.of_seq (String.to_seq input) in
  let tokens = Lexer.tokenise chars in
  let exp = Parser.build_ast tokens in
  let output = string_of_expr exp in
  print_endline output
