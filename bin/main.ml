let () =
  print_string "Enter an expression: ";
  flush stdout;

  let input = read_line () in
  
  let chars = List.of_seq (String.to_seq input) in
  let tokens = Lexer.tokenise chars in
  let exp = Parser.build_ast tokens in
  let result = Eval.eval_expr exp in
  let output = Print_ast.string_of_expr exp in
  
  print_endline "\n---- AST ----";
  print_endline output;
  print_endline "------";
  Printf.printf "Result: %s\n" (Eval.string_of_value result)
