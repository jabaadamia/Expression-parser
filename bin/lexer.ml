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


let is_digit c = '0' <= c && c <= '9' 

let rec collect_digits f = function
  | [] -> ([], [])
  | x :: xs when f x ->
      let (ys, zs) = collect_digits f xs in
      (x :: ys, zs)
  | xs -> ([], xs)

let rec tokenise char_seq = match char_seq with
  | [] -> []
  | ' ' :: rest -> tokenise rest
  
  | 'f'::'a'::'l'::'s'::'e'::rest -> FALSE :: tokenise rest
  | 't'::'r'::'u'::'e'::rest -> TRUE :: tokenise rest
  
  | '=' :: '=' :: rest -> EQ :: tokenise rest 
  | '!' :: '=' :: rest -> NEQ :: tokenise rest
  | '>' :: '=' :: rest -> GEQ :: tokenise rest
  | '<' :: '=' :: rest -> LEQ :: tokenise rest
  | '>' :: rest -> GT :: tokenise rest
  | '<' :: rest -> LT :: tokenise rest
  | '!' :: rest -> NOT :: tokenise rest
  | '&' :: '&' :: rest -> AND :: tokenise rest
  | '|' :: '|' :: rest -> OR :: tokenise rest
  
  | '+' :: rest -> ADD :: tokenise rest
  | '-' :: rest -> SUB :: tokenise rest
  | '*' :: rest -> MUL :: tokenise rest
  | '/' :: rest -> DIV :: tokenise rest
  
  | '(' :: rest -> LPAREN :: tokenise rest
  | ')' :: rest -> RPAREN :: tokenise rest
  
  | c :: rest ->
    if is_digit c then
      let digits, rest' = collect_digits is_digit (c :: rest) in
      match rest' with
      | '.' :: xs ->
          let right_digits, others = collect_digits is_digit xs in
          FLOAT
            (float_of_string
               (String.of_seq (List.to_seq (digits @ ('.' :: right_digits)))))
          :: tokenise others
      | _ -> INT (int_of_string (String.of_seq (List.to_seq digits))) :: tokenise rest'
    else
      failwith (Printf.sprintf "Unexpected character: %c" c)
