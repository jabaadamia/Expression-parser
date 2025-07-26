open Ast
open Lexer

exception ParseError of string

let unwrap_arith ex = match ex with
  | Arith e -> e
  | Bool _ -> raise (ParseError ("Expected an arithmetic expression."))

let unwrap_bool ex = match ex with
  | Bool e -> e
  | Arith _ -> raise (ParseError ("Expected a boolean expression."))

let rec parse_or_expr tokens = parse_or_expr_tail tokens
and parse_and_expr tokens = parse_and_expr_tail tokens
and parse_relational_expr tokens = parse_relational_expr_tail tokens
and parse_additive_expr tokens = parse_additive_expr_tail tokens
and parse_multiplicative_expr tokens = parse_multiplicative_expr_tail tokens
and parse_unary_expr tokens = parse_unary_expr_tail tokens
and parse_primary_expr tokens = parse_primary_expr_tail tokens

(* lowest priority: OR *)
and parse_or_expr_tail tokens =
  let lhs, rest = parse_and_expr tokens in
  match rest with
  | OR :: rest' ->
      let rhs, rest'' = parse_or_expr_tail rest' in
      (Bool (Or (unwrap_bool lhs, unwrap_bool rhs)), rest'')
  | _ -> (lhs, rest)

(* next priority: AND *)
and parse_and_expr_tail tokens =
  let lhs, rest = parse_relational_expr tokens in
  match rest with
  | AND :: rest' ->
      let rhs, rest'' = parse_and_expr_tail rest' in
      (Bool (And (unwrap_bool lhs, unwrap_bool rhs)), rest'')
  | _ -> (lhs, rest)

(* relational Operators (==, !=, >, <, etc.) *)
and parse_relational_expr_tail tokens =
  let lhs, rest = parse_additive_expr tokens in
  match rest with
  | EQ :: rest' ->
      let rhs, rest'' = parse_additive_expr rest' in
      (Bool (Eq (unwrap_arith lhs, unwrap_arith rhs)), rest'')
  | NEQ :: rest' ->
      let rhs, rest'' = parse_additive_expr rest' in
      (Bool (Neq (unwrap_arith lhs, unwrap_arith rhs)), rest'')
  | GT :: rest' ->
      let rhs, rest'' = parse_additive_expr rest' in
      (Bool (Ge (unwrap_arith lhs, unwrap_arith rhs)), rest'')
  | LT :: rest' ->
      let rhs, rest'' = parse_additive_expr rest' in
      (Bool (Le (unwrap_arith lhs, unwrap_arith rhs)), rest'')
  | GEQ :: rest' ->
      let rhs, rest'' = parse_additive_expr rest' in
      (Bool (Geq (unwrap_arith lhs, unwrap_arith rhs)), rest'')
  | LEQ :: rest' ->
      let rhs, rest'' = parse_additive_expr rest' in
      (Bool (Leq (unwrap_arith lhs, unwrap_arith rhs)), rest'')
  | _ -> (lhs, rest)

(* +, - *)
and parse_additive_expr_tail tokens =
  let rec loop lhs_expr tokens =
    match tokens with
    | ADD :: rest ->
        let rhs_expr, rest' = parse_multiplicative_expr rest in
        let new_lhs = Arith (Add (unwrap_arith lhs_expr, unwrap_arith rhs_expr)) in
        loop new_lhs rest'
    | SUB :: rest ->
        let rhs_expr, rest' = parse_multiplicative_expr rest in
        let new_lhs = Arith (Sub (unwrap_arith lhs_expr, unwrap_arith rhs_expr)) in
        loop new_lhs rest'
    | _ -> (lhs_expr, tokens)
  in
  let lhs, rest = parse_multiplicative_expr tokens in
  loop lhs rest

(* multiplicarion division *, /, *)
and parse_multiplicative_expr_tail tokens =
  let rec loop lhs_expr tokens =
    match tokens with
    | MUL :: rest ->
        let rhs_expr, rest' = parse_unary_expr rest in
        let new_lhs = Arith (Mul (unwrap_arith lhs_expr, unwrap_arith rhs_expr)) in
        loop new_lhs rest'
    | DIV :: rest ->
        let rhs_expr, rest' = parse_unary_expr rest in
        let new_lhs = Arith (Div (unwrap_arith lhs_expr, unwrap_arith rhs_expr)) in
        loop new_lhs rest'
    | _ -> (lhs_expr, tokens)
  in
  let lhs, rest = parse_unary_expr tokens in
  loop lhs rest

(* unary Operators !, - *)
and parse_unary_expr_tail tokens =
  match tokens with
  | NOT :: rest ->
      let expr, rest' = parse_unary_expr rest in
      (Bool (Not (unwrap_bool expr)), rest')
  | SUB :: rest -> 
      let expr, rest' = parse_unary_expr rest in
      (Arith (Neg (unwrap_arith expr)), rest')
  | _ -> parse_primary_expr tokens

(* highest priority: Literals and Parentheses *)
and parse_primary_expr_tail tokens =
  match tokens with
  | INT n :: rest -> (Arith (IConst n), rest)
  | FLOAT f :: rest -> (Arith (FConst f), rest)
  | TRUE :: rest -> (Bool True, rest)
  | FALSE :: rest -> (Bool False, rest)
  | LPAREN :: rest ->
      let expr, rest' = parse_or_expr rest in
      (match rest' with
       | RPAREN :: rest'' ->
           let paren_expr = match expr with
             | Arith e -> Arith (Paren e)
             | Bool b -> Bool (BParen b)
           in
           (paren_expr, rest'')
       | _ -> raise (ParseError "Syntax error: Expected a closing parenthesis ')'"))
  | _ -> raise (ParseError "Syntax error: Unexpected token")

let build_ast tokens =
  let expr, rest = parse_or_expr tokens in
  match rest with
  | [] -> expr
  | OEF :: [] -> expr 
  | _ :: _ -> raise (ParseError ("Syntax error: Unexpected token after expression"))

