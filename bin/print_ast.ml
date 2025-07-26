open Ast

let indent n = String.make (n * 2) ' '

let rec string_of_arithm indent_level a_expr =
  string_of_arithm_node indent_level a_expr

and string_of_bool indent_level b_expr =
  string_of_bool_node indent_level b_expr

and string_of_arithm_node indent_level = function
  | IConst i -> Printf.sprintf "%sInt(%d)" (indent indent_level) i
  | FConst f -> Printf.sprintf "%sFloat(%f)" (indent indent_level) f
  | Neg e -> Printf.sprintf "%sNeg\n%s" (indent indent_level) (string_of_arithm (indent_level + 1) e)
  | Paren e -> Printf.sprintf "%sParen\n%s" (indent indent_level) (string_of_arithm (indent_level + 1) e)
  | Add (e1, e2) -> Printf.sprintf "%sAdd\n%s\n%s" (indent indent_level) (string_of_arithm (indent_level + 1) e1) (string_of_arithm (indent_level + 1) e2)
  | Sub (e1, e2) -> Printf.sprintf "%sSub\n%s\n%s" (indent indent_level) (string_of_arithm (indent_level + 1) e1) (string_of_arithm (indent_level + 1) e2)
  | Mul (e1, e2) -> Printf.sprintf "%sMul\n%s\n%s" (indent indent_level) (string_of_arithm (indent_level + 1) e1) (string_of_arithm (indent_level + 1) e2)
  | Div (e1, e2) -> Printf.sprintf "%sDiv\n%s\n%s" (indent indent_level) (string_of_arithm (indent_level + 1) e1) (string_of_arithm (indent_level + 1) e2)

and string_of_bool_node indent_level = function
  | True -> Printf.sprintf "%sTrue" (indent indent_level)
  | False -> Printf.sprintf "%sFalse" (indent indent_level)
  | Not e -> Printf.sprintf "%sNot\n%s" (indent indent_level) (string_of_bool (indent_level + 1) e)
  | BParen e -> Printf.sprintf "%sParen\n%s" (indent indent_level) (string_of_bool (indent_level + 1) e)
  | Or (e1, e2) -> Printf.sprintf "%sOr\n%s\n%s" (indent indent_level) (string_of_bool (indent_level + 1) e1) (string_of_bool (indent_level + 1) e2)
  | And (e1, e2) -> Printf.sprintf "%sAnd\n%s\n%s" (indent indent_level) (string_of_bool (indent_level + 1) e1) (string_of_bool (indent_level + 1) e2)
  | Eq (e1, e2) -> Printf.sprintf "%sEq\n%s\n%s" (indent indent_level) (string_of_arithm (indent_level + 1) e1) (string_of_arithm (indent_level + 1) e2)
  | Neq (e1, e2) -> Printf.sprintf "%sNeq\n%s\n%s" (indent indent_level) (string_of_arithm (indent_level + 1) e1) (string_of_arithm (indent_level + 1) e2)
  | Geq (e1, e2) -> Printf.sprintf "%sGeq\n%s\n%s" (indent indent_level) (string_of_arithm (indent_level + 1) e1) (string_of_arithm (indent_level + 1) e2)
  | Leq (e1, e2) -> Printf.sprintf "%sLeq\n%s\n%s" (indent indent_level) (string_of_arithm (indent_level + 1) e1) (string_of_arithm (indent_level + 1) e2)
  | Gt (e1, e2) -> Printf.sprintf "%sGt\n%s\n%s" (indent indent_level) (string_of_arithm (indent_level + 1) e1) (string_of_arithm (indent_level + 1) e2)
  | Lt (e1, e2) -> Printf.sprintf "%sLt\n%s\n%s" (indent indent_level) (string_of_arithm (indent_level + 1) e1) (string_of_arithm (indent_level + 1) e2)

let string_of_expr top_expr =
  match top_expr with
  | Arith a -> "--- Arithmetic Expression ---\n" ^ (string_of_arithm 0 a)
  | Bool b -> "--- Boolean Expression ---\n" ^ (string_of_bool 0 b)
