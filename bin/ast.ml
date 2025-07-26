type expr = 
  | Arith of arithm_expr 
  | Bool of bool_expr 
and arithm_expr = 
  | IConst of int 
  | FConst of float 
  | Neg of arithm_expr 
  | Paren of arithm_expr 
  | Add of arithm_expr * arithm_expr 
  | Sub of arithm_expr * arithm_expr 
  | Mul of arithm_expr * arithm_expr 
  | Div of arithm_expr * arithm_expr 
and bool_expr = 
  | False 
  | True 
  | Not of bool_expr 
  | BParen of bool_expr 
  | Or of bool_expr * bool_expr 
  | And of bool_expr * bool_expr 
  | Eq of arithm_expr * arithm_expr 
  | Neq of arithm_expr * arithm_expr 
  | Geq of arithm_expr * arithm_expr 
  | Leq of arithm_expr * arithm_expr 
  | Lt of arithm_expr * arithm_expr 
  | Gt of arithm_expr * arithm_expr
