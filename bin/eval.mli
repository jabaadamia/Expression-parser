type value =
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool
val eval_expr : Ast.expr -> value
val string_of_value : value -> string
