open Ast

type value =
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool

let string_of_value = function
  | IntVal i -> string_of_int i
  | FloatVal f -> string_of_float f
  | BoolVal b -> string_of_bool b

let rec eval_arithm a = eval_arithm_expr a
and eval_bool b = eval_bool_expr b

and eval_arithm_expr expr =
  match expr with
  | IConst i -> IntVal i
  | FConst f -> FloatVal f
  | Neg a ->
    (match eval_arithm a with
     | IntVal i -> IntVal (-i)
     | FloatVal f -> FloatVal (-.f)
     | _ -> failwith "Type error: Negation requires a number")
  | Paren a -> eval_arithm a
  | Add (a1, a2) ->
    (match eval_arithm a1, eval_arithm a2 with
     | IntVal i1, IntVal i2 -> IntVal (i1 + i2)
     | FloatVal f1, FloatVal f2 -> FloatVal (f1 +. f2)
     | _ -> failwith "Type error: Addition requires two numbers of the same type")
  | Sub (a1, a2) ->
    (match eval_arithm a1, eval_arithm a2 with
     | IntVal i1, IntVal i2 -> IntVal (i1 - i2)
     | FloatVal f1, FloatVal f2 -> FloatVal (f1 -. f2)
     | _ -> failwith "Type error: Subtraction requires two numbers of the same type")
  | Mul (a1, a2) ->
    (match eval_arithm a1, eval_arithm a2 with
     | IntVal i1, IntVal i2 -> IntVal (i1 * i2)
     | FloatVal f1, FloatVal f2 -> FloatVal (f1 *. f2)
     | _ -> failwith "Type error: Multiplication requires two numbers of the same type")
  | Div (a1, a2) ->
    (match eval_arithm a1, eval_arithm a2 with
     | IntVal i1, IntVal i2 -> IntVal (i1 / i2) 
     | FloatVal f1, FloatVal f2 -> FloatVal (f1 /. f2)
     | _ -> failwith "Type error: Division requires two numbers of thesame type")

and eval_bool_expr expr =
  match expr with
  | True -> BoolVal true
  | False -> BoolVal false
  | BParen b -> eval_bool b
  | Not b ->
    (match eval_bool b with
     | BoolVal v -> BoolVal (not v)
     | _ -> failwith "Type error: NOT requires a boolean")
  | And (b1, b2) ->
    (match eval_bool b1 with
     | BoolVal false -> BoolVal false 
     | BoolVal true -> eval_bool b2
     | _ -> failwith "Type error: AND requires booleans")
  | Or (b1, b2) ->
    (match eval_bool b1 with
     | BoolVal true -> BoolVal true 
     | BoolVal false -> eval_bool b2
     | _ -> failwith "Type error: OR requires booleans")
  | Eq (a1, a2) ->
    (match eval_arithm a1, eval_arithm a2 with
     | IntVal i1, IntVal i2 -> BoolVal (i1 = i2)
     | FloatVal f1, FloatVal f2 -> BoolVal (f1 = f2)
     | _ -> failwith "Type error: Equality requires two numbers of the same type")
  | Neq (a1, a2) ->
    (match eval_arithm a1, eval_arithm a2 with
     | IntVal i1, IntVal i2 -> BoolVal (i1 <> i2)
     | FloatVal f1, FloatVal f2 -> BoolVal (f1 <> f2)
     | _ -> failwith "Type error: Inequality requires two numbers of the same type")
  | Gt (a1, a2) ->
    (match eval_arithm a1, eval_arithm a2 with
    | IntVal i1, IntVal i2 -> BoolVal (i1 > i2)
    | FloatVal f1, FloatVal f2 -> BoolVal (f1 > f2)
    | _ -> failwith "Type error")
  | Lt (a1, a2) ->
    (match eval_arithm a1, eval_arithm a2 with
    | IntVal i1, IntVal i2 -> BoolVal (i1 < i2)
    | FloatVal f1, FloatVal f2 -> BoolVal (f1 < f2)
    | _ -> failwith "Type error")
  | Geq (a1, a2) ->
    (match eval_arithm a1, eval_arithm a2 with
    | IntVal i1, IntVal i2 -> BoolVal (i1 >= i2)
    | FloatVal f1, FloatVal f2 -> BoolVal (f1 >= f2)
    | _ -> failwith "Type error")
  | Leq (a1, a2) ->
    (match eval_arithm a1, eval_arithm a2 with
    | IntVal i1, IntVal i2 -> BoolVal (i1 <= i2)
    | FloatVal f1, FloatVal f2 -> BoolVal (f1 <= f2)
    | _ -> failwith "Type error")


let eval_expr expr =
  match expr with
  | Arith a -> eval_arithm_expr a
  | Bool b -> eval_bool_expr b
