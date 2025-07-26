Expression parser in OCaml
This project is a simple interpreter for a language that supports basic arithmetic and boolean expressions. It includes a lexer, a parser to build an Abstract Syntax Tree (AST), and an evaluator to compute the result of the parsed expressions.

Arithmetic Operations: Addition (+), Subtraction (-), Multiplication (*), Division (/).

Integer and Float Literals: e.g., 123, 4.6.

Boolean Logic: true, false, && (and), || (or), ! (not).

Relational Operators: ==, !=, >, <, >=, <=.

Parentheses for grouping expressions.

Unary Negation.

Building and Running:
This project uses dune for building.

Prerequisites
OCaml

Dune

Steps
Clone the repository:

git clone https://github.com/jabaadamia/Expression-parser
cd expression_parser

Build the project:

dune build

After a successful build, you can run the main executable. It will prompt you to enter an expression.

dune exec expression_parser

You will see a prompt. Type an expression and press Enter.

Demo:

expression_parser % dune exec expression_parser
Enter an expression: (2+2)*4--4 == 20 && !false || 1.5 > 3.0

---- AST ----
--- Boolean Expression ---
Or
  And
    Eq
      Sub
        Mul
          Paren
            Add
              Int(2)
              Int(2)
          Int(4)
        Neg
          Int(4)
      Int(20)
    Not
      False
  Gt
    Float(1.500000)
    Float(3.000000)
------
Result: true

