structure Tree =
struct
  datatype binop = PLUS | MINUS | MUL | DIV
  datatype relop = EQ | LT | LE
  datatype exp = CONST of int
               | BINOP of binop * exp * exp
               | RELOP of relop * exp * exp
end
