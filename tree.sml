structure Tree =
struct
  datatype binop = PLUS | MINUS | MUL | DIV
  datatype exp = CONST of int
               | BINOP of binop * exp * exp
end
