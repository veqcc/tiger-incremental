structure Tree :
sig
  datatype binop = PLUS | MINUS
  datatype exp = CONST of int
               | BINOP of binop * exp * exp
end =

struct
  datatype binop = PLUS | MINUS
  datatype exp = CONST of int
               | BINOP of binop * exp * exp
end
