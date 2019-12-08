structure Tree =
struct
  datatype binop = PLUS | MINUS | MUL | DIV
  datatype relop = EQ | LT | LE

  datatype stm = EXP of exp
               | MOVE of exp * exp

       and exp = CONST of int
               | BINOP of binop * exp * exp
               | RELOP of relop * exp * exp
               | ESEQ of stm * exp
               | MEM of exp
end
