structure Tree =
struct
  datatype binop = PLUS | MINUS | MUL | DIV
  datatype relop = EQ | NE | LT | LE

  datatype stm = SEQ of stm * stm
               | LABEL of Temp.label
               | JUMP of exp * Temp.label list
               | CJUMP of relop * exp * exp * Temp.label * Temp.label
               | EXP of exp
               | MOVE of exp * exp

       and exp = CONST of int
               | BINOP of binop * exp * exp
               | RELOP of relop * exp * exp
               | ESEQ of stm * exp
               | MEM of exp
               | NAME of Temp.label
               | TEMP of Temp.temp
end
