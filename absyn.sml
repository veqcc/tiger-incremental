structure Absyn =
struct
  type pos = int

  datatype exp =
      IntExp of int
    | OpExp of {left: exp, oper: oper, right: exp, pos: pos}

  and oper =
      PlusOp
    | MinusOp
    | TimesOp
    | DivideOp
    | EqOp
    | LtOp
    | LeOp
    | GtOp
    | GeOp
end
