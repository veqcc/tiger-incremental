structure Absyn =
struct
  type pos = int

  datatype var = SimpleVar of Symbol.symbol * pos
  datatype oper = PlusOp | MinusOp | TimesOp | DivideOp
                | EqOp | LtOp | LeOp | GtOp | GeOp

  datatype exp =
      IntExp of int
    | VarExp of var
    | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
    | LetExp of {varDec: dec, body: exp, pos: pos}

  and dec = VarDec of {symbol: Symbol.symbol, init: exp, pos: pos}
end
