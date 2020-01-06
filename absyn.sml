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
    | LetExp of {decs: dec list, body: exp, pos: pos}
    | IfExp of {test: exp, then': exp, else': exp option, pos: pos}
    | ForExp of {var: Symbol.symbol, low: exp, high: exp, body: exp, pos: pos}

  and dec = VarDec of {symbol: Symbol.symbol, init: exp, pos: pos}
end
