structure Absyn =
struct
  type pos = int
  type symbol = Symbol.symbol

  datatype var = SimpleVar of symbol * pos
  datatype oper = PlusOp | MinusOp | TimesOp | DivideOp
                | EqOp | LtOp | LeOp | GtOp | GeOp

  datatype exp =
      IntExp of int
    | VarExp of var
    | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
    | LetExp of {vars: dec, body: exp, pos: pos}

  and dec = VarDec of {name: symbol, init: exp, pos: pos}
end
