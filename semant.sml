structure Semant :
sig
  val transExp: Absyn.exp -> Tree.exp
end =

struct
  fun transExp(Absyn.IntExp i) = Tree.CONST i
    | transExp(Absyn.OpExp {left, oper, right, pos}) =
        let
          val leftExp = transExp left
          val rightExp = transExp right
        in
          case oper of
            Absyn.PlusOp   => Tree.BINOP (Tree.PLUS, leftExp, rightExp)
          | Absyn.MinusOp  => Tree.BINOP (Tree.MINUS, leftExp, rightExp)
          | Absyn.TimesOp  => Tree.BINOP (Tree.MUL, leftExp, rightExp)
          | Absyn.DivideOp => Tree.BINOP (Tree.DIV, leftExp, rightExp)
          | Absyn.EqOp     => Tree.RELOP (Tree.EQ, leftExp, rightExp)
          | Absyn.LtOp     => Tree.RELOP (Tree.LT, leftExp, rightExp)
          | Absyn.LeOp     => Tree.RELOP (Tree.LE, leftExp, rightExp)
          | Absyn.GtOp     => Tree.RELOP (Tree.LT, rightExp, leftExp)
          | Absyn.GeOp     => Tree.RELOP (Tree.LE, rightExp, leftExp)
  end
end
