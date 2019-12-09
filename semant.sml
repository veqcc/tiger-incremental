structure Semant :
sig
  val transProg: Absyn.exp -> Tree.exp
end =

struct
  type venv = Env.entry Symbol.table
  val offset = ref 0

  fun transExp venv =
    let
      fun trExp (Absyn.IntExp i) = Tree.CONST i
        | trExp (Absyn.VarExp (Absyn.SimpleVar (symbol, pos))) =
            (case Symbol.look (venv, symbol) of
              NONE =>
                (ErrorMsg.error pos ("undefined variable `" ^ Symbol.extractName symbol ^ "`");
                Tree.MEM (Tree.CONST 0))
              | SOME (Env.VarEntry access) =>
                Tree.MEM (Tree.CONST access)
            )
        | trExp (Absyn.OpExp {left, oper, right, pos}) =
            let
              val leftExp = trExp left
              val rightExp = trExp right
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
        | trExp (Absyn.LetExp {decs, body, pos}) =
            let
              fun reducer (dec, {venv, exps}) =
                let
                  val result = transDec venv dec
                in
                  {venv = #venv result, exps = exps @ #exps result}
                end
              val {venv = venv', exps} = List.foldl reducer {venv = venv, exps = []} decs
              fun seq (exps: Tree.stm list) =
                case exps of
                  []      => Tree.EXP(Tree.CONST 0)
                | x :: [] => x
                | x :: xs => Tree.SEQ (x, seq xs)
              val bodyExp = transExp venv' body
            in
              Tree.ESEQ (seq exps, bodyExp)
            end
    in
      trExp
    end

  and transDec venv =
    let
      fun trDec (Absyn.VarDec {symbol, init, pos}) =
        let
          val exp' = transExp venv init
          val _ = offset := !offset + 8;
          val venv' = Symbol.enter(venv, symbol, Env.VarEntry (!offset))
        in
          {venv = venv', exps = [Tree.MOVE(Tree.MEM(Tree.CONST (!offset)), exp')]}
        end
      in
        trDec
    end

  fun transProg exp = transExp Env.baseVenv exp
end
