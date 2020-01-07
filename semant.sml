structure Semant :
sig
  val transProg: Absyn.exp -> Tree.exp
end =

struct
  type venv = Env.entry Symbol.table
  val offset = ref 0

  fun seq (exps: Tree.stm list) =
    case exps of
      []      => Tree.EXP(Tree.CONST 0)
    | x :: [] => x
    | x :: xs => Tree.SEQ (x, seq xs)

  fun transExp venv =
    let
      fun trExp (Absyn.IntExp i) = Tree.CONST i
        | trExp (Absyn.VarExp var) = trVar var
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
              val bodyExp = transExp venv' body
            in
              Tree.ESEQ (seq exps, bodyExp)
            end
        | trExp (Absyn.IfExp {test, then', else', pos}) =
            (case (trExp test, trExp then', Option.map trExp else') of
              (testExp, thenExp, SOME elseExp) =>
                let
                  val t = Temp.newLabel ()
                  val f = Temp.newLabel ()
                  val j = Temp.newLabel ()
                  val r = Temp.newTemp ()
                in
                  Tree.ESEQ (
                    seq [
                      Tree.CJUMP (Tree.EQ, Tree.CONST 0, testExp, f, t),
                      Tree.LABEL t,
                      Tree.MOVE (Tree.TEMP r, thenExp),
                      Tree.JUMP (Tree.NAME j, [j]),
                      Tree.LABEL f,
                      Tree.MOVE (Tree.TEMP r, elseExp),
                      Tree.LABEL j
                    ],
                    Tree.TEMP r
                  )
                end
            | (testExp, thenExp, NONE) =>
                let
                  val t = Temp.newLabel ()
                  val f = Temp.newLabel ()
                  val j = Temp.newLabel ()
                  val r = Temp.newTemp ()
                in
                  Tree.ESEQ (
                    seq [
                      Tree.CJUMP (Tree.EQ, Tree.CONST 0, testExp, f, t),
                      Tree.LABEL t,
                      Tree.MOVE (Tree.TEMP r, thenExp),
                      Tree.JUMP (Tree.NAME j, [j]),
                      Tree.LABEL f,
                      Tree.MOVE (Tree.TEMP r, Tree.CONST 0),
                      Tree.LABEL j
                    ],
                    Tree.TEMP r
                  )
                end
              )
        | trExp (Absyn.WhileExp {test, body, pos}) =
            let
              val testExp = trExp test
              val bodyExp = trExp body
              val s = Temp.newLabel ()
              val b = Temp.newLabel ()
              val e = Temp.newLabel ()
            in
              Tree.ESEQ (
                seq [
                  Tree.LABEL s,
                  Tree.CJUMP (Tree.EQ, Tree.CONST 0, testExp, e, b),
                  Tree.LABEL b,
                  Tree.EXP bodyExp,
                  Tree.JUMP (Tree.NAME s, [s]),
                  Tree.LABEL e
                ],
                Tree.CONST 0
              )
            end
        | trExp (Absyn.ForExp {var, low, high, body, pos}) =
            let
              val loopEnd = Symbol.createSymbol "_LOOP_END"
              val decs = [
                Absyn.VarDec {symbol = var, init = low, pos = pos},
                Absyn.VarDec {symbol = loopEnd, init = high, pos = pos}
              ]
              val bodyExp =
                Absyn.WhileExp {
                  test = Absyn.OpExp {
                    left = Absyn.VarExp (Absyn.SimpleVar (var, pos)),
                    oper = Absyn.LeOp,
                    right = Absyn.VarExp (Absyn.SimpleVar (loopEnd, pos)),
                    pos = pos
                  },
                  body = Absyn.SeqExp [
                    (body, pos),
                    (Absyn.AssignExp {
                      var = Absyn.SimpleVar (var, pos),
                      exp = Absyn.OpExp {
                        left = Absyn.VarExp (Absyn.SimpleVar (var, pos)),
                        oper = Absyn.PlusOp,
                        right = Absyn.IntExp 1,
                        pos = pos
                      },
                      pos = pos
                    }, pos)
                  ],
                  pos = pos
                }
            in
              trExp (Absyn.LetExp {decs = decs, body = bodyExp, pos = pos})
            end
        | trExp (Absyn.SeqExp exps) =
            (case exps of
              [] => Tree.CONST 0
            | (e, _) :: es => Tree.ESEQ (Tree.EXP(trExp e), trExp (Absyn.SeqExp es))
            )
        | trExp (Absyn.AssignExp {var, exp, pos}) =
            Tree.ESEQ (
              Tree.MOVE (trVar var, trExp exp),
              Tree.CONST 0
            )
      and trVar (Absyn.SimpleVar (symbol, pos)) =
            (case Symbol.look (venv, symbol) of
              NONE =>
              (ErrorMsg.error pos ("undefined variable `" ^ Symbol.extractName symbol ^ "`");
              Tree.MEM (Tree.CONST 0))
            | SOME (Env.VarEntry access) =>
              Tree.MEM (Tree.CONST access)
            )
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
