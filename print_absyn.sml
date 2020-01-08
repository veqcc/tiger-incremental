structure PrintAbsyn :
sig
  val print : TextIO.outstream * Absyn.exp -> unit
end =

struct
  structure A = Absyn
  fun print (outstream, e0) =
    let fun say s = TextIO.output(outstream, s)
        fun sayln s = (say s; say "\n")
        fun indent 0 = ()
          | indent i = (say " "; indent(i - 1))
        fun opname A.PlusOp = "PlusOp"
          | opname A.MinusOp = "MinusOp"
          | opname A.TimesOp = "TimesOp"
          | opname A.DivideOp = "DivideOp"
          | opname A.EqOp = "EqOp"
          | opname A.LtOp = "LtOp"
          | opname A.LeOp = "LeOp"
          | opname A.GtOp = "GtOp"
          | opname A.GeOp = "GeOp"
        fun dolist d f [a] = (sayln ""; f(a, d + 1))
          | dolist d f (a :: r) = (sayln ""; f(a, d + 1); say ","; dolist d f r)
          | dolist d f nil = ()

        fun exp(A.IntExp i, d) =
              (indent d; say "IntExp("; say(Int.toString i); say ")")
          | exp(A.VarExp v, d) =
              (indent d; say "VarExp("; var(v, 0); say ")")
          | exp(A.OpExp{left, oper, right, pos}, d) =
              (indent d; say "OpExp("; say(opname oper); sayln ",";
              exp(left, d + 1); sayln ","; exp(right, d + 1); say ")")
          | exp(A.LetExp{decs, body, pos}, d) =
              (indent d; say "LetExp([";
              dolist d dec decs; sayln "],";
              exp(body, d + 1); say ")")
          | exp(A.IfExp{test, then', else', pos}, d) =
              (indent d; sayln "IfExp(";
              exp(test, d + 1); sayln ",";
              exp(then', d + 1);
              case else' of
                NONE => ()
              | SOME e => (sayln ","; exp(e, d + 1));
              say ")")
          | exp(A.ForExp{var = var', low, high, body, pos}, d) =
              (indent d; sayln "ForExp(";
              indent (d + 1); say(Symbol.extractName var'); sayln ",";
              exp(low, d + 1); sayln ",";
              exp(high, d + 1); sayln ",";
              exp(body, d + 1); say ")")
          | exp(A.WhileExp{test, body, pos}, d) =
              (indent d; sayln "WhileExp(";
              exp(test, d + 1); sayln ",";
              exp(body, d + 1); say ")")
          | exp(A.AssignExp{var = var', exp = exp', pos}, d) =
              (indent d; sayln "AssignExp(";
              var(var', d + 1); sayln ",";
              exp(exp', d + 1); say ")")
          | exp(A.SeqExp lst, d) =
              (indent d; say "SeqExp[";
              dolist d exp (map #1 lst); say "]")
          | exp(A.CallExp{func, args, pos}, d) =
              (indent d; say "CallExp("; say (Symbol.extractName func); say ",[";
              dolist d exp args; say "])")

        and dec(A.VarDec{symbol, init, pos}, d) =
              (indent d; say "VarDec("; say(Symbol.extractName symbol);
              say ","; exp(init, 1); say ")")
          | dec(A.FunDec{symbol, params, body, pos}, d) =
              let
                fun f ({symbol, pos}, d) =
                  (indent d; say (Symbol.extractName symbol))
              in
                (indent d; say "FunDec([";
                dolist d f params; sayln "],";
                exp (body, d + 1); say ")")
              end

        and var(A.SimpleVar(symbol, pos), d) =
              (indent d; say "SimpleVar(";
              say(Symbol.extractName symbol); say ")")

    in
      exp(e0, 0);
      sayln "";
      TextIO.flushOut outstream
  end
end
