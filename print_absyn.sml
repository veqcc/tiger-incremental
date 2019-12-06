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

        fun exp(A.IntExp i, d) =
              (indent d; say "IntExp("; say(Int.toString i); say ")")
          | exp(A.OpExp{left, oper, right, pos}, d) =
              (indent d; say "OpExp("; say(opname oper); sayln ",";
              exp(left, d + 1); sayln ","; exp(right, d + 1); say ")")
          | exp(A.LetExp{vars, body, pos}, d) =
              (indent d; sayln "LetExp(";
              dec(vars, d + 1); sayln ",";
              exp(body, d + 1); say ")")

        and dec(A.VarDec{name, init, pos}, d) =
              (indent d; say "VarDec("; say(Symbol.name name);
              say ","; exp(init, d + 1); say ")")

    in
      exp(e0, 0);
      sayln "";
      TextIO.flushOut outstream
  end
end
