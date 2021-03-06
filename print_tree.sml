structure PrintTree :
sig
  val print : TextIO.outstream * Tree.exp -> unit
end =

struct
  structure T = Tree
  fun print (outstream, s0) =
    let fun say s =  TextIO.output(outstream,s)
        fun sayln s = (say s; say "\n")
        fun indent 0 = ()
          | indent i = (say " "; indent(i - 1))
        fun binop T.PLUS = say "PLUS"
          | binop T.MINUS = say "MINUS"
          | binop T.MUL = say "MUL"
          | binop T.DIV = say "DIV"
        fun relop T.EQ = say "EQ"
          | relop T.LT = say "LT"
          | relop T.LE = say "LE"

        fun stm(T.MOVE(e1, e2),d) =
              (indent d; sayln "MOVE("; exp(e1, d + 1); sayln ",";
			        exp(e2, d + 1); say ")")
          | stm(T.EXP e, d) =
              (indent d; sayln "EXP("; exp(e, d + 1); say ")")
          | stm(T.SEQ (s1, s2), d) =
              (indent d; sayln "SEQ(";
              stm(s1, d + 1); sayln ",";
              stm(s2, d + 1); say ")")
          | stm(T.LABEL label, d) =
              (indent d; say "LABEL "; say (Symbol.extractName label))
          | stm(T.JUMP (e, _), d) =
              (indent d; sayln "JUMP("; exp(e, d + 1); say ")")
          | stm(T.CJUMP(r, e1, e2, l1, l2), d) =
              (indent d; say "CJUMP(";
              relop r; sayln ",";
              exp(e1, d + 1); sayln ",";
              exp(e2, d + 1); sayln ",";
              indent(d + 1); say(Symbol.extractName l1); say ",";
              say (Symbol.extractName l2); say ")")

        and exp(T.CONST i, d) = (indent d; say "CONST "; say(Int.toString i))
          | exp(T.BINOP(oper, left, right), d) =
              (indent d; say "BINOP("; binop oper; sayln ",";
              exp(left, d + 1); sayln ","; exp(right, d + 1); say ")")
          | exp(T.RELOP(oper, left, right), d) =
              (indent d; say "RELOP("; relop oper; sayln ",";
              exp(left, d + 1); sayln ","; exp(right, d + 1); say ")")
          | exp(T.ESEQ(s, e), d) =
              (indent d; sayln "ESEQ("; stm(s, d + 1); sayln ",";
              exp(e, d + 1); say ")")
          | exp(T.MEM(e), d) =
              (indent d; sayln "MEM("; exp(e, d + 1); say ")")
          | exp(T.NAME label, d) =
              (indent d; say "NAME "; say (Symbol.extractName label))
          | exp(T.TEMP temp, d) =
              (indent d; say "TEMP t"; say(Int.toString temp))
    in
      exp(s0, 0);
      sayln "";
      TextIO.flushOut outstream
    end
end
