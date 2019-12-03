structure PrintTree :
sig
  val print : TextIO.outstream * Tree.exp -> unit
end =

struct
  structure T = Tree
  fun print (outstream, s0) =
    let fun say s =  TextIO.output(outstream,s)
        fun sayln s= (say s; say "\n")
        fun indent 0 = ()
          | indent i = (say " "; indent(i - 1))
        fun binop T.PLUS = say "PLUS"
          | binop T.MINUS = say "MINUS"
          | binop T.MUL = say "MUL"
          | binop T.DIV = say "DIV"

        fun exp(T.CONST i, d) = (indent d; say "CONST "; say(Int.toString i))
          | exp(T.BINOP(oper, left, right), d) =
              (indent d; say "BINOP("; binop oper; sayln ",";
              exp(left, d + 1); sayln ","; exp(right, d + 1); say ")")
    in
      exp(s0, 0);
      sayln "";
      TextIO.flushOut outstream
    end
end
