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
        fun exp(A.IntExp i, d) = (indent d; say "IntExp("; say(Int.toString i); say ")")
    in
      exp(e0, 0);
      sayln "";
      TextIO.flushOut outstream
  end
end
