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
          | indent i = (say " "; indent(i-1))
        fun exp(T.CONST i, d) = (indent d; say "CONST "; say(Int.toString i))
    in
      exp(s0, 0);
      sayln "";
      TextIO.flushOut outstream
    end
end
