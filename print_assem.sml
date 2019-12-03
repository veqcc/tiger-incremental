structure PrintAssem :
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
        fun exp(T.CONST i, d) = (indent d; say "mov rax "; sayln(Int.toString i))
    in
      sayln ".intel_syntax noprefix";
      sayln ".global main";
      sayln "main:";
      exp(s0, 2);
      sayln "  ret";
      TextIO.flushOut outstream
    end
end
