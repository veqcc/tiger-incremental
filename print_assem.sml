structure PrintAssem :
sig
  val print : TextIO.outstream * Tree.exp -> unit
end =

struct
  structure T = Tree
  fun print (outstream, s0) =
    let fun say s = TextIO.output(outstream,s)
        fun sayln s = (say s; say "\n")

        (* stack machine *)
        fun exp(T.CONST i) = (say "  push "; sayln(Int.toString i))
          | exp(T.BINOP(oper, left, right)) =
              (exp(left);
              exp(right);
              sayln "  pop rdi";
              sayln "  pop rax";
              case oper of
                T.PLUS  => sayln "  add rax, rdi"
              | T.MINUS => sayln "  sub rax, rdi"
              | T.MUL   => sayln "  imul rax, rdi"
              | T.DIV   => (sayln "  cqo"; sayln "  idiv rdi");
              sayln "  push rax")
          | exp(T.RELOP(oper, left, right)) =
              (exp(left);
              exp(right);
              sayln "  pop rdi";
              sayln "  pop rax";
              sayln "  cmp rax, rdi";
              case oper of
                T.EQ => sayln "  sete al"
              | T.LT => sayln "  setl al"
              | T.LE => sayln "  setle al";
              sayln "  movzb rax, al";
              sayln "  push rax")

    in
      sayln ".intel_syntax noprefix";
      sayln ".global main";
      sayln "main:";
      exp(s0);
      sayln "  pop rax";
      sayln "  ret";
      TextIO.flushOut outstream
    end
end
