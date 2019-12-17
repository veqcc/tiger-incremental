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
        fun stm (T.SEQ (s1, s2)) =
            (stm(s1);
            stm(s2))
          | stm (T.EXP e) = exp(e)
          | stm (T.MOVE (T.MEM e1, e2)) =
              (exp(e1);
              exp(e2);
              sayln "  pop rdx";
              sayln "  pop rsi";
              sayln "  mov rdi, rbp";
              sayln "  sub rdi, rsi";
              sayln "  lea rax, [rdi]";
              sayln "  mov [rax], rdx";
              sayln "  push rax")
          | stm (T.MOVE (T.TEMP t, e)) =
              (exp(e);
              sayln "  pop rax";
              sayln ("  mov r1" ^ (Int.toString t) ^ ", rax"))
          | stm (T.LABEL label) =
              sayln ((Symbol.extractName label) ^ ":")
          | stm (T.JUMP (T.NAME name, _)) =
              sayln ("  jmp " ^ (Symbol.extractName name))
          | stm (T.CJUMP (r, e1, e2, l1, l2)) =
              (exp(T.RELOP(r, e1, e2));
              sayln "  pop rax";
              sayln "  cmp rax, 0";
              sayln ("  je " ^ (Symbol.extractName l2));
              sayln ("  jmp " ^ (Symbol.extractName l1)))

        and exp (T.CONST i) = (say "  push "; sayln(Int.toString i))
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
          | exp (T.RELOP (oper, left, right)) =
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
          | exp (T.ESEQ (s, e)) =
              (stm(s);
              sayln "  pop rax";
              exp(e))
          | exp (T.MEM e) =
              (exp(e);
              sayln "  pop rsi";
              sayln "  mov rdi, rbp";
              sayln "  sub rdi, rsi";
              sayln "  lea rax, [rdi]";
              sayln "  mov rax, [rax]";
              sayln "  push rax")
          | exp (T.TEMP t) =
              sayln ("  push r1" ^ (Int.toString t))

    in
      sayln ".intel_syntax noprefix";
      sayln ".global main";
      sayln "main:";
      sayln "  push rbp";
      sayln "  mov rbp, rsp";
      sayln "  sub rsp, 128";

      exp(s0);
      sayln "  pop rax";
      sayln "  mov rsp, rbp";
      sayln "  pop rbp";
      sayln "  ret";
      TextIO.flushOut outstream
    end
end
