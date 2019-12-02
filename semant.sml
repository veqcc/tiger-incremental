structure Semant :
sig
  val transExp: Absyn.exp -> Tree.exp
end =

struct
  fun transExp(Absyn.IntExp i) = Tree.CONST i
end
