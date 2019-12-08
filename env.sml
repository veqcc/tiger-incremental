structure Env :
sig
  datatype entry = VarEntry of int
  val baseVenv: entry Symbol.table
end =

struct
  datatype entry = VarEntry of int
  val baseVenv = Symbol.empty
end
