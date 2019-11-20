signature TIGER_TOKENS =
sig
  type linenum
  type token
  val INT: (int) * linenum * linenum -> token
  val EOF: linenum * linenum -> token
end
