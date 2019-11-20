structure Tokens : TIGER_TOKENS =
struct
  type linenum = int
  type token = string
  fun INT(c, i, j) = "INT("^Int.toString(c)^") " ^ Int.toString(i)
  fun EOF(i, j) = "EOF " ^ Int.toString(i)
end
