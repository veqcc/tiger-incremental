
type lexresult = Tokens.token
fun eof() = Tokens.EOF(0,0)

exception NotAnInt

fun getInt(optionInt : int option) = case optionInt of
  SOME(n) => n
  | _ => raise NotAnInt

%%

digits = [0-9]+;

%%

{digits} => (Tokens.INT(getInt (Int.fromString yytext), yypos, yypos + size yytext));
