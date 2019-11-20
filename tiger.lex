
type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

fun eof() = Tokens.EOF(0,0)

exception NotAnInt

fun getInt(optionInt : int option) = case optionInt of
  SOME(n) => n
  | _ => raise NotAnInt

%%

%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
digits = [0-9]+;

%%

{digits} => (Tokens.INT(getInt (Int.fromString yytext), yypos, yypos + size yytext));
