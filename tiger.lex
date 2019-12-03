
type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos, pos) end

exception NotAnInt

fun getInt(optionInt : int option) = case optionInt of
  SOME(n) => n
  | _ => raise NotAnInt

%%

%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
digits = [0-9]+;

%%

"+"          => (Tokens.PLUS (yypos, yypos + 1));
"-"          => (Tokens.MINUS (yypos, yypos + 1));
{digits}     => (Tokens.INT(getInt (Int.fromString yytext), yypos, yypos + size yytext));
"\n"         => (lineNum := !lineNum + 1; linePos := yypos :: !linePos; continue());
(" "|\t|\r)  => (continue());
.            => (ErrorMsg.error yypos ("illegal character '" ^ yytext ^ "'"); continue());
