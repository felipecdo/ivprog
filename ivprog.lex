structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (InTokens.toString l), ": ", e, "\n"
      ])

%%
%header (functor IVProgLexFun(structure Tokens: IVProg_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];  
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (Tokens.NUM (valOf (InTokens.fromString yytext), !pos, !pos));

"=="     => ( Tokens.EQ(!pos,!pos) );
"<>"     => ( Tokens.NEQ(!pos,!pos) );
"+"      => ( Tokens.PLUS(!pos,!pos) );
"-"      => ( Tokens.MINUS(!pos,!pos) );
"*"      => ( Tokens.TIMES(!pos,!pos) );
"/"      => ( Tokens.DIV(!pos,!pos) );
"%"      => ( Tokens.MOD(!pos,!pos) );
">"      => ( Tokens.GREATER(!pos,!pos) );
"<"      => ( Tokens.LOWER(!pos,!pos) );
">="     => ( Tokens.GE(!pos,!pos) );
"<="     => ( Tokens.LE(!pos,!pos) );
"e"      => ( Tokens.AND(!pos,!pos) );
"ou"     => ( Tokens.OR(!pos,!pos) );
"nao"    => ( Tokens.NOT(!pos,!pos) );
"("      => ( Tokens.LP(!pos,!pos) );
")"      => ( Tokens.RP(!pos,!pos) );
"verdadeiro" => ( Tokens.TRUE(!pos,!pos)  );
"false"      => ( Tokens.FALSE(!pos,!pos)  );

characters='.*';
boolean=Tokens.TRUE|Tokens.FALSE;

natural={digit}+;
integer=[(\-)]{natural};
real={integer}\.{natural};
number={integer}|{real};

identifier = (_|{alpha})(_|{alpha}|{digit});

binary_boolean_operator = Tokens.AND|Tokens.OR;
unary_boolean_operator = Tokens.NOT;
relational_operator = Tokens.GREATER|Tokens.LOWER|Tokens.GE|Tokens.LE|Tokens.NEQ|Tokens.EQ;
arithmetic_operator = Tokens.PLUS|Tokens.MINUS|Tokens.TIMES|Tokens.DIV|Tokens.MOD;
boolean_operator = {relational_operator}|{binary_boolean_operator};

data_type = inteiro|real|booleano|texto;
literal = {number}|{identifier}|{boolean}|{characters}|vazio;

{alpha}+ => (if yytext="print"
                 then Tokens.PRINT(!pos,!pos)
                 else Tokens.ID(yytext,!pos,!pos)
            );


"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());
