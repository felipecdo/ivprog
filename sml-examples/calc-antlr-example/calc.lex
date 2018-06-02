%name CalcLexer;
%let digit = [0-9];
%let int = {digit}+;
%let alpha = [a-zA-Z];
%let id = {alpha}({alpha} | {digit})*;
%defs (
structure T = CalcTokens
type lex_result = T.token
fun eof() = T.EOF
);
let => ( T.KW_let );
in => ( T.KW_in );
{id} => ( T.ID yytext );
{int} => ( T.NUM (valOf (Int.fromString yytext)) );
"=" => ( T.EQ );
"+" => ( T.PLUS );
"-" => ( T.MINUS );
"*" => ( T.TIMES );
"(" => ( T.LP );
")" => ( T.RP );
" " | \n | \t
=> ( continue() );
. => ( print "Error during lexical process" );