%name IvProgLexer;
%let digit = [0-9];
%let alpha = [a-zA-Z];
%let characters = ".*";
%let boolean = verdadeiro|falso;
(*number*)
%let natural = {digit}+;
%let integer = [(\-)]{natural};
%let real = {integer}\.{natural};
%let number = {integer}|{real};
(*id*)
%let identifier = (_|{alpha})(_|{alpha}|{digit});
(*operators*)
%let binary_boolean_operator = T.AND|T.OR;
%let unary_boolean_operator = T.NOT;
%let relational_operator = T.GREATER|T.LOWER|T.GE|T.LE|T.NEQ|T.EQ;
%let arithmetic_operator = T.PLUS|T.MINUS|T.TIMES|T.DIV|T.MOD;
%let boolean_operator = {relational_operator}|{binary_boolean_operator};
(*other rules*)
%let data_type = inteiro|real|booleano|texto;
%let literal = {number}|{identifier}|{boolean}|{characters}|vazio;
%defs (
structure T = IvProgTokens
type lex_result = T.token
fun eof() = T.EOF
);
"==" => ( T.EQ );
"<>" => ( T.NEQ );
"+" => ( T.PLUS );
"-" => ( T.MINUS );
"*" => ( T.TIMES );
"/" => ( T.DIV );
"%" => ( T.MOD );
">" => ( T.GREATER );
"<" => ( T.LOWER );
">=" => ( T.GE );
"<=" => ( T.LE );
"e" => ( T.AND );
"ou" => ( T.OR );
"nao" => ( T.NOT );
"(" => ( T.LP );
")" => ( T.RP );
" " | \n | \t
=> ( continue() );
. => ( print "Error during lexical process" );

