(* Missing check *)
%name IVProgLexer;

%let digit = [0-9];
%let int = {digit}+;(* Ainda nao suporta negativo *)
%let real = {int}"."{digit}+;(* Ainda nao suporta negativo *)
%let letter = [a-zA-Z];
%let id = (_|{letter})(_|{letter}|{digit})*;
%let relational_op = ("<" | ">" | "<=" | ">=" | "==" | "<>")+;
%let sum_op = ("+" | "-" | "ou")+;
%let multi_op = ("*" | "/" | "e" | "%")+;
%let boolean = "verdadeiro"|"falso";

%states CON_STRING;

%defs (
  structure T = IVProgTokens
  structure BoolConv = BooleanConverter
  type lex_result = T.token
  fun eof() = T.EOF
  val stringbuf = ref "";
);
(* <start state list> regular expression => ( code ); *)
<INITIAL> bloco => ( T.KW_bloco );
<INITIAL> se => ( T.KW_if );
<INITIAL> senao => ( T.KW_else );

<INITIAL> inteiro => ( T.KW_int );
<INITIAL> real => ( T.KW_real );
<INITIAL> texto => ( T.KW_texto );
<INITIAL> booleano => ( T.KW_booleano );
<INITIAL> retorna => ( T.KW_return );


<INITIAL> enquanto => ( T.KW_while );
<INITIAL> para => ( T.KW_for );
<INITIAL> de => ( T.KW_from );
<INITIAL> ate => ( T.KW_until );
<INITIAL> nao => ( T.Not );

<INITIAL> {relational_op} => ( T.RELATIONAL_OP yytext );
<INITIAL> {sum_op} => ( T.SUM_OP yytext );
<INITIAL> {multi_op} => ( T.MULTI_OP yytext );

<INITIAL> -{int} => ( T.CON_int((valOf (Int.fromString yytext))) );
<INITIAL> {int} => ( T.CON_int (valOf (Int.fromString yytext)) );
<INITIAL> -{real} => ( T.CON_real (valOf (Real.fromString yytext)) );
<INITIAL> {real} => ( T.CON_real (valOf (Real.fromString yytext)) );


(* Não funciona e nao tenho ideia do motivo *)
<INITIAL> {boolean} => ( T.CON_boolean (valOf (BoolConv.fromString yytext)) );
<INITIAL> {id} => ( T.ID yytext );

<INITIAL> "(" => ( T.LP );
<INITIAL> ")" => ( T.RP );
<INITIAL> "{" => ( T.LC );
<INITIAL> "}" => ( T.RC );
<INITIAL> ";" => ( T.SEMI );
<INITIAL> "," => ( T.COMMA );
<INITIAL> "=" => ( T.EQ );
<INITIAL> " " | \r | \n | \t => ( continue() );
<INITIAL> "#" [^\r\n]* \n => ( skip() );
<INITIAL> "\"" => ( YYBEGIN(CON_STRING); stringbuf := ""; continue() );
<CON_STRING> "\"" => ( YYBEGIN(INITIAL); T.CON_string(!stringbuf) );
<CON_STRING> [^"]* => ( stringbuf := (!stringbuf ^ yytext); continue() );
