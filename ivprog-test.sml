structure IVProg : sig
	           val parse : unit -> unit
                 end = 
struct

(* 
 * We apply the functors generated from IVProg.lex and IVProg.grm to produce
 * the IVProgParser structure.
 *)

  structure IVProgLrVals =
    IVProgLrValsFun(structure Token = LrParser.Token)

  structure IVProgLex =
    IVProgLexFun(structure Tokens = IVProgLrVals.Tokens)

  structure IVProgParser =
    Join(structure LrParser = LrParser
	 structure ParserData = IVProgLrVals.ParserData
	 structure Lex = IVProgLex)

(* 
 * We need a function which given a lexer invokes the parser. The
 * function invoke does this.
 *)

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
	      TextIO.output(TextIO.stdOut,
			    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
       in IVProgParser.parse(0,lexstream,print_error,())
      end

(* 
 * Finally, we need a driver function that reads one or more expressions
 * from the standard input. The function parse, shown below, does
 * this. It runs the IVProgulator on the standard input and terminates when
 * an end-of-file is encountered.
 *)

  fun parse () = 
      let val lexer = IVProgParser.makeLexer (fn _ =>
                                               (case TextIO.inputLine TextIO.stdIn
                                                of SOME s => s
                                                 | _ => ""))
	  val dummyEOF = IVProgLrVals.Tokens.EOF(0,0)
	  val dummySEMI = IVProgLrVals.Tokens.SEMI(0,0)
	  fun loop lexer =
	      let val (result,lexer) = invoke lexer
		  val (nextToken,lexer) = IVProgParser.Stream.get lexer
		  val _ = case result
			    of SOME r =>
				TextIO.output(TextIO.stdOut,
				       "result = " ^ (Int.toString r) ^ "\n")
			     | NONE => ()
	       in if IVProgParser.sameToken(nextToken,dummyEOF) then ()
		  else loop lexer
	      end
       in loop lexer
      end

end (* structure IVProg *)
