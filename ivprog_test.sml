structure IVProgTester =
  struct
    structure IP = IVProgParseFn(IVProgLexer)

    fun string_to_tokens(inputString: string): IVProgTokens.token list =
      let
        val initial_strm = IVProgLexer.streamifyInstream (TextIO.openString inputString)
        val lexer = IVProgLexer.lex (AntlrStreamPos.mkSourcemap())
        fun dowork(strm) =
          let
            val lex_result = lexer strm
            val next_token = #1 lex_result
          in
            if (next_token = IVProgTokens.EOF)
            then []
            else next_token :: dowork(#3 lex_result)
          end
      in
        dowork(initial_strm)
      end

    fun string_to_ast(inputString: string): Ast.Exp =
      let
        val strm = IVProgLexer.streamifyInstream (TextIO.openString inputString)
        val lexer = IVProgLexer.lex (AntlrStreamPos.mkSourcemap())
        val (r, strm', errs) = IP.parse lexer strm
      in
        (case r
          of SOME(exp) => exp
          |  _ => raise Exceptions.ParseError ("parse error on " ^ inputString))
      end

end