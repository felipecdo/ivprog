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
            if IVProgTokens.isEOF next_token
            then []
            else next_token :: dowork(#3 lex_result)
          end
      in
        dowork(initial_strm)
      end

    fun tok2s tok = IVProgTokens.toString tok

    fun string_to_ast(inputString: string) =
      let
        val sm = AntlrStreamPos.mkSourcemap()
        val strm = IVProgLexer.streamifyInstream (TextIO.openString inputString)
        val lexer = IVProgLexer.lex (sm)
        val (r, strm', errs) = IP.parse lexer strm
      in
        print (String.concatWith "\n"
          (List.map (AntlrRepair.repairToString tok2s sm)
            errs));
        r
      end

    fun file_to_ast(file: string) =
      let
        val sm = AntlrStreamPos.mkSourcemap()
        val strm = IVProgLexer.streamifyInstream (TextIO.openIn file)
        val lexer = IVProgLexer.lex (sm)
        val (r, strm', errs) = IP.parse lexer strm
      in
        case r of
          SOME(env) => IVProgProcessor.inicializa(env)
          | NONE => print "Deu merda"
      end

  end