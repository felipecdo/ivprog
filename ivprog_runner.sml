structure IVProgRunner = 
struct
	structure IP = IVProgParseFn(IVProgLexer)
	exception Erro

	fun tok2s tok = IVProgTokens.toString tok

	fun run(path:string) =
      let
        val sm = AntlrStreamPos.mkSourcemap()
        val strm = IVProgLexer.streamifyInstream (TextIO.openIn path)
        val lexer = IVProgLexer.lex (sm)
        val (r, strm', errs, _) = IP.parse lexer strm
      in
        case r of
          SOME(env) => IVProgProcessor.inicializa(env)
          | NONE => (print(String.concatWith "\n"
          (List.map (AntlrRepair.repairToString tok2s sm)
            errs));raise Erro)
      end
end