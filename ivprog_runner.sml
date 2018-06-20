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
      handle Exceptions.UndefinedBlock => (print("Chamada de bloco inexistente.");raise Erro)
          | Exceptions.ProcedureReturn a => (print(a);raise Erro)
          | Exceptions.FunctionMustReturn a => (print(a);raise Erro)
          | Exceptions.VariableNotDeclared a => (print(a);raise Erro)
          | Exceptions.NoMain a => (print(a);raise Erro)
          | Exceptions.InternalError => (print("Operação inesperada. Erro Interno.");raise Erro)
          | Exceptions.ProcedureAsExpression a => (print(a);raise Erro)
          | Exceptions.UnboundParameters a => (print(a);raise Erro)
          | Exceptions.IncompatibleType a  => (print(a);raise Erro)
          | Exceptions.UndeclaredVariable a => (print(a);raise Erro)
          | Exceptions.AlreadyDeclaredVariable a => (print(a);raise Erro)
          | Exceptions.IllegalState => (print("O programa atingiu um estado inválido.");raise Erro)
end