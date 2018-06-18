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
      handle Ast.UndefinedBlock => (print("Chamada de bloco inexistente.");raise Erro)
          | Ast.ProcedureReturn a => (print(a);raise Erro)
          | Ast.FunctionMustReturn a => (print(a);raise Erro)
          | Store.VariableNotDeclared a => (print(a);raise Erro)
          | IVProgProcessor.NoMain a => (print(a);raise Erro)
          | IVProgProcessor.InternalError => (print("Operação inesperada. Erro Interno.");raise Erro)
          | IVProgProcessor.ProcedureAsExpression a => (print(a);raise Erro)
          | IVProgProcessor.ProcedureReturn a => (print(a);raise Erro)
          | IVProgProcessor.UnboundParameters a => (print(a);raise Erro)
          | IVProgProcessor.IncompatibleType a  => (print(a);raise Erro)
          | IVProgProcessor.UndeclaredVariable a => (print(a);raise Erro)
          | IVProgProcessor.AlreadyDeclaredVariable a => (print(a);raise Erro)
          | IVProgProcessor.IllegalState => (print("O programa atingiu um estado inválido.");raise Erro)
end