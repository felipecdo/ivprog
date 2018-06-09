structure IVProgProcessor = struct

	exception NoMain of string

	fun inicializa(env:Ast.BlockEnv) = 
		let
			val bloco = Ast.applyEnv(env, "principal")
		in case bloco of
			Ast.Procedure(id,[],comandos)  => print("Bloco " ^ id ^ "encontrado")
			| Ast.Procedure(_,hd::tl,_) => raise NoMain "Bloco principal definido incorretamente."
			| _ => raise NoMain "Bloco principal não definido"
		end

end