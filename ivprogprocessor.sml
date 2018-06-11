structure IVProgProcessor = struct

	exception NoMain of string
	exception InternalError
	exception ProcedureAsExpression of string
	exception ProcedureReturn of string
	exception UnboundParameters of string
	exception IncompatibleType of string
	exception UndeclaredVariable of string
	exception AlreadyDeclaredVariable of string

	fun inicializa(env:Ast.BlockEnv) = 
		let
			val bloco = Ast.applyEnv(env, "principal")
			val store = Store.empty()
		in case bloco of
			Ast.Procedure(id,[],comandos)  => chama_procedure(comandos,[],[],(store,env))
			| Ast.Procedure(_,hd::tl,_) => raise NoMain "Bloco principal definido incorretamente."
			| _ => raise NoMain "Bloco principal não definido"
		end

	and avalia_expressao(Ast.IntConstant(a), (sto,env)) = Store.SVInt(a)
		| avalia_expressao(Ast.StringConstant(a), (sto,env)) = Store.SVTexto(a)
		| avalia_expressao(Ast.RealConstant(a), (sto,env)) = Store.SVReal(a)
		| avalia_expressao(Ast.BoolConstant(a), (sto,env)) = Store.SVBool(a)
		| avalia_expressao(Ast.Variable(a), (sto,env)) = Store.applyStore(sto,a)
		| avalia_expressao(Ast.Unit, (sto,env)) = Store.Undefined
		| avalia_expressao(Ast.Neg(exp), (sto,env)) = let
			val vl = avalia_expressao(exp, (sto,env))
			fun checkBool(Store.SVBool a) = SOME(Store.SVBool a)
				| checkBool(_) = NONE
			val ehBool = checkBool(vl)
		in
			case ehBool of
				SOME(Store.SVBool a) => Store.SVBool(not a)
				| NONE => raise IncompatibleType ("Tipo de dado resultante da expressão é incompatível.")
		end
		| avalia_expressao(Ast.CallFunc(id, exp), (sto,env)) = let
			val abst = Ast.applyEnv(env,id)
		in
			case abst of
				Ast.Procedure(_,_,_) => raise ProcedureAsExpression("O bloco "^id^" não retorna um valor.")
				| Ast.Function(_,ktipo,pfs,cs) => let
					val (sto1,_)  = chama_funcao(cs,ktipo,pfs,exp, (sto,env))
				in
					Store.applyStore(sto1,"$")
				end
		end
		| avalia_expressao(Ast.InfixApp(e0,"+",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVInt(a),Store.SVInt(b)) => Store.SVInt(a+b)
				| (Store.SVReal(a),Store.SVReal(b)) => Store.SVReal(a+b)
				| (Store.SVTexto(a),Store.SVTexto(b)) => Store.SVTexto(a^b)
				| (_,_) => let
					val tex1 = Store.toString(vl1)
					val tex2 = Store.toString(vl2)
					val _ = print(tex1^tex2)
				in
					raise IncompatibleType("Operação '+' inválida")
				end 
		end
		| avalia_expressao(Ast.InfixApp(e0,"-",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVInt(a),Store.SVInt(b)) => Store.SVInt(a-b)
				| (Store.SVReal(a),Store.SVReal(b)) => Store.SVReal(a-b)
				| (_,_) => raise IncompatibleType("Operação '-'' inválida")
		end
		| avalia_expressao(Ast.InfixApp(e0,"*",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVInt(a),Store.SVInt(b)) => Store.SVInt(a*b)
				| (Store.SVReal(a),Store.SVReal(b)) => Store.SVReal(a*b)
				| (_,_) => raise IncompatibleType("Operação '*'' inválida")
		end
		| avalia_expressao(Ast.InfixApp(e0,"/",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVInt(a),Store.SVInt(b)) => Store.SVInt(a div b)
				| (Store.SVReal(a),Store.SVReal(b)) => if Real.sign(b) = 0 then raise IncompatibleType("Divisão por 0") else
					Store.SVReal(a/b)
				| (_,_) => raise IncompatibleType("Operação '/' inválida")
		end
		| avalia_expressao(Ast.InfixApp(e0,"%",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVInt(a),Store.SVInt(0)) => raise IncompatibleType("Divisão por 0")
				| (Store.SVInt(a),Store.SVInt(b)) => Store.SVInt(a mod b)
				| (_,_) => raise IncompatibleType("Operação '%' inválida")
		end
		| avalia_expressao(Ast.InfixApp(e0,">",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVInt(a),Store.SVInt(b)) => Store.SVBool(a>b)
				| (Store.SVReal(a),Store.SVReal(b)) => Store.SVBool(a>b)
				| (_,_) => raise IncompatibleType("Operação '>' inválida")
		end
		| avalia_expressao(Ast.InfixApp(e0,"<",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVInt(a),Store.SVInt(b)) => Store.SVBool(a<b)
				| (Store.SVReal(a),Store.SVReal(b)) => Store.SVBool(a<b)
				| (_,_) => raise IncompatibleType("Operação '<' inválida")
		end
		| avalia_expressao(Ast.InfixApp(e0,">=",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVInt(a),Store.SVInt(b)) => Store.SVBool(a>=b)
				| (Store.SVReal(a),Store.SVReal(b)) => Store.SVBool(a>=b)
				| (_,_) => raise IncompatibleType("Operação '>=' inválida")
		end
		| avalia_expressao(Ast.InfixApp(e0,"<=",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVInt(a),Store.SVInt(b)) => Store.SVBool(a<=b)
				| (Store.SVReal(a),Store.SVReal(b)) => Store.SVBool(a<=b)
				| (_,_) => raise IncompatibleType("Operação '>=' inválida")
		end
		| avalia_expressao(Ast.InfixApp(e0,"==",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVInt(a),Store.SVInt(b)) => Store.SVBool(Store.eq(vl1,vl2))
				| (Store.SVReal(a),Store.SVReal(b)) => Store.SVBool(Store.eq(vl1,vl2))
				| (Store.SVTexto(a),Store.SVTexto(b)) => Store.SVBool(Store.eq(vl1,vl2))
				| (_,_) => let
					val tex1 = Store.toString(vl1)
					val tex2 = Store.toString(vl2)
					val _ = print(tex1^tex2)
				in
					raise IncompatibleType("Operação '==' inválida")
				end
		end
		| avalia_expressao(Ast.InfixApp(e0,"<>",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVInt(a),Store.SVInt(b)) => Store.SVBool(a<>b)
				| (Store.SVReal(a),Store.SVReal(b)) => Store.SVBool(Real.!=(a,b))
				| (_,_) => raise IncompatibleType("Operação '<>' inválida")
		end
		| avalia_expressao(Ast.InfixApp(e0,"e",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVBool(a),Store.SVBool(b)) => Store.SVBool(a andalso b)
				| (_,_) => raise IncompatibleType("Operação 'and' inválida")
		end
		| avalia_expressao(Ast.InfixApp(e0,"ou",e1), (sto,env)) = let
			val vl1 = avalia_expressao(e0,(sto,env))
			val vl2 = avalia_expressao(e1,(sto,env))
		in
			case (vl1,vl2) of
				(Store.SVBool(a),Store.SVBool(b)) => Store.SVBool(a orelse b)
				| (_,_) => raise IncompatibleType("Operação 'and' inválida")
		end
		| avalia_expressao(_, (sto,env)) = raise InternalError

	and associa_params([],[],novo, (old,env)) = novo
		| associa_params([],hd::tl,novo, (old,env)) = raise UnboundParameters "Quatidade de paramêtros não correspondem"
		| associa_params(hd::tl,[],novo, (old,env)) = raise UnboundParameters "Quatidade de paramêtros não correspondem"
		| associa_params( (tipo,id)::pfs, exp::pas, novo, (old,env)) = let
			val vl = avalia_expressao(exp,(old,env))
		in
			if Store.checkType(tipo,vl) then Store.updateStore(novo,id,vl) else 
				raise IncompatibleType ("Tipo do paramêtro diferente do esperado. Esperado: "^Ast.typeToString(tipo)^" Informado: "^Store.typeToString(vl))
		end

	and executa_comandos(c::cs,(sto,env)) = let
			val (Store.State(m,b),_) = executa_comando(c,(sto,env))
		in if b then (Store.State(m,b),env) else executa_comandos(cs,(sto,env)) end
		| executa_comandos([],(sto,env)) = (sto,env)

	and executa_comando(_,(Store.State(m,true), env)) = (Store.State(m,true), env)
		| executa_comando(Ast.CallProc(id, pas), (sto,env)) = let
			val abst = Ast.applyEnv(env,id)
		in case (abst) of
			Ast.Procedure(_,pfs,cs) => let
				val _ = chama_procedure(cs,pfs,pas,(sto,env))
			in
				(sto,env)
			end 
			| Ast.Function(_,ktipo,pfs,cs) => let
				val _ = chama_funcao(cs,ktipo,pfs,pas,(sto,env))
			in
				(sto,env)
			end 
		end

		| executa_comando(Ast.Attrib(id,exp),(sto,env)) = let
			val vl = avalia_expressao(exp,(sto,env))
		in
			if Store.cmpStoreType(sto,id,vl) then (Store.updateStore(sto,id,vl), env) else 
				raise IncompatibleType ("Tipo de dado resultante da expressão é incompatível com  "^id)
		end

		| executa_comando(Ast.Return(exp), (sto,env)) = let
			val vl = avalia_expressao(exp, (sto,env))
			val canReturn = Store.eq(Store.applyStore(sto,"$type"), Store.SVTexto("function"))

		in
			if canReturn then (Store.updateStore(Store.setReturned(sto,true),"$",vl), env) else
				raise ProcedureReturn("O bloco não espera por um retorno.")
		end

		| executa_comando(Ast.Decl(id,ktipo,exp), (sto,env)) = let
			val vl = avalia_expressao(exp,(sto,env))
			val exists = Store.isDeclared(sto, id)
		in
			if (Store.checkType(ktipo,vl) andalso not exists) then (Store.updateStore(sto,id,vl), env) else
				raise AlreadyDeclaredVariable("Variável "^id^"já definida ou tipo atribuído incompatível.")
		end

		| executa_comando(Ast.Skip, (sto,env)) = (sto,env)
		| executa_comando(Ast.IfThenElse(exp, c0, c1), (sto,env)) = let
			val vl = avalia_expressao(exp,(sto,env))
			fun checkBool(Store.SVBool a) = SOME(Store.SVBool a)
				| checkBool(_) = NONE
			val ehBool = checkBool(vl)
		in
			case ehBool of
				SOME(Store.SVBool a) => if a then executa_comandos(c0,(sto,env)) else executa_comandos(c1,(sto,env))
				| NONE => raise IncompatibleType ("O comando condicional espera como paramêtro uma expressão lógica, diferente da que foi informada")
		end

		| executa_comando(Ast.For(id,a,b,comandos), (sto,env)) = let
			val attrib = Ast.Attrib(id,Ast.IntConstant(a))
			val inc = if a > b then 1 else ~1
			val attribInc = Ast.Attrib(id, Ast.InfixApp(Ast.Variable(id), "+" , Ast.IntConstant(inc)))
			val compOp = if inc = 1 then "<=" else ">="
			val condWhile = Ast.InfixApp(Ast.Variable(id), compOp, Ast.IntConstant(b))
			val cWhile = Ast.While(condWhile,attribInc::comandos)
			val novaLista = [attrib,cWhile]
		in
			executa_comandos(novaLista,(sto,env))
		end

		| executa_comando(Ast.While(exp,cs),(sto,env)) = let
			val vl = avalia_expressao(exp,(sto,env))
			fun checkBool(Store.SVBool a) = SOME(Store.SVBool a)
				| checkBool(_) = NONE
			val ehBool = checkBool(vl)
		in
			case ehBool of
				SOME(Store.SVBool a) => if a then executa_comando(Ast.While(exp,cs),executa_comandos(cs,(sto,env))) else (sto,env)
				| NONE => raise IncompatibleType("O comando condicional espera como paramêtro uma expressão lógica, diferente da que foi informada")
		end
		| executa_comando(Ast.LangCall("imprimir"),(sto,env)) = let
			val vl = Store.applyStore(sto,"p1");
			val _ = print(Store.toString(vl)^"\n")
		in
			(sto,env)
		end

	and chama_procedure(comandos,pfs,pas,(sto,env)) = let
		val sto1 = associa_params(pfs,pas,Store.empty(), (sto,env))
		val _ = Store.updateStore(sto1,"$type",Store.SVTexto("procedure"))
	in executa_comandos(comandos,(sto1,env)) end

	and chama_funcao(comandos, ktipo, pfs,pas,(sto,env)) = let
		val sto1 = associa_params(pfs,pas,Store.empty(), (sto,env))
		val _ = Store.updateStore(sto1,"$type",Store.SVTexto("function"))
		val _ = executa_comandos(comandos,(sto1,env))
	in if Store.checkTypeInStore(sto1,ktipo, "$") then (sto1,env) else 
		raise IncompatibleType ("Tipo do retorno não compatível com o declarado. Declarado: "^Ast.typeToString(ktipo)^" Informado: "^Store.typeToString(Store.applyStore(sto1,"$"))) end

end
