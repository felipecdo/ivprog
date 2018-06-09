structure Store = 
struct

	exception VariableNotDeclared of string

	structure A = Ast

	datatype StoreSV = 
			SVInt of int
		| SVReal of real
		| SVTexto of string
		| SVBool of bool
		| Undefined

	datatype Store = State of StoreSV StringMap.map ref

	fun empty() = State(ref StringMap.empty)

	fun applyStore(State(m), id) = case (StringMap.find((!m), id)) of
			SOME(vl) => vl
		|	NONE => raise VariableNotDeclared("Variável não declarada: " ^ id)
		;

	fun updateStore(State(m), id, vl) = m := StringMap.insert((!m), id, vl)

	fun isDeclared(State(m), id) = case StringMap.find((!m), id) of
			SOME(vl) => true
		|	NONE => false
		;

	fun checkType(state:Store, ktype:A.Type, id:string) =
		let
			val vl = applyStore(state, id)
		in case (vl, ktype) of
				(SVInt _, KInt) => true
			|	(SVReal _, KReal) => true
			|	(SVTexto _, KText) => true
			|	(SVBool _, KBool) => true
			| (_ , _) => false
	end

end