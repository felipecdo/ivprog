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

	datatype Store = State of StoreSV StringMap.map ref * bool

	fun empty() = State(ref StringMap.empty,false)

	fun applyStore(State(m,b), id) = case (StringMap.find((!m), id)) of
			SOME(vl) => vl
		|	NONE => raise VariableNotDeclared("Variável não declarada: " ^ id)

	fun updateStore(State(m,b), id, vl) = let 
		val _ = m := StringMap.insert((!m), id, vl)
	in
		State(m,b)
	end

	fun setReturned(State(m,b), vl) = State(m, vl)

	fun isDeclared(State(m,b), id) = case StringMap.find((!m), id) of
			SOME(vl) => true
		|	NONE => false
		
	fun checkType(ktype:A.Type, vl:StoreSV) = case (vl, ktype) of
				(SVInt _, KInt) => true
			|	(SVReal _, KReal) => true
			|	(SVTexto _, KText) => true
			|	(SVBool _, KBool) => true
			| (_ , _) => false

	fun checkTypeInStore(state:Store, ktype:A.Type, id:string) =
		let
			val vl = applyStore(state, id)
			val res = checkType(ktype,vl)
		in res
	end

	fun cmpStoreType(state,id, vl) = case (applyStore(state,id),vl) of
		(SVInt _, SVInt _) => true
		| (SVReal _, SVReal _) => true
		| (SVTexto _, SVTexto _) => true
		| (SVBool _, SVBool _) => true
		| (_ , _) => false
	

	fun typeToString stype = case stype of
    SVInt _ => "inteiro"
    | SVReal _ => "real"
    | SVBool _ => "booleano"
    | SVTexto _ => "texto"
    | Undefined => "undefined"
   

  fun eq(SVInt(a), SVInt(b)) = a = b
  	| eq(SVReal(a), SVReal(b)) = Real.==(a,b)
  	| eq(SVTexto(a), SVTexto(b)) = a = b
  	| eq(SVBool(a),SVBool(b)) = a = b
  	| eq(_ , _) = false
end