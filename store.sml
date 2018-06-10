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

	and applyStore(State(m,b), id) = case (StringMap.find((!m), id)) of
			SOME(vl) => vl
		|	NONE => (print(id);raise VariableNotDeclared("Variável não declarada: " ^ id))

	and updateStore(State(m,b), id, vl) = let 
		val _ = m := StringMap.insert((!m), id, vl)
	in
		State(m,b)
	end

	and setReturned(State(m,b), vl) = State(m, vl)

	and isDeclared(State(m,b), id) = case StringMap.find((!m), id) of
			SOME(vl) => true
		|	NONE => false
		
	and checkType(ktype:A.Type, vl:StoreSV) = case (vl, ktype) of
				(SVInt _, KInt) => true
			|	(SVReal _, KReal) => true
			|	(SVTexto _, KText) => true
			|	(SVBool _, KBool) => true
			| (_ , _) => false

	and checkTypeInStore(state:Store, ktype:A.Type, id:string) =
		let
			val vl = applyStore(state, id)
			val res = checkType(ktype,vl)
		in res
	end

	and cmpStoreType(state,id, vl) = case (applyStore(state,id),vl) of
		(SVInt _, SVInt _) => true
		| (SVReal _, SVReal _) => true
		| (SVTexto _, SVTexto _) => true
		| (SVBool _, SVBool _) => true
		| (_ , _) => false
	

	and typeToString stype = case stype of
    SVInt _ => "inteiro"
    | SVReal _ => "real"
    | SVBool _ => "booleano"
    | SVTexto _ => "texto"
    | Undefined => "undefined"

  and toString stype = case stype of
    SVInt a => "inteiro:"^Int.toString(a)
    | SVReal a => "real:"^Real.toString(a)
    | SVBool a => "booleano:"^Bool.toString(a)
    | SVTexto a => "texto:"^a
    | Undefined => "undefined"
   

  and eq(SVInt(a), SVInt(b)) = a = b
  	| eq(SVReal(a), SVReal(b)) = Real.==(a,b)
  	| eq(SVTexto(a), SVTexto(b)) = String.compare(a,b) = EQUAL
  	| eq(SVBool(a),SVBool(b)) = a = b
  	| eq(_ , _) = false
end