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
		|	NONE => (print("Searching for: "^id^"\nOptions: ["^Ast.concatList(StringMap.listKeys((!m)))^"\n");raise Exceptions.VariableNotDeclared("Variável não declarada: " ^ id))

	and updateStore(State(m,b), id, vl) = let 
		val _ = m := StringMap.insert((!m), id, vl)
	in
		State(m,b)
	end

	and setReturned(State(m,b), vl) = State(m, vl)

	and isDeclared(State(m,b), id) = case StringMap.find((!m), id) of
			SOME(vl) => true
		|	NONE => false
		
	and checkType(A.KType(A.KInt), SVInt(a)) = true
		| checkType(A.KType(A.KReal), SVReal(a)) = true
		| checkType(A.KType(A.KBool), SVBool(a)) = true
		| checkType(A.KType(A.KText), SVTexto(a)) = true
		| checkType(A.KUnit(l), sv) = checkTypeList((List.map A.KType l),sv)
		| checkType(_, _) = false

	and checkTypeList([], _) = false
		| checkTypeList(hd::tl, sv) = checkType(hd, sv) orelse checkTypeList(tl,sv)

	and checkTypeInStore(state:Store, ktype:A.Type, id:string) =
		let
			val vl = applyStore(state, id)
		in checkType(ktype,vl)
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
    SVInt a => Int.toString(a)
    | SVReal a => Real.toString(a)
    | SVBool a => BooleanConverter.toString(a)
    | SVTexto a => a
    | Undefined => "undefined"
   

  and eq(SVInt(a), SVInt(b)) = a = b
  	| eq(SVReal(a), SVReal(b)) = Real.==(a,b)
  	| eq(SVTexto(a), SVTexto(b)) = String.compare(a,b) = EQUAL
  	| eq(SVBool(a),SVBool(b)) = a = b
  	| eq(_ , _) = false

  and asInt(SVInt(a)) = SOME(a)
  	| asInt(_) = NONE

  and asReal(SVReal(a)) = SOME(a)
  	| asReal(_) = NONE

  and asTexto(SVTexto(a)) = SOME(a)
  	| asTexto(_) = NONE

  and asBool(SVBool(a)) = SOME(a)
  	| asBool(_) = NONE

  and toAst(SVInt(a)) = A.IntConstant(a)
  	| toAst(SVReal(a)) = A.RealConstant(a)
  	| toAst(SVBool(a)) = A.BoolConstant(a)
  	| toAst(SVTexto(a)) = A.StringConstant(a)
  	| toAst(Undefined ) = raise Exceptions.IllegalState

  and gt(SVInt(a), SVInt(b)) = a > b
  	| gt(SVReal(a), SVReal(b)) = a>b
  	| gt(_ , _) = false
end