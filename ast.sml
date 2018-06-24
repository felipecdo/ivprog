structure
Ast = struct

  datatype BaseType =
      KInt
    | KReal
    | KText
    | KBool
    ;

  datatype Type = 
    KType of BaseType
    | KUnit of BaseType list
    ;

  fun baseTypeToString(bType) = case bType of
    KInt => "inteiro"
    | KReal => "real"
    | KBool => "booleano"
    | KText => "texto"
    ;

  fun typeToString ktype = case ktype of
    KType(t) => baseTypeToString t
    | KUnit(l) => List.foldl (fn (a,b) => if b = "" then baseTypeToString(a) else b^", "^baseTypeToString(a)) "" l
    ;
  
  type a = Type * string

  datatype Exp =
      IntConstant of int
    | StringConstant of string
    | RealConstant of real
    | BoolConstant of bool
    | Variable of string
    | InfixApp of Exp * string * Exp
    | CallFunc of string * Exp list
    | Neg of Exp
    ;

  datatype Commands = 
      Return of Exp
    | Attrib of string * Exp
    | Decl of string * Type * Exp
    | CallProc of string * Exp list
    | IfThenElse of Exp * Commands list * Commands list
    | While of Exp * Commands list
    | For of string * Exp * Exp * Commands list
    | Skip
    | LangCall of string
    ;

  datatype Block = 
      Function of string * Type * a list * Commands list
    | Procedure of string * a list * Commands list
    ;

  datatype BlockEnv = Env of Block StringMap.map ref * Block StringMap.map ref


  fun as_formal_param(t:Type, identificador:string) = (t, identificador)

  fun create_procedure(id:string,lista: a list,comandos: Commands list) = 
    let
      val bloco = Procedure(id,lista,comandos)
    in if validate_proc(comandos) then (id,bloco) else raise Exceptions.ProcedureReturn("O bloco "^id^" não pode conter o comando retorna.")
    end
  and validate_proc([]) = true
    | validate_proc(c::cs) = (case c of
      Return _ => false
      | IfThenElse(_,c1,c2) => validate_proc(c1) andalso validate_proc(c2) andalso validate_proc(cs)
      | While(_, c1) =>  validate_proc(c1) andalso validate_proc(cs)
      | For(_,_,_,c1) =>  validate_proc(c1) andalso validate_proc(cs)
      | _ => validate_proc(cs))

  and create_function(id:string, tipo:Type, lista: a list, comandos: Commands list) = 
    let
      val bloco = Function(id, tipo, lista, comandos)
    in if validate_fun(comandos) then (id,bloco) else raise Exceptions.FunctionMustReturn("O bloco "^id^" não garante retorno.")  
    end
  and validate_fun(c::cs) = (case c of
    Return _ => true
    | IfThenElse(_,_,c2) => validate_fun(c2) orelse validate_fun(cs)
    | _ => validate_fun(cs))
  | validate_fun([]) = false

  fun empty_env() = let
    val imprimir = Procedure("escreva",[(KUnit([KText,KInt,KBool,KReal]),"p1")],[LangCall("imprimir")])
    val ler = Function("leia",KType(KText),[],[LangCall("leia")])
    val asInt = Function("como_inteiro",KType(KInt),[(KUnit([KText,KReal]),"p1")],[LangCall("como_inteiro")])
    val asReal = Function("como_real",KType(KReal),[(KUnit([KText,KInt]),"p1")],[LangCall("como_real")])
    val asBool = Function("como_booleano",KType(KBool),[(KType(KText),"p1")],[LangCall("como_booleano")])
    val asText = Function("como_texto",KType(KText),[(KUnit([KInt,KBool,KReal]),"p1")],[LangCall("como_texto")])
    val lista = [("escreva",imprimir),("leia",ler),("como_inteiro",asInt),
      ("como_real",asReal),("como_booleano",asBool),("como_texto",asText)
      ]
    val mLang = ref StringMap.empty
    fun addToEnv((id,bloco)) = mLang := StringMap.insert((!mLang),id,bloco)
    val _ = List.map addToEnv lista

  in
    Env((ref StringMap.empty), mLang)
  end 

  fun convert_env(Env(m,ml), []) = Env(m,ml)
    | convert_env(Env(m,ml), (id,bloco)::tl) = let
      val _ = m :=StringMap.insert((!m),id,bloco)
    in
      convert_env(Env(m,ml),tl)
    end

  fun isEnvDefined(Env(m,ml), id) = case StringMap.find((!m),id) of
      SOME(bloco) => true
    | NONE => case StringMap.find((!ml), id) of
      SOME(lang) => true
      | NONE => false

  fun applyEnv(Env(m,ml), id) = case StringMap.find((!m),id) of
      SOME(bloco) => bloco
    | NONE => case StringMap.find((!ml), id) of
      SOME(lang) => lang
      | NONE => (print("Searching for: "^id^"\nOptions: ["^concatList(StringMap.listKeys((!m)))^"\n");raise Exceptions.UndefinedBlock)

  and concatList([]) = "]"
        | concatList(h::t) = h^", "^concatList(t)

  fun expListToString([]) = ""
        | expListToString((exp)::t) = expToString(exp,0) ^", "^ expListToString(t)

  and expToString(exp, count) = case exp of
    IntConstant v => "IntConstant("^Int.toString(v)^")"
    | StringConstant  v => "StringConstant("^v^")"
    | RealConstant  v => "RealConstant("^Real.toString(v)^")"
    | BoolConstant  v => "BoolConstant("^Bool.toString(v)^")"
    | Variable  id => "Variable("^id^")"
    | InfixApp (exp1,str,exp2) => "InfixApp("^expToString(exp1, count+1)^", "^str^", "^expToString(exp2, count+1)^")"
    | CallFunc (str,expList) => "CallFunc("^str^", ["^expListToString(expList)^"])"
    | Neg  v => "Neg("^expToString(v, count+1)^")"
  
  fun commandListToString([]) = ""
        | commandListToString((cmd)::t) = commandToString(cmd) ^"; "^ commandListToString(t)

  and commandToString command = case command of
    Return exp => "Return: "^expToString(exp,0) 
    | Attrib (str,exp) => "Attrib: "^str^" <- "^expToString(exp,0)
    | Decl (str,decType,exp)  => (expToString(exp,0);"Decl: "^str^":"^typeToString(decType)^" <- "^expToString(exp, 0))
    | CallProc (str,expList)  => "CallProc: "^str^" ("^expListToString(expList)^")" 
    | IfThenElse (exp, cmdList1, cmdList2) => "IfThenElse: "^expToString(exp,0) 
                                            ^"\n\t\t\t true:  "^commandListToString(cmdList1)
                                            ^"\n\t\t\t false: "^commandListToString(cmdList2)
    | While (exp, cmdList) => "While: "^expToString(exp,0) 
                                       ^"\n\t\t\t true:  "^commandListToString(cmdList)
    | For (str, exp1, exp2, cmdList) => "For: "^expToString(exp1,0) ^"; "^expToString(exp2,0) 
                                             ^"\n\t\t\t true:  "^commandListToString(cmdList)
    | Skip => "Skip"
    | LangCall str => "LangCall: "^str

  fun printCmds([]) = true
        | printCmds((command)::t) = (print("\n\t\t"^commandToString(command));true) andalso printCmds(t)

  fun printParams([]) = true
        | printParams((tp,var)::t) = (print((var)^":"^typeToString(tp)^"; ");true)  andalso printParams(t)

  fun printBlock(Function(str,returnType,list,commands)) = 
          (print(str^" (funcao)\n\t retorno: "^typeToString(returnType)^"\n\t parametros: ");true) 
          andalso printParams(list) 
          andalso ((print("\n\t comandos: ");true)) 
          andalso printCmds(commands)
          andalso ((print("\n\n");true)) 
        | printBlock(Procedure(str,list,commands)) = 
          (print(str^" (procedure)\n\t parametros: ");true) 
          andalso printParams(list) 
          andalso ((print("\n\t comandos: ");true)) 
          andalso printCmds(commands) 
          andalso ((print("\n\n");true)) 

  fun printSomething(map:Block StringMap.map ref, h) = case StringMap.find((!map),h) of
      SOME(bloco) => printBlock(bloco)
    | NONE => true

  fun printListEnv([], map:Block StringMap.map ref) = true
        | printListEnv(h::t, map) = printSomething(map, h)  andalso printListEnv(t, map)
        
  fun printEnv(Env(m,ml)) =  printListEnv (StringMap.listKeys((!m)), m) 
    
end
