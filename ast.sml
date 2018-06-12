structure
Ast = struct

  exception UndefinedBlock

  datatype Type =
      KInt
    | KReal
    | KText
    | KBool
    | KUnit
    ;

  fun typeToString ktype = case ktype of
    KInt => "inteiro"
    | KReal => "real"
    | KBool => "booleano"
    | KText => "texto"
    | KUnit => "undefined"
  
  type a = Type * string

  datatype Exp =
      IntConstant of int
    | StringConstant of string
    | RealConstant of real
    | BoolConstant of bool
    | Unit
    | Variable of string
    | InfixApp of Exp * string * Exp
    | CallFunc of string * Exp list
    | Neg of Exp
    ;
    
  fun expListToString([]) = ""
        | expListToString((exp)::t) = expToString(exp,0) ^", "^ expListToString(t)

  and expToString(exp, count) =
  if count > 50 then "\n------------------------------------\n!!! ATENÇÃO LOOP DETECTADO !!! ATENÇÃO LOOP DETECTADO !!!\n------------------------------------\n"
  else case exp of
    IntConstant exp => "IntConstant("^Int.toString(exp)^")"
    | StringConstant  exp => "StringConstant("^exp^")"
    | RealConstant  exp => "RealConstant("^Real.toString(exp)^")"
    | BoolConstant  exp => "BoolConstant("^Bool.toString(exp)^")"
    | Unit => "Unit"
    | Variable  exp => "Variable("^exp^")"
    | InfixApp (exp1,str,exp2) => "InfixApp("^expToString(exp1, count+1)^", "^str^", "^expToString(exp2, count+1)^")"
    | CallFunc (str,expList) => "CallFunc("^expToString(exp, count+1)^", ["^expListToString(expList)^"])"
    | Neg  exp => "Neg("^expToString(exp, count+1)^")"

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
  
  fun commandListToString([]) = ""
        | commandListToString((cmd)::t) = commandToString(cmd) ^"; "^ commandListToString(t)

  and commandToString command = case command of
    Return exp => "Return: "^expToString(exp,0) 
    | Attrib (str,exp) => "Attrib: "^str^" <- "^expToString(exp,0)
    | Decl (str,decType,exp)  => (print("debug1");expToString(exp,0);print("debug3");"Decl: "^str^":"^typeToString(decType)^" <- "^expToString(exp, 0))
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

  datatype Block = 
      Function of string * Type * a list * Commands list
    | Procedure of string * a list * Commands list
    ;

  datatype BlockEnv = Env of Block StringMap.map ref * Block StringMap.map ref


  fun as_formal_param(t:Type, identificador:string) = (t, identificador)

  fun create_procedure(id:string,lista: a list,comandos: Commands list) = 
    let
      val bloco = Procedure(id,lista,comandos)
    in
      (id,bloco)
    end

  fun create_function(id:string, tipo:Type, lista: a list, comandos: Commands list) = 
    let
      val bloco = Function(id, tipo, lista, comandos)
    in
      (id,bloco)
    end

  fun empty_env() = let
    val imprimir = Procedure("escreva",[(KUnit,"p1")],[LangCall("imprimir")])
    val ler = Function("leia",KText,[],[LangCall("leia")])
    val asInt = Function("como_inteiro",KInt,[(KUnit,"p1")],[LangCall("como_inteiro")])
    val asReal = Function("como_real",KReal,[(KUnit,"p1")],[LangCall("como_real")])
    val asBool = Function("como_booleano",KBool,[(KUnit,"p1")],[LangCall("como_booleano")])
    val asText = Function("como_texto",KText,[(KUnit,"p1")],[LangCall("como_texto")])
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

  fun applyEnv(Env(m,ml), id) = case StringMap.find((!m),id) of
      SOME(bloco) => bloco
    | NONE => case StringMap.find((!ml), id) of
      SOME(lang) => lang
      | NONE => (print(id);raise UndefinedBlock)


  fun printCmds([]) = true
        | printCmds((command)::t) = (print("\n\t\t"^commandToString(command));true)  andalso printCmds(t)

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

  fun printList([], map:Block StringMap.map ref) = true
        | printList(h::t, map) = printSomething(map, h)  andalso printList(t, map)
        
  fun printEnv(Env(m,ml)) =  printList (StringMap.listKeys((!m)), m) 
    
end
