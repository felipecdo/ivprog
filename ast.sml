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

  datatype BlockEnv =  Env of Block StringMap.map ref * Block StringMap.map ref

  fun typeToString ktype = case ktype of
    KInt => "inteiro"
    | KReal => "real"
    | KBool => "booleano"
    | KText => "texto"
    | KUnit => "undefined"

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
  
end
