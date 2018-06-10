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
    | For of string * int * int * Commands list
    | Skip
    | LangCall of string
    ;

  datatype Block = 
      Function of string * Type * a list * Commands list
    | Procedure of string * a list * Commands list
    ;

  datatype BlockEnv =  Env of Block StringMap.map ref

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
    val m = ref StringMap.empty
    val _ = m :=StringMap.insert((!m),"escreva",imprimir)
  in
    Env(m)
  end 

  fun convert_env(Env(m), []) = Env(m)
    | convert_env(Env(m), (id,bloco)::tl) = let
      val _ = m :=StringMap.insert((!m),id,bloco)
    in
      convert_env(Env(m),tl)
    end

  fun applyEnv(Env(m), id) = case StringMap.find((!m),id) of
      SOME(bloco) => bloco
    | NONE => raise UndefinedBlock
  
end
