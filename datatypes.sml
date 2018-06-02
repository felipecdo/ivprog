structure DataTypes =
struct
datatype
         id = id of string
and
         digit = Int32.int
and
         letter = "A"|"a"|"B"|"b"|"C"|"c"|"D"|"d"|"E"|"e"|"F"|"f"|"G"|"g"|"H"|"h"|"I"|"i"|"J"|"j"|"K"|"k"|"L"|"l"|"M"|"m"|"N"|"n"|"O"|"o"|"P"|"p"|"Q"|"q"|"R"|"r"|"S"|"s"|"T"|"t"|"U"|"u"|"V"|"v"|"W"|"w"|"X"|"x"|"Y"|"y"|"Z"|"z"
and
         numbers = integer | real
and
         types = inteiro | booleano | texto | real
and
         arithmetic_operator = PLUS | MINUS | TIMES | DIV 
and      
         logical_operator = AND | OR 
and
         relational_operator = LT | GT | LEQ | GEQ 
and    
         equality_operator = EQ | NOTEQ
and
         unary_op = NOT
and 
         label = Label of id
               | LabelBeginEnd of (id * string * string)
               | NoneLabel
and
         expr = NoneExpr
              | ArithmeticBinaryOperation of (expr * arithmetic_operator * expr * int * int)
              | LogicalBinaryOperation of (expr * logical_operator * expr * int * int)
              | RelationalBinaryOperation of (expr * relational_operator * expr * int * int)
              | EqualityBinaryOperation of (expr * equality_operator * expr * int * int)
              | UnaryOperation of  (unary_op * expr * int * int)
              | CharLit of (string * int * int)
              | IntLit of (Int32.int * int * int)
              | StrLit of (string * int * int)
              | True of (int * int)
              | False of (int * int)
              | ArglessFnCall of (id * int * int)
              | FnCall of (id * expr list * int * int)
              | Name of (id * int * int)
              | ArrayDeref of (id * expr * int * int)
              | ActualArgument of (expr * int * int)
              | Checked of (csx_type * expr)
and
         csx_decl = Field of (csx_type * id * int * int)
                  | InitializedField of (csx_type * id * expr * int * int)
                  | Method of (csx_type * id * csx_decl list * csx_decl list * stmt list * int * int)
                  | MainMethod of (csx_decl list * stmt list * int * int)
                  | FormalArg of (csx_type * id * int * int)
                  | NoneDecl
and 
         csx_member_decls = MemberDecls of (csx_decl list * csx_decl list)
and      
         stmt = NoneStmt
              | Block of (csx_decl list * stmt list * int * int)
              | IfThen of (expr * stmt * int * int)
              | IfThenElse of (expr * stmt * stmt * int * int)
              | While of (expr * stmt * int * int)
              | LabeledWhile of (label * expr * stmt * int * int)
              | Asg of (expr * expr * int * int)
              | Read of (expr * int * int)
              | Reads of (stmt list * int * int)
              | Write of (expr * int * int)
              | Writes of (stmt list * int * int)
              | ArglessProcCall of (id * int * int)
              | ProcCall of (id * expr list * int * int)
              | Return of (expr * int * int)
              | Break of (label * int * int)
              | Continue of (label * int * int)
              | Exit of (Int32.int * int * int)
and 
         csx_program = Program of (id * csx_member_decls)

end;
