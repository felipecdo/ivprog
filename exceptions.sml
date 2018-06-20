structure
Exceptions = struct

  exception NoMain of string
  exception InternalError
  exception ProcedureAsExpression of string
  exception ProcedureReturn of string
  exception UnboundParameters of string
  exception IncompatibleType of string
  exception UndeclaredVariable of string
  exception AlreadyDeclaredVariable of string
  exception IllegalState
  exception VariableNotDeclared of string
  exception UndefinedBlock
  exception ProcedureReturn of string
  exception FunctionMustReturn of string
  exception DuplicateBlock

end