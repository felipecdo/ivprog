structure BooleanConverter = struct
  (* melhorar estas condições *)  
  fun fromString "verdadeiro" = SOME(true)
  	| fromString "falso" = SOME(false)
  	| fromString _ = NONE

  fun toString true = "verdadeiro"
  	| toString false = "falso"
end