structure BooleanConverter = struct
  (* melhorar estas condições *)  
  fun fromString(s) = if s = "verdadeiro" then SOME(true) else SOME(false)
end