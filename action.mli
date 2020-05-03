type phrase = string list 
type action =
  |Drop of phrase
  |Draw
  |ChangeColor of phrase
  |ChangeCard of phrase
  |Player of phrase
  |Quit
  |One
  |Two
  |OnePlayer
  |TwoPlayer
  |ThreePlayer
  |FourPlayer

exception Unavailable
exception CmdNotFound

val check_uno: string list-> bool
val no_empties: string list-> string list -> string list
val parse: string -> action