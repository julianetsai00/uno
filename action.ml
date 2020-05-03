(** [Action] determines the game command based on the parsed user input. *)
open Gamestate

(** Type [phrase] represents the phrase followed by the user's chosen command.*)
type phrase = string list 

(** Type [action] represents the a certain player action or game setting.  *)
type action =
  |Drop of phrase
  |Draw 
  |ChangeColor of phrase
  |ChangeCard of phrase
  |Player of phrase
  |Quit
  (**Game setting with added computer player *)
  |One 
  (**Game setting without added computer player *)
  |Two
  |OnePlayer
  |TwoPlayer
  |ThreePlayer
  |FourPlayer

exception Unavailable
exception CmdNotFound

(** [check_uno lst] is true if "uno" is typed in the player's command.  
    False otherwise. *)
let rec check_uno (lst) = 
  match lst with
  | [] -> false
  | h::t -> if String.lowercase_ascii h = "uno" then true else check_uno t 

(** [no_empties cmd_list acc] is [acc], which is [cmd_list] with all the "" 
    (empty) strings removed. 
    Returns [acc] if [cmd_list] is empty. *)
let rec no_empties cmd_list acc =
  match cmd_list with 
  |[] -> acc
  |h::t -> if h <> "" then no_empties t (h::acc) else no_empties t acc

(** [parse str] is the command of parsing [str].
    Raises: [CmdNotFound] if [str] is not a valid command.
            [Unavailable] if [str] is empty. 
    Requires: [str] contains only alphanumeric characters and spaces. *)
let parse str =
  let command = [] |> no_empties (String.split_on_char ' ' str) |> 
                List.rev in 
  match command with
  |h::t when String.lowercase_ascii h= "drop"-> Drop t
  |h::t when String.lowercase_ascii h="draw" && t = [] -> Draw
  |h::t when String.lowercase_ascii h="color" -> ChangeColor t
  |h::t when String.lowercase_ascii h="card" -> ChangeCard t
  |h::t when String.lowercase_ascii h = "player" -> Player t
  |h::t when String.lowercase_ascii h="quit" -> Quit
  |h::t when String.lowercase_ascii h = "yes" && t=[] -> One
  |h::t when String.lowercase_ascii h = "no" && t=[] -> Two
  |h::t when String.lowercase_ascii h = "1" && t=[] -> OnePlayer
  |h::t when String.lowercase_ascii h = "2" && t=[] -> TwoPlayer
  |h::t when String.lowercase_ascii h = "3" && t=[] -> ThreePlayer
  |h::t when String.lowercase_ascii h = "4" && t=[] -> FourPlayer
  |h::t -> raise(CmdNotFound)
  |[] -> raise(Unavailable)