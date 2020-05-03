(** [Play] executes the game engine. *)

open Uno
open Gamestate
open Action

(** [is_legal result] verifies that [result] is a valid GameState t. *)
let is_legal result =
  match result with 
  | Gamestate.Legal t -> 
    let _ = Unix.sleep 2 in t
  | Gamestate.Illegal -> let _ = Unix.sleep 1 in
    failwith "oopsy!"

(** [is_valid_card_option name] verifies that the [name] of a card 
    that a player chooses to turn their Free Card to is not a Free Card. *)
let is_valid_card_option name =
  let card = match_card_name_to_card name cards
  in if card_name card = "Free Card" then
    failwith "" else name 

(** [check st t] is true if the current player in st has two cards left, and if 
    "uno" is not typed in the player's command. False otherwise. *)
let check st t =
  if current_turn st = 1  && List.length (player_one st) = 2 
     && (check_uno t= false) then true else if
    current_turn st = 2  && List.length (player_two st) = 2 
    && (check_uno t= false) then true else if 
    current_turn st = 3  && List.length (player_three st) = 2 
    && (check_uno t= false) then true else if 
    current_turn st = 4  && List.length (player_four st) = 2 
    && (check_uno t= false) then true else if 
    current_turn st = 5  && List.length (vs_player st) = 2 
    && (check_uno t= false) then true else false  

(** [concats lst] is the concatenated string of the words in [lst]. *)
let rec concats lst = 
  match lst with 
  |[] -> " "
  |h::t -> h ^", " ^ concats t 

(** [check_one_card_left st] notifies current player if another player in the 
    game has one card left in the state [st] *)
let check_one_card_left st =
  if List.length (player_one st) = 1 then 
    let _ = print_string("Player one has one card left!\n"); in ()
  else if List.length (player_two st) = 1 then 
    let _ = print_string("Player two has one card left!\n");in ()
  else if List.length (player_three st) = 1 then 
    let _ = print_string("Player three has one card left!\n");in ()
  else if List.length (player_four st) = 1 then 
    let _ = print_string("Player four has one card left!\n");in ()
  else if List.length (vs_player st) = 1 then 
    let _ = print_string("Computer Player has one card left!\n");in ()
  else ()

(** [print_player_cards st] prints the current player cards in state [st] *)
let print_player_cards st=
  (print_string ("\n Here are your cards " ); 
   if current_turn st =1 then (print_string ("player one.\n"); 
                               print_string (concats (player_one st)));
   if current_turn st =2 then (print_string ("player two.\n");
                               print_string(concats (player_two st)));
   if current_turn st =3 then (print_string ("player three.\n");
                               print_string(concats (player_three st)));
   if current_turn st =4 then (print_string ("player four.\n");
                               print_string(concats (player_four st)));
   print_string ("\n The current card is " ^ current_card_name st );
   print_string ("\n You can either draw or drop one of your cards!\n");)

(**  [drop_check st] implements the drop call, and checks to see if 
     [t] is an appropiate card to stack on the current card in [st] if the stack 
     list of cards is not empty.*)
let rec drop_check st t=
  let card_name = String.concat " " (List.filter (fun x -> x<> "uno") t) in 
  if get_stack_cards st <> [] && current_card_special st = DT && 
     (List.mem card_name (whose_deck (current_turn st) st)) &&
     card_name_to_special card_name Uno.cards <> DT then add_stack_cards st 
                                                         |> is_legal |> repl 
  else if get_stack_cards st <> [] && current_card_number st = -6 && 
          (List.mem card_name (whose_deck (current_turn st) st)) &&
          card_name_to_special card_name Uno.cards <> WCDF then 
    add_stack_cards st |> is_legal |> repl
  else matching_card st t

(** [draw_check st] implements the draw feature when a player attempts
    to draw. Checks to see that the current [st] does not involve stacking. If 
    [st] does involve stacking the player who drew is automatically given the 
    stacked cards, else one card is added to their deck. *)
and draw_check st=
  if Gamestate.get_stack_cards st <> [] && 
     (current_card_special st = DT || current_card_number st = -6)
  then add_stack_cards st |> is_legal |> repl else 
    (print_string("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"); 
     draw st |> is_legal |> repl);

  (** [repl st] implements and updates the [st] of the game after every input
      command. *)
and repl st = 
  begin
    let _ = check_one_card_left st in 
    let _= check_win st in
    try 
      (match read_line () |> Action.parse with
       |Drop t -> drop_check st t
       |Draw -> draw_check st 
       |Quit -> exit 0
       |_ ->  print_string ("Invalid command! Try using draw or drop followed 
         by the card in your hand that you want to drop. \n"); repl st)
    with
    | Action.CmdNotFound -> 
      print_string ("Invalid command! Try using draw or drop followed by the 
      card in your hand that you want to drop. \n"); repl st
    | Action.Unavailable -> print_string ("Hey! Did you mean to leave that 
      blank? Try typing a command like draw or drop followed by the card in your 
  hand that you want to drop.\n"); repl st
  end 
(** [matching card st t] matches the type of the dropped card to a game 
    action. *)
and matching_card st t =
  begin 
    (let card_name = String.concat " " (List.filter (fun x -> x<> "uno") t) in 
     try
       print_string("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
       let s =(if (check st t = true) then (draw_two_uno card_name st|>is_legal) 
               else st) in 
       begin
         match (Uno.card_name_to_special card_name Uno.cards) with 
         | DT -> draw_two card_name s|> is_legal |> repl
         | Skip -> turn_skip card_name s|> is_legal |> repl
         | Reverse -> turn_reverse card_name s|> is_legal |> repl
         | WC ->  if (List.mem card_name (whose_deck (current_turn st) st))then
             (print_endline ("\n What color do you want to change to?");
              try 
                begin
                  match read_line () |> Action.parse with 
                  | ChangeColor t -> let color_name = String.concat " " t in 
                    wild_card color_name s|> is_legal |> repl
                  | Quit -> exit 0
                  | _ -> failwith "doesn't come here"
                end
              with _ -> print_string("\n Invalid color please try again. \n");
                print_endline("Try dropping the card again and picking a valid 
                color. \n"); repl st)
           else let _ = print_endline("...") in Illegal|> is_legal|>repl
         | WCDF -> if (List.mem card_name (whose_deck (current_turn st) st)) 
           then (print_endline ("\n What color do you want to change to?");
                 try 
                   begin
                     match read_line () |> Action.parse with 
                     | ChangeColor t -> let color_name = String.concat " " t in 
                       draw_four color_name s|> is_legal |> repl
                     | Quit -> exit 0
                     | _ -> failwith "doesn't come here"
                   end
                 with _ -> print_string("\n Invalid color please try again. 
                 \n");
                   print_endline("Try dropping the card again and picking a 
                   valid color. \n"); repl st)
           else let _ = print_endline("...") in Illegal|> is_legal|>repl 
         | NotSpecial ->  drop card_name s|> is_legal |> repl
         | WCSH -> if (List.mem card_name (whose_deck (current_turn s) s)) then 
             (print_endline ("\n Who do you want to swap hands with and what 
             color do you want to change to?");
              try 
                begin
                  match read_line()  |> Action.parse with 
                  | Player t -> 
                    let player_name = int_of_string (List.nth t 0) in
                    let color_name = (List.nth t 1)in 
                    let s' = swap_cycle player_name s color_name

                    in repl (is_legal 
                               (swap_hands s' (whose_deck (current_turn s') s') 
                                  (whose_deck (swapping s') s')))
                  | _-> failwith "no"
                end
              with _ -> print_string("\n Invalid player or color please try 
              again. \n");
                print_endline("Try dropping the card again and picking a valid 
                player and color . \n"); repl st)
           else let _ = print_endline("...") in Illegal|> is_legal|>repl 

         |Free -> if (List.mem card_name (whose_deck (current_turn s) s)) then 
             (print_endline ("\n What card do you want to change this to? 
             Please type 'card' followed by any card in the
             Uno deck");
              try 
                begin
                  match read_line () |> Action.parse with 
                  | ChangeCard t -> let card_name = String.concat " " t in 
                    let card = is_valid_card_option card_name in 
                    begin
                      match (Uno.card_name_to_special card Uno.cards) with 
                      | DT -> draw_two_free card_name s|> is_legal |> repl
                      | Skip -> turn_skip_free card_name s|> is_legal |> repl 
                      | Reverse -> turn_reverse_free card_name s|> is_legal |> 
                                   repl

                      | WC ->  
                        (print_endline ("\n What color do you want to change 
                        to?");
                         try 
                           begin
                             match read_line () |> Action.parse with 
                             | ChangeColor t -> let color_name = 
                                                  String.concat " " t in 
                               wild_card_free color_name s|> is_legal |> repl
                             | Quit -> exit 0
                             | _ -> failwith "doesn't come here"
                           end
                         with _ -> print_string("\n Invalid color please try 
                         again. \n");
                           print_endline("Try dropping the card again and 
                           picking a valid color. \n"); repl st)

                      | WCDF -> 
                        (print_endline ("\n What color do you want to change 
                        to?");
                         try 
                           begin
                             match read_line () |> Action.parse with 
                             | ChangeColor t -> let color_name = 
                                                  String.concat " " t in 
                               draw_four_free color_name s|> is_legal |> repl
                             | Quit -> exit 0
                             | _ -> failwith "doesn't come here"
                           end
                         with _ -> print_string("\n Invalid color please try 
                         again. \n");
                           print_endline("Try dropping the card again and 
                           picking a valid color. \n"); repl st)

                      | NotSpecial ->  free_card card s|> is_legal |> repl
                      | WCSH -> 
                        (print_endline ("\n Who do you want to swap hands with 
                        and what color do you want to change to?");
                         try 
                           begin
                             match read_line()  |> Action.parse with 
                             | Player t -> 
                               let player_name = int_of_string (List.nth t 0) in
                               let color_name = (List.nth t 1)in 
                               let s' = swap_cycle_free player_name s color_name
                               in 
                               repl (is_legal(swap_hands s' (whose_deck 
                                                               (current_turn s') 
                                                               s') 
                                                (whose_deck (swapping 
                                                               s') s')))
                             | _-> failwith "no"
                           end
                         with _ -> print_string("\n Invalid player or color 
                         please try again. \n");
                           print_endline("Try dropping the card again and 
                           picking a valid player and color . \n"); repl st)

                      | _ -> failwith ""
                    end

                  | Quit -> exit 0
                  | _ -> failwith "doesn't come here"
                end
              with _ -> print_string("\n Invalid card please try again. \n");
                print_endline("Try dropping any card in the Uno Deck that is 
                not a free card. \n"); repl st)
           else let _ = print_endline("...") in Illegal|> is_legal|>repl 
       end
     with
     |_-> print_string("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
       print_string("You must pick a card with matching number or color in your 
       deck.\n"); repl st)
  end

(**[color_select_wc s st t] allows the user to drop a wild card and change the 
   current color of the deck to one of the game colors of their choice, updating
   the state [s].  If they don't choose a valid color, they will be prompted to
   try again. *)
and color_select_wc s t st=
  (print_endline ("\n What color do you want to change to?");
   try 
     begin
       match read_line () |> Action.parse with 
       | ChangeColor t -> let color_name = String.concat " " t in 
         wild_card color_name s|> is_legal |> repl
       | Quit -> exit 0
       | _ -> failwith "doesn't come here"
     end
   with _ -> print_string("\n Invalid color please try again. \n");
     print_endline("Try dropping the card again and picking a valid 
                color. \n"); repl st)

(**[color_select_draw_four s st t] allows the user to drop a wild card draw 4
   and change the current color of the deck to one of the game colors of their 
   choice, updating the state [s].  If they don't choose a valid color, they 
   will be prompted to try again. *)
and color_select_draw_four s t st=
  (print_endline ("\n What color do you want to change to?");
   try 
     begin
       match read_line () |> Action.parse with 
       | ChangeColor t -> let color_name = String.concat " " t in 
         draw_four color_name s|> is_legal |> repl
       | Quit -> exit 0
       | _ -> failwith "doesn't come here"
     end
   with _ -> print_string("\n Invalid color please try again. \n");
     print_endline("Try dropping the card again and picking a valid 
                color. \n"); repl st)

(** [check_win st] ends the game if a player gets rid of all their 
    cards in [st].  Otherwise, it prints the current player's cards. *)
and check_win st =
  if List.length (player_one st) = 0 then 
    let _ = print_string("\nPlayer One wins!\n"); in exit 0
  else if List.length (player_two st) = 0 then 
    let _= print_string("\nPlayer Two wins!\n"); in exit 0
  else if List.length (player_three st) = 0 then 
    let _= print_string("\nPlayer Three wins!\n"); in exit 0
  else if List.length (player_four st) = 0 then 
    let _= print_string("\nPlayer Four wins!\n"); in exit 0
  else if List.length (vs_player st) = 0 then 
    let _= print_string("\nComputer Player wins!\n"); in exit 0
  else if current_turn st =5 then 
    vscomputer st (vs_player st) |> is_legal |> repl
  else 
    print_player_cards st

(** [start_game vers numplayer] starts the game with number of players equal
    to [numplayer], and a computer player if [vers] is 1, or without a computer 
    player if [vers] is 2.   *)
let rec start_game vers numplayer=
  let state = Gamestate.init vers numplayer in
  repl state

(** [vsmode numplayer] gets if the user wants to add a computer player to the 
    game or not.*)
let rec vsmode (numplayer:int) =
  ANSITerminal.(print_string [red]
                  "\nWould you like to add a computer player?\n"); 
  try 
    match read_line () |> Action.parse with
    |One-> start_game 1 (numplayer+1)
    |Two -> start_game 2 numplayer
    |_-> print_string ("Type either 'yes' or 'no' for if you want to add a 
    computer player"); vsmode numplayer
  with 
  |_-> print_string ("Type either 'yes' or 'no' for if you want to add a 
  computer player");vsmode numplayer 

(** [how_many ()] gets the number of human players that will be in the game. *)
let rec how_many ()=
  ANSITerminal.(print_string [red]
                  "\nHow many players do you have? Type in a number 1-4\n"); 
  try 
    match read_line () |> Action.parse with
    |OnePlayer ->  1 
    |TwoPlayer -> 2
    |ThreePlayer -> 3
    |FourPlayer -> 4
    |_->print_string("Type 1-4 for how many players you have."); how_many () 
  with 
  |_-> print_string ("Type 1-4 for how many players you have."); how_many ()

(** [main ()] informs the user that the game is about to begin and asks how
    many players will be playing and if they want to add a computer player.  *)
let rec main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Uno. This is made for up to four players.\n");
  print_endline ("\nUno Rules:\n");
  print_endline("\nGame Play: The first player is whoever you decide to be 
  player one. The game goes on to player two, three... etc depending on how many 
  players you choose to have, with finally the computer player going last, if 
  you decide to include one in your game.\n");
  print_endline("Each player takes turns trying to match their card to the 
  current card in the pile (at the beginning of the game a random 
  number/non-special card will be selected as the start card). In order to have 
  a matching card, the card you are trying to drop and the current card in the 
  pile must either match in color, number, or special capability 
  (i.e. Skip, Reverse, etc). You are only allowed to drop one card at a time, 
  and if you cannot drop a card you must draw from the deck. Drawing from the 
  deck finishes your turn, and you will see your new card in your hand when it 
  is your turn again.\n");
  print_endline("The game continues until a player has one card left. When 
  that player is dropping their second to last card they must follow their 
  drop command with the word uno or else they will have two cards added to 
  their deck (Note: Uno must be written every single time a player is trying to 
  drop their second to last card). Once a player drops their last card, they are
  they are declared the winner and the game is over.\n");
  print_endline("\nAction Cards: There are 7 action cards that all have 
  different abilities. They are as follows: \nReverse: Switches the order of 
  gameplay. If the game was currently going clockwise, the game will now go
  counterclockwise. This card can be played on top of a card that matches the 
  Reverse card by color or on top of another Reverse card. \nSkip: Skips the 
  next persons turn. This card can only be dropped on top of a card that matches
  the Skip card by color or on top of another Skip card (Note: the player being
  skipped cannot drop a skip onto the skip because their turn would have
  already been skipped). \nDraw Two: Possibly adds two cards to the next 
  player's deck, and the next player loses their turn. This card can only be 
  dropped on top of a card that matches the Draw Two card by color or on top of 
  another Draw Two card. Also please note that stacking on draw twos is 
  possible. This means that if a player drops a draw two and the next player has 
  a draw two, they can drop their draw two, which would therefore require the 
  third player to drop a draw two or else forfeit their turn and collect 4 
  cards. \nWild: This card allows the player who dropped it to change the color 
  to any valid color in the game (i.e. Red, Blue, Yellow, Green). If this card 
  is dropped the next player must drop a card that matches the color that the 
  player before them changed the color to. This card can be dropped on top of 
  any card. \nWild Draw Four: This card allows the player who dropped it to 
  change the color to any valid color in the game (i.e. Red, Blue, Yellow, 
  Green) and possibly adds 4 cards to the next player's hand, while also them
  losing their turn. Also please note that stacking on wild draw fours is 
  possible. This means that if a player drops a wild draw four and the next 
  player has a wild draw four, they can drop their wild draw four, which would 
  therefore require the third player to drop a wild draw four or else forfeit 
  their turn and collect 8 cards. This card can be dropped on top of any card.
  \nWild Card Swap Hands: This card allows you to swap hands with anyone of your
  choice, and change the color of play to a valid game color (as like in Wild
  Card). The card can be played on top of any card. When choosing the 
  player to swap with and the new color, format your response with
  'player *number* *color*', and make sure the color name is capital.
   For example, if I want to swap hands with player 3 and change the color to 
   red, I would type, 'player 3 Red.'
  \nFree: This card allows the player who dropped it to change the card to any
  not special card, or a special card that matches either the same 
  special attribute or color of the current card. When choosing your card,
  format your response with 'card *card_name'. For eexample, if I want to
  change my card to a Wild Card, I would type, 'card Wild Card'.
   This card can be dropped on top of any other card, 
   however this card cannot be used against stacking. 
  This means that a player cannot attempt to change this card to a draw two 
  or a wild draw four in order 
  to prevent them from collecting the currently stacked cards. If a player tries
  to, they will be given the currently stacked cards and lose their turn.\n");
  print_endline("\nGame Commands: \nWhen dropping a card: drop __________ 
  (i.e. drop Red Draw 2) Please note that you must write the card exactly how 
  you see it or else it will be invalid. \nWhen drawing a card: draw (and that's
  it!) \nWhen changing the color: color __________ (i.e. color Blue) Please note
  that you must write a valid color and the first letter in the color should be
  capitalized. \nWhen attempting to exit the game: quit (and that's it!) 
  \nWhen attempting to claim uno: drop ___________ uno (i.e. drop Red 3 uno)");
  let num_player = how_many () 
  in vsmode num_player


(* Execute the game engine. *)
let () = main ()