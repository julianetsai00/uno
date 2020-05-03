(** [Gamestate] updates the state of the game throughout every player's turn. *)
open Uno

(** Type [t] represents the current state of the game.   *)
type t = 
  {
    (** A valid gamesetting for the option of adding computer player.  1 means
        an added computer player, 2 means no added computer player. *)
    gamestate:int;
    (** Number of players in the game, including computer player. *)
    numplayers: int;
    (** Direction of the game, either ascending order or descending order. *)
    direction: string;
    (** Current turn.*)
    turn: int;
    (** Current card on top of the deck.  *)
    current_card : card list;
    (** Player one's deck. *)
    player_one_cards : string list;
    (** Player two's deck. *)
    player_two_cards : string list;
    (**Player three's deck. *)
    player_three_cards : string list;
    (**Player four's deck. *)
    player_four_cards : string list;
    (**Computer Player's deck. *)
    computer_player_cards : string list;
    (**Currently stacked cards. *)
    stack_cards : string list;
    (**Current swapping status. *)
    swapping : int;
  }

(** [testing_initializer g n d t cc p1c p2c p3c p4c cpc] 
    is a mock state for the test file. *)
let testing_initializer g n d t cc p1c p2c p3c p4c cpc sc sw=
  {
    gamestate = g;
    numplayers =n;
    direction = d;
    turn =t;
    current_card= cc; 
    player_one_cards= p1c;
    player_two_cards =p2c; 
    player_three_cards= p3c; 
    player_four_cards= p4c; 
    computer_player_cards=cpc;
    stack_cards=sc;
    swapping=sw;
  }

(** [init vers numplayers] is the starting state of the game with all player 
    decksinitalized with seven random cards, and a random start card.  Default 
    first turn is 1, and direction is ascending.  The gamestate is [vers], and 
    number of players is [numplayers].*)
let init vers numplayers= 
  {
    gamestate= vers;
    numplayers= numplayers;
    direction = "asc";
    turn =1 ;
    current_card= start_card Uno.cards;
    player_one_cards = card_list_to_card_name_list (initial_cards Uno.cards);
    player_two_cards = card_list_to_card_name_list (initial_cards Uno.cards) ;
    player_three_cards = card_list_to_card_name_list (initial_cards Uno.cards);
    player_four_cards = card_list_to_card_name_list (initial_cards Uno.cards) ;
    computer_player_cards =card_list_to_card_name_list(initial_cards Uno.cards);
    stack_cards = [];
    swapping = 0;
  }

(** [current_direction st] is the current direction of [st]. "asc" if direction
    is in ascending order, "des" for descending order. *)
let current_direction st=
  st.direction

(** [numplayers st] is the number of players in [st]. If no computer player,
    maximum is 4 players.  With computer player, up to 5 players.*) 
let numplayers st=
  st.numplayers

(** [gamestate st] is the setting of the computer player of [st]. 1 if there is 
    an added computer player,2 if there is no added computer player  *)
let gamestate st=
  st.gamestate

(** [get_stack_cards st] is the current list of stacked cards in [st]. *)
let get_stack_cards st = 
  st.stack_cards

(** [current_turn st] is the current turn of [st]. Is either 1 or 2. *)
let current_turn st=
  st.turn

(** [current_card_name st] is the name of the current card in state [st]. *)
let current_card_name st=
  card_name (List.hd st.current_card)

(** [current_card_color st] is the color of the current card in state [st]. *)
let current_card_color st=
  card_color(List.hd st.current_card)

(** [current_card_number st] is the number of the current card in [st]. *)
let current_card_number st =
  card_number (List.hd st.current_card)

(** [current_card_special st] is the ability of the current card in state[st].*)
let current_card_special st=
  card_special (List.hd st.current_card)

(** [player_one st] are player one's cards in state [st]. *)
let player_one st= 
  st.player_one_cards

(** [player_two st] are player two's cards in state [st]. *)
let player_two st=
  st.player_two_cards

(** [player_three st] are player three's cards in state [st]. *)
let player_three st=
  st.player_three_cards

(** [player_four st] are player four's cards in state [st]. *)
let player_four st=
  st.player_four_cards

(** [vs_player st] are computer player's cards in state [st]. *)
let vs_player st =
  st.computer_player_cards

(** Type result is the outcome of a player comand.  Legal if player command was 
    valid, and Illegal otherwise. *)
type result = Legal of t | Illegal

(** [take_out_one card_name card_hand acc] is [acc], which is the [card_name] 
    with the first instance of [card_name] removed.*)
let rec take_out_one (card_name:string)(card_hand: string list)(acc)=
  match card_hand with 
  |h::t -> if h= card_name then acc@t else take_out_one card_name t (h::acc)
  |[] ->  failwith("impossible")

(** [check_valid card_name deck st] is true if [card_name] is in [deck], and if 
    the color, number, or special ability of [card_name] matches those of the 
    current card in state [st].  False otherwise. *)
let check_valid card_name deck st=
  List.mem card_name deck &&
  ((card_name_to_color card_name Uno.cards= 
    (card_name_to_color (current_card_name st ) Uno.cards))
   || (card_name_to_number card_name Uno.cards = 
       (card_name_to_number (current_card_name st) Uno.cards)))

(** [cycle current_turn direction numplayers gamest] is the next player's turn 
    based on the [current_turn], [direction], number of players [numplayers],
    and gamestate [gamest].*)
let cycle current_turn direction numplayers gamest=
  if gamest = 2 then 
    (if (current_turn = numplayers && direction = "asc") then 1 else 
     if (direction="des" && current_turn = 1) then numplayers else 
     if direction = "asc" then current_turn +1 else current_turn -1)
  else 
    (if (current_turn = numplayers-1 && direction = "asc") then 5 else 
     if (direction="des" && current_turn = 1) then 5 else 
     if (direction = "asc" && current_turn=5 )then 1 else 
     if (direction = "des" && current_turn=5 ) then numplayers -1
     else if direction = "asc" then current_turn +1 else current_turn -1)

(** [cycle_skip current_turn direction numplayers gamest] is the next next 
    player's turn based on the [current_turn], [direction], number of players 
    [numplayers],and gamestate [gamest].*)
let cycle_skip current_turn direction numplayers gamest=
  if gamest =2 then 
    (if current_turn > numplayers/2 &&
        direction = "asc" && current_turn+2>numplayers
     then (if ((current_turn+2) mod numplayers = 0) then current_turn else 
             (current_turn+2) mod numplayers)
     else if direction = "asc" && numplayers=2 then 1 else 
     if direction="asc" then current_turn +2 else
     if current_turn =2 && direction = "des" then numplayers else
     if current_turn =1 && direction ="des" then numplayers -1 else 
       current_turn-2)
  else 
    (if (current_turn = numplayers-1 && direction = "asc") then 1 else 
     if (direction="asc" && current_turn = 5 && numplayers >2) then 2 
     else if direction="asc" && current_turn = 5 then 5
     else if direction="asc" && current_turn +2 =numplayers then 5
     else if direction ="asc" then current_turn +2
     else if (direction = "des" && current_turn=1) then numplayers-1
     else if (direction ="des" && current_turn =2) then 5
     else if (direction="des" && current_turn =5 ) then 
       (if 3-(5-numplayers)=0 then current_turn else 3-(5-numplayers))
     else current_turn-2)

(** [whose deck turn st] is the deck of the player at the current [turn]
    in [st]. *)
let whose_deck turn st=
  match turn with 
  |1 -> st.player_one_cards
  |2 -> st.player_two_cards
  |3 -> st.player_three_cards
  |4-> st.player_four_cards
  |5-> st.computer_player_cards
  |_-> failwith("dont come hereee")

(**[drop card_name st] is the result of the current player dropping [card_name] 
   at state [st]. If this [card_name] a valid drop, removes [card_name] from 
   current player's deck, and updates the current card of the game to be 
   [card_name]. Additionally, it becomes the next player's turn. *)
let drop card_name st = 
  if (check_valid card_name (whose_deck st.turn st) st)
  then let _ = print_endline("Five seconds until next player. \n\nPlayer "^
                             (current_turn st|>string_of_int)^" dropped " ^ 
                             card_name);  in
    Legal {gamestate=gamestate st;
           numplayers=numplayers st;
           direction=current_direction st;
           turn =cycle (current_turn st) (current_direction st) (numplayers st) 
               (gamestate st);
           current_card= [Uno.match_card_name_to_card card_name Uno.cards];
           player_one_cards = if current_turn st = 1
             then take_out_one card_name st.player_one_cards []
             else st.player_one_cards;
           player_two_cards= if current_turn st =2 
             then take_out_one card_name st.player_two_cards [] 
             else st.player_two_cards;
           player_three_cards =if current_turn st =3 
             then take_out_one card_name st.player_three_cards [] 
             else st.player_three_cards; 
           player_four_cards= if current_turn st =4
             then take_out_one card_name st.player_four_cards [] 
             else st.player_four_cards; 
           computer_player_cards= if current_turn st =5
             then take_out_one card_name st.computer_player_cards [] 
             else st.computer_player_cards;
           stack_cards = [];
           swapping =0;
          }
  else let _ = print_endline("...") in Illegal

(** [draw st] is the result of the current player drawing a random card for 
    their turn in state [st].  This adds a random card to the current player's 
    deck and moves on to the next player's turn. Current card remains the same.*)
let draw st=
  let _ =print_endline("A card's added. \nFive seconds until next player.") in
  Legal { gamestate=gamestate st;
          numplayers=numplayers st;
          direction=current_direction st;
          turn =cycle (current_turn st) (current_direction st) (numplayers st) 
              (gamestate st);
          current_card= st.current_card;
          player_one_cards = if current_turn st =1 then 
              card_list_to_card_name_list (Uno.draw_random cards 1 115 []) 
              @ st.player_one_cards
            else st.player_one_cards;
          player_two_cards=  if current_turn st =2 then 
              card_list_to_card_name_list (Uno.draw_random cards 1 115 []) 
              @ st.player_two_cards
            else st.player_two_cards;
          player_three_cards=  if current_turn st =3 then 
              card_list_to_card_name_list (Uno.draw_random cards 1 115 [])
              @ st.player_three_cards
            else st.player_three_cards;
          player_four_cards= if current_turn st =4 then 
              card_list_to_card_name_list (Uno.draw_random cards 1 115 []) 
              @ st.player_four_cards
            else st.player_four_cards;
          computer_player_cards =  if current_turn st =5 then 
              card_list_to_card_name_list (Uno.draw_random cards 1 115 []) 
              @ st.computer_player_cards
            else st.computer_player_cards;
          stack_cards = [];
          swapping =0;
        }

(** [add_stack_cards st] adds the currently stacked cards in [st] to the 
    player's deck who fails to drop a draw two on top of draw two or a Wild Card 
    Draw 4 on top of another Wild Card Draw 4. That player's turn is also 
    skipped. *)
let add_stack_cards st = 
  let _ = if current_turn st = 5 && current_card_special st = DT then 
      print_endline("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
      The computer player has failed to drop a draw two, 
  so " ^ string_of_int (List.length st.stack_cards) ^ " cards were added to 
  their deck and they lost their turn!") 
    else if current_turn st = 5 && current_card_number st = -6 then 
      print_endline ("n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
      The computer player has failed to drop a Wild Card Draw 4, 
  so " ^ string_of_int (List.length st.stack_cards) ^ " cards were added to 
  their deck and they lost their turn!")
    else if current_card_special st = DT then 
      print_endline ("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
      You failed to drop another draw 2, so now you have lost 
      your turn and " ^ (List.length (get_stack_cards st) |> string_of_int) ^ 
                     " cards have been added to your deck!") 
    else print_endline ("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n
    You failed to drop another Wild Card Draw 4, so now you 
    have lost your turn and " ^ 
                        (List.length (get_stack_cards st) |> string_of_int) ^ 
                        " cards have been added to your deck!") in 
  let _ = print_endline ("Five seconds until the next player.") in 
  Legal {
    gamestate = gamestate st;
    numplayers = numplayers st;
    direction = current_direction st;
    turn = cycle (current_turn st) (current_direction st) (numplayers st) 
        (gamestate st);
    current_card = st.current_card;
    player_one_cards = if current_turn st = 1 then 
        st.stack_cards @ st.player_one_cards else st.player_one_cards;
    player_two_cards = if current_turn st = 2 then 
        st.stack_cards @ st.player_two_cards else st.player_two_cards;
    player_three_cards = if current_turn st = 3 then 
        st.stack_cards @ st.player_three_cards else st.player_three_cards;
    player_four_cards = if current_turn st = 4 then 
        st.stack_cards @ st.player_four_cards else st.player_four_cards;
    computer_player_cards = if current_turn st = 5 then 
        st.stack_cards @ st.computer_player_cards else st.computer_player_cards;
    stack_cards = [];
    swapping =0;
  }

(** [draw_two card_name st] is the result of a player dropping a draw two card
    [card_name] onto the pile, under the current state [st]. If the [card_name] 
    is a valid drop then the [card_name] will be removed from the player's deck, 
    and the next player will be told that they have to drop another draw two or 
    risk picking up the currently stacked cards in [st] and losing their turn.
*)
let draw_two card_name st = 
  if (check_valid card_name (whose_deck st.turn st) st)
  then 
    let _ = print_endline("Five seconds until the next player.") in
    let _ =  
      if cycle (current_turn st) (current_direction st) (numplayers st) 
          (gamestate st) = 5 then 
        print_endline ("Player " ^ (current_turn st |> string_of_int) ^ " has 
        tried to add two cards to the computers deck!")
      else print_endline("Player " ^ (current_turn st |> string_of_int) ^ "
       has tried to add two cards to your deck! 
    \nIf you do not drop a draw two, you will be given all the currently stacked 
    cards and your turn will be forfeited!") in 
    Legal{
      gamestate=gamestate st;
      numplayers=numplayers st;
      direction=current_direction st;
      turn = cycle (current_turn st) (current_direction st) (numplayers st) 
          (gamestate st);
      current_card = [Uno.match_card_name_to_card card_name Uno.cards];
      player_one_cards = if current_turn st =1 then 
          take_out_one card_name st.player_one_cards [] else 
          st.player_one_cards;
      player_two_cards = if current_turn st =2 then 
          take_out_one card_name st.player_two_cards [] else 
          st.player_two_cards;
      player_three_cards = if current_turn st =3 then 
          take_out_one card_name st.player_three_cards [] else 
          st.player_three_cards;
      player_four_cards = if current_turn st =4 then 
          take_out_one card_name st.player_four_cards [] else 
          st.player_four_cards;
      computer_player_cards = if current_turn st =5 then 
          take_out_one card_name st.computer_player_cards [] else 
          st.computer_player_cards;
      stack_cards = card_list_to_card_name_list 
          (Uno.draw_random cards 2 115 []) @ st.stack_cards;
      swapping =0;
    }
  else let _ = print_endline("...") in Illegal

(**[turn_skip card_name st] is the result of the current player dropping the 
   skip card [card_name] at state [st]. If this [card_name] a valid drop, 
   this removes [card_name] from current player's deck, skips the next player's
   turn, and updates the current card of the game to be [card_name]. 
   Additionally, it becomes the next next player's turn. *)
let turn_skip card_name st = 
  if (check_valid card_name (whose_deck st.turn st) st)
  then
    let _ = print_endline("Player " ^
                          ( cycle (current_turn st)(current_direction st) 
                              (numplayers st) (gamestate st)|> string_of_int)
                          ^ " has been skipped.\nFive seconds until player " 
                          ^(cycle_skip(current_turn st) (current_direction st) 
                              (numplayers st) (gamestate st)|> string_of_int)
                          ^"'s turn.")  in  
    Legal{
      gamestate=gamestate st;
      numplayers=numplayers st;
      direction=current_direction st;
      turn = cycle_skip(current_turn st) (current_direction st) (numplayers st) 
          (gamestate st);
      current_card = [Uno.match_card_name_to_card card_name Uno.cards];
      player_one_cards = if current_turn st = 1
        then take_out_one card_name st.player_one_cards [] 
        else st.player_one_cards;
      player_two_cards= if current_turn st =2 
        then take_out_one card_name st.player_two_cards [] 
        else st.player_two_cards;
      player_three_cards =if current_turn st =3 
        then take_out_one card_name st.player_three_cards [] 
        else st.player_three_cards; 
      player_four_cards= if current_turn st =4
        then take_out_one card_name st.player_four_cards [] 
        else st.player_four_cards; 
      computer_player_cards= if current_turn st =5
        then take_out_one card_name st.computer_player_cards [] 
        else st.computer_player_cards;
      stack_cards = st.stack_cards;
      swapping =0;
    }
  else let _ = print_endline("...") in Illegal

(** [turn_reverse card_name st] is the result of the current player dropping the 
    reverse card [card_name] at state [st]. If this [card_name] a valid drop, 
    this removes [card_name] from current player's deck, changes the direction 
    of the game (from ascending to descending, or from descending to ascending), 
    and updates the current card of the game to be [card_name]. 
    The turn goes to the next player in the reverse direction.*)
let turn_reverse card_name st = 
  if (check_valid card_name (whose_deck st.turn st) st)
  then 
    let _ = print_endline("The direction has been reversed. \nFive seconds 
    until it is player " ^((cycle (current_turn st) 
                              (if current_direction st = "asc"  then "des"  
                               else "asc") 
                              (numplayers st) (gamestate st))|>string_of_int)
                          ^"'s turn.")in  
    Legal{
      gamestate=gamestate st;
      numplayers=numplayers st;
      direction=if current_direction st = "asc" then "des" else "asc";
      turn = cycle (current_turn st) (if current_direction st = "asc" then 
                                        "des" else "asc") 
          (numplayers st) (gamestate st);
      current_card = [Uno.match_card_name_to_card card_name Uno.cards];
      player_one_cards = if current_turn st = 1
        then take_out_one card_name st.player_one_cards [] 
        else st.player_one_cards;
      player_two_cards= if current_turn st =2 
        then take_out_one card_name st.player_two_cards [] 
        else st.player_two_cards;
      player_three_cards =if current_turn st =3 
        then take_out_one card_name st.player_three_cards [] 
        else st.player_three_cards; 
      player_four_cards= if current_turn st =4
        then take_out_one card_name st.player_four_cards [] 
        else st.player_four_cards; 
      computer_player_cards= if current_turn st =5
        then take_out_one card_name st.computer_player_cards [] 
        else st.computer_player_cards;
      stack_cards = st.stack_cards;
      swapping =0;
    }
  else let _ = print_endline("...") in Illegal

(** [wild_card color_name st] is the result of the current player dropping the 
    wild card with the [color_name] of their choice at state [st]. This removes 
    the wild card from current player's deck, changes the current card 
    to be card [color_name]. Additionally, it becomes the next player's turn.*)
let wild_card color_name st = 

  let _ = print_endline ("Player " ^(current_turn st |> string_of_int )
                         ^" changed the color! \nFive seconds until player " 
                         ^( cycle (current_turn st) (current_direction st) 
                              (numplayers st) (gamestate st)|> string_of_int)
                         ^"'s turn.") in 
  Legal{
    gamestate=gamestate st;
    turn = cycle (current_turn st) (current_direction st)(numplayers st)
        (gamestate st) ;
    direction= current_direction st;
    numplayers=numplayers st;
    current_card = [Uno.match_card_name_to_card color_name Uno.cards];
    player_one_cards = if current_turn st = 1
      then take_out_one "Wild Card" st.player_one_cards [] 
      else st.player_one_cards;
    player_two_cards= if current_turn st =2 
      then take_out_one "Wild Card"  st.player_two_cards [] 
      else st.player_two_cards;
    player_three_cards =if current_turn st =3 
      then take_out_one "Wild Card"  st.player_three_cards [] 
      else st.player_three_cards; 
    player_four_cards= if current_turn st =4
      then take_out_one "Wild Card"  st.player_four_cards [] 
      else st.player_four_cards; 
    computer_player_cards= if current_turn st =5
      then take_out_one "Wild Card"  st.computer_player_cards [] 
      else st.computer_player_cards;
    stack_cards = st.stack_cards;
    swapping =0;
  }

(** [draw_four color_name st] is the result of the current player dropping the 
    wild card draw 4 card with the [color_name] of their choice at state [st]. 
    This removes the wild card draw 4 card from current player's deck, and
    prompts the next player that they have to drop a wild draw four too, or they
    lose their turn and will be given the currently stacked cards in [st]. This 
    also changes the current card of the game to be the card [color_name]. It 
    becomes the next player's turn. *)
let draw_four color_name st=
  let _ = print_endline("Five seconds until the next player.") in
  let _ =  
    if cycle (current_turn st) (current_direction st) (numplayers st) 
        (gamestate st) = 5 then 
      print_endline ("Player " ^ (current_turn st |> string_of_int) ^ " has 
      tried to add four cards to the computers deck!")
    else print_endline("Player " ^ (current_turn st |> string_of_int) ^ " has 
    tried to add four cards to your deck! 
    \nIf you do not drop a Wild Card Draw 4, you will be given all the currently 
    stacked cards and your turn will be forfeited!") in 
  Legal { gamestate=gamestate st;
          numplayers=numplayers st;
          direction= current_direction st;
          turn =cycle (current_turn st) (current_direction st)(numplayers st) 
              (gamestate st);
          current_card= [Uno.match_card_name_to_card color_name Uno.cards];
          player_one_cards = if current_turn st =1 then 
              take_out_one "Wild Card Draw 4" st.player_one_cards [] else 
              st.player_one_cards;
          player_two_cards = if current_turn st =2 then 
              take_out_one "Wild Card Draw 4" st.player_two_cards [] else 
              st.player_two_cards;
          player_three_cards = if current_turn st =3 then 
              take_out_one "Wild Card Draw 4" st.player_three_cards [] else 
              st.player_three_cards;
          player_four_cards = if current_turn st =4 then 
              take_out_one "Wild Card Draw 4" st.player_four_cards [] else 
              st.player_four_cards;
          computer_player_cards = if current_turn st =5 
            then take_out_one "Wild Card Draw 4" st.computer_player_cards [] 
            else st.computer_player_cards;
          stack_cards = card_list_to_card_name_list 
              (Uno.draw_random cards 4 115 []) @ st.stack_cards;
          swapping =0;
        }

(** [free_card card_name st] is the result of the 
    current player dropping a Free Card at state [st]. This changes the card 
    to whatever the current player chooses as the [card_name] and 
    removes the Free Card from the current player's deck.*)
let free_card card_name st =
  let _ = print_endline ("Player " ^(current_turn st |> string_of_int )
                         ^" has used a free card! \nFive seconds until it is 
                         player " ^(cycle (current_turn st) 
                                      (current_direction st) (numplayers st) 
                                      (gamestate st) |> string_of_int) ^"'s 
                                      turn.") in 
  Legal{
    gamestate=gamestate st;
    turn = cycle (current_turn st) (current_direction st)(numplayers st)
        (gamestate st);
    direction= current_direction st;
    numplayers=numplayers st;
    current_card = [Uno.match_card_name_to_card card_name Uno.cards];
    player_one_cards = if current_turn st = 1
      then take_out_one "Free Card" st.player_one_cards [] else 
        st.player_one_cards;
    player_two_cards= if current_turn st =2 
      then take_out_one "Free Card"  st.player_two_cards [] else 
        st.player_two_cards;
    player_three_cards =if current_turn st =3 
      then take_out_one "Free Card"  st.player_three_cards [] else 
        st.player_three_cards; 
    player_four_cards= if current_turn st =4
      then take_out_one "Free Card"  st.player_four_cards [] else 
        st.player_four_cards; 
    computer_player_cards= if current_turn st =5
      then take_out_one "Free Card"  st.computer_player_cards [] else 
        st.computer_player_cards;
    stack_cards = st.stack_cards;
    swapping =0;}

(** [swap_hands st player_swapping player_swapped] is the result of the 
    current card being a Wild Card Swap Hands. This swaps the cards of the 
    [player_swapping] and [player_swapped] and skips to the next person's turn.*)
let swap_hands st player_swapping player_swapped =

  Legal{
    gamestate=gamestate st;
    turn = cycle (current_turn st) (current_direction st)(numplayers st)
        (gamestate st);
    direction= current_direction st;
    numplayers=numplayers st;
    current_card = st.current_card;
    player_one_cards = if current_turn st = 1
      then player_swapped else if st.swapping = 1 then player_swapping else
        st.player_one_cards;
    player_two_cards= if current_turn st = 2  
      then player_swapped else if st.swapping = 2 then player_swapping else
        st.player_two_cards;
    player_three_cards =if current_turn st =3 
      then player_swapped else if st.swapping = 3 then player_swapping else
        st.player_three_cards;
    player_four_cards= if current_turn st =4
      then player_swapped else if st.swapping = 4 then player_swapping else
        st.player_four_cards;
    computer_player_cards= if current_turn st =5
      then player_swapped else if st.swapping = 5 then player_swapping else
        st.computer_player_cards;
    stack_cards = [];
    swapping = 0;}

(** [swap_cycle player_name st color_name] is the result of the current player
    dropping the Wild Card Swap Hands card with the [color_name] of their choice 
    at state [st]. This removes the Wild Card Swap Hands from current player's 
    deck and changes the current card to be card [color_name].*)
let swap_cycle player_name st color_name= 

  let _ = print_endline ("Player " ^(current_turn st |> string_of_int )
                         ^" has swapped hands with player " ^ 
                         string_of_int player_name ^"! 
                         \nFive seconds until it is player " 
                         ^( cycle (current_turn st) (current_direction st) 
                              (numplayers st) (gamestate st)|> string_of_int)
                         ^"'s turn.") in 
  {
    gamestate=gamestate st;
    turn = st.turn;
    direction= current_direction st;
    numplayers=numplayers st;
    current_card =  [Uno.match_card_name_to_card color_name Uno.cards];
    player_one_cards = if current_turn st = 1
      then take_out_one "Wild Card Swap Hands" st.player_one_cards [] 
      else st.player_one_cards;
    player_two_cards= if current_turn st =2 
      then take_out_one "Wild Card Swap Hands"  st.player_two_cards [] 
      else st.player_two_cards;
    player_three_cards =if current_turn st =3 
      then take_out_one "Wild Card Swap Hands" st.player_three_cards [] 
      else st.player_three_cards; 
    player_four_cards= if current_turn st =4
      then take_out_one "Wild Card Swap Hands"  st.player_four_cards [] 
      else st.player_four_cards; 
    computer_player_cards= if current_turn st =5
      then take_out_one "Wild Card Swap Hands"  st.computer_player_cards [] 
      else st.computer_player_cards;
    stack_cards = [];
    swapping =player_name;}

(** [swapping st] is the player that is chosen to swap hands with at state [st]. 
    If this is 0, no player is prompted to have their hand swapped with 
    another player. *)
let swapping st =
  st.swapping

(** [comp_choose_color] is a random color generator for the computer player. *)
let comp_choose_color =
  List.nth ["Red";"Blue"; "Yellow"; "Green"] (Random.int 4)

(** [comp_choose_color] is a random player generator for the computer player. *)
let comp_choose_hand =
  List.nth [1; 2; 3; 4] (Random.int 4)

(** [vscomputer st vs_player_traverse] is the result of the computer player
    selecting a valid game action (drop/draw) based on the current card in state 
    [st] and the computer player's cards [vs_player_traverse]. If there's a 
    valid card in computer's deck [vs_player_traverse], computer player will 
    drop the first valid card in the deck. Otherwise, computer will draw.*)
let rec vscomputer st vs_player_traverse=
  if st.stack_cards <> [] && current_card_special st = DT then
    match vs_player_traverse with 
    |h::t -> if h = "Red Draw 2" || h = "Green Draw 2" || h = "Yellow Draw 2"
                || h = "Blue Draw 2" then draw_two h st else vscomputer st t
    |[] -> add_stack_cards st 
  else 
  if st.stack_cards <> [] && current_card_number st = -6 then
    match vs_player_traverse with 
    |h::t -> if h = "Wild Card Draw 4" then draw_four comp_choose_color st else
        vscomputer st t
    |[] -> add_stack_cards st 
  else 
    match vs_player_traverse with
    |h::t -> let check_match = match_card_name_to_card h Uno.cards in 
      if (current_card_color st = card_color check_match ||
          current_card_number st = card_number check_match) then 
        match card_special check_match with
        |DT -> draw_two h st
        |Skip -> turn_skip h st
        |Reverse -> turn_reverse h st
        |WC -> wild_card comp_choose_color st
        |WCDF -> draw_four comp_choose_color st
        |NotSpecial -> drop h st
        |WCSH -> let swapper = comp_choose_hand in
          let s = swap_cycle swapper st comp_choose_color in
          swap_hands (s) 
            (s.computer_player_cards) (whose_deck swapper s)
        | Free -> free_card (current_card_name st) st

      else if card_special check_match = WC || card_special check_match = WCDF 
              || card_special check_match = WCSH || 
              card_special check_match = Free && st.stack_cards = [] then 
        match card_special check_match with
        |WC -> wild_card comp_choose_color st
        |WCDF -> draw_four comp_choose_color st
        |WCSH -> let swapper = comp_choose_hand in
          let s = swap_cycle swapper st comp_choose_color in
          swap_hands (s) 
            (s.computer_player_cards) (whose_deck swapper s)
        | Free -> 
          free_card (current_card_name st) st
        |_-> failwith "won't get here"
      else vscomputer st t
    |[]-> draw st 


(** [draw_two_uno card_name st] is the result of the current player forgetting
    to follow their drop command with "uno", when they have two cards left.  
    This adds two cards to the current player's deck. *)
let draw_two_uno card_name st = 
  print_string("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
  if List.mem card_name (whose_deck (current_turn st) st) then  
    let _ = print_endline("Oops! Looks like you forgot to say uno after dropping
     your card, so two cards have been added to your deck.") in
    Legal{
      gamestate=gamestate st;
      numplayers=numplayers st;
      direction= current_direction st;
      turn = current_turn st;
      current_card = st.current_card;
      player_one_cards = if current_turn st =1 then
          (card_list_to_card_name_list (Uno.draw_random cards 2 115 [])) 
          @ st.player_one_cards
        else st.player_one_cards;
      player_two_cards = if current_turn st =2 then 
          (card_list_to_card_name_list (Uno.draw_random cards 2 115 [])) 
          @ st.player_two_cards
        else st.player_two_cards;
      player_three_cards = if current_turn st =3 then
          (card_list_to_card_name_list (Uno.draw_random cards 2 115 [])) 
          @ st.player_three_cards
        else st.player_three_cards;
      player_four_cards = if current_turn st =4 then 
          (card_list_to_card_name_list (Uno.draw_random cards 2 115 [])) 
          @ st.player_four_cards
        else st.player_four_cards;
      computer_player_cards = if current_turn st =5 then
          (card_list_to_card_name_list (Uno.draw_random cards 2 115 [])) 
          @ st.computer_player_cards
        else st.computer_player_cards;
      stack_cards = st.stack_cards;
      swapping = 0;
    }
  else let _ = print_endline("...") in Illegal

(** [turn_reverse_free card_name st] is the result of the current player 
    choosing to turn their free card into a reverse card with the [card_name] 
    of their choice at state [st]. If this [card_name] a valid drop, 
    this removes the Free Card from the current player's deck, changes 
    the direction of the game (from ascending to descending, or 
    from descending to ascending), and updates the current card of the game
     to be [card_name]. The turn goes to the next player in the 
     reverse direction.*)
let turn_reverse_free card_name st = 
  if ((card_name_to_color card_name Uno.cards= 
       (card_name_to_color (current_card_name st ) Uno.cards))
      || (card_name_to_number card_name Uno.cards = 
          (card_name_to_number (current_card_name st) Uno.cards)))
  then 
    let _ = print_endline("The direction has been reversed. 
    \nFive seconds until it is player " ^((cycle (current_turn st) 
                                             (if current_direction st = "asc"  
                                              then "des" else "asc") 
                                             (numplayers st) (gamestate st))
                                          |>string_of_int)
                          ^"'s turn.")in  
    Legal {
      gamestate=gamestate st;
      numplayers=numplayers st;
      direction=if current_direction st = "asc" then "des" else "asc";
      turn = cycle (current_turn st) 
          (if current_direction st = "asc" then "des" else "asc") 
          (numplayers st) (gamestate st);
      current_card = [Uno.match_card_name_to_card card_name Uno.cards];
      player_one_cards = if current_turn st = 1
        then take_out_one "Free Card" st.player_one_cards [] else 
          st.player_one_cards;
      player_two_cards= if current_turn st =2 
        then take_out_one "Free Card" st.player_two_cards [] else 
          st.player_two_cards;
      player_three_cards =if current_turn st =3 
        then take_out_one "Free Card" st.player_three_cards [] else 
          st.player_three_cards; 
      player_four_cards= if current_turn st =4
        then take_out_one "Free Card" st.player_four_cards [] else 
          st.player_four_cards; 
      computer_player_cards= if current_turn st =5
        then take_out_one "Free Card" st.computer_player_cards [] else 
          st.computer_player_cards;
      stack_cards = [];
      swapping =0;}
  else failwith "not possible"

(** [draw_two_free card_name st] is the result of a player choosing to turn 
    their free card into a draw two card [card_name] at state [st].
    If the [card_name] is a valid drop then the Free Card will be removed from 
    the player's deck, and the next player will be told that they have to drop 
    another draw two or risk picking up the currently stacked cards in [st] and 
    losing their turn.
*)
let draw_two_free card_name st = 
  if ((card_name_to_color card_name Uno.cards= 
       (card_name_to_color (current_card_name st ) Uno.cards))
      || (card_name_to_number card_name Uno.cards = 
          (card_name_to_number (current_card_name st) Uno.cards)))
  then 
    let _ = print_endline("Five seconds until the next player.") in
    let _ =  
      if cycle (current_turn st) (current_direction st) (numplayers st) 
          (gamestate st) = 5 then 
        print_endline ("Player " ^ (current_turn st |> string_of_int) ^ " has 
        tried to add two cards to the computers deck!")
      else print_endline("Player " ^ (current_turn st |> string_of_int) ^ " 
      has tried to add two cards to your deck! 
    \nIf you do not drop a draw two, you will be given all the currently stacked 
    cards and your turn will be forfeited!") in 
    Legal{
      gamestate=gamestate st;
      numplayers=numplayers st;
      direction=current_direction st;
      turn = cycle (current_turn st) (current_direction st) (numplayers st) 
          (gamestate st);
      current_card = [Uno.match_card_name_to_card card_name Uno.cards];
      player_one_cards = if current_turn st =1 then take_out_one "Free Card" 
            st.player_one_cards []
        else st.player_one_cards;
      player_two_cards = if current_turn st =2 then take_out_one "Free Card" 
            st.player_two_cards []
        else st.player_two_cards;
      player_three_cards = if current_turn st =3 then take_out_one "Free Card" 
            st.player_three_cards []
        else st.player_three_cards;
      player_four_cards = if current_turn st =4 then take_out_one "Free Card" 
            st.player_four_cards []
        else st.player_four_cards;
      computer_player_cards = if current_turn st =5 then take_out_one 
            "Free Card" st.computer_player_cards []
        else st.computer_player_cards;
      stack_cards = card_list_to_card_name_list (Uno.draw_random cards 2 115 []) 
                    @ st.stack_cards;
      swapping =0;
    }
  else let _ = print_endline("...") in Illegal

(**[turn_skip_free card_name st] is the result of the current player 
   choosing to turn their free card into a skip card [card_name] at state [st]. 
   If this [card_name] a valid drop, this removes Free Card from the current 
   player's deck, skips the next player's turn, and updates the current card of 
   the game to be [card_name]. Additionally, it becomes 
   the next next player's turn. *)
let turn_skip_free card_name st = 
  if ((card_name_to_color card_name Uno.cards= 
       (card_name_to_color (current_card_name st ) Uno.cards))
      || (card_name_to_number card_name Uno.cards = 
          (card_name_to_number (current_card_name st) Uno.cards)))
  then 
    let _ = print_endline("Player " ^( cycle (current_turn st) 
                                         (current_direction st) (numplayers st) 
                                         (gamestate st)|> string_of_int)
                          ^ " has been skipped.  \nFive seconds until it is 
                          player " 
                          ^( cycle_skip(current_turn st) (current_direction st) 
                               (numplayers st) (gamestate st)|> string_of_int)
                          ^"'s turn.")  in  
    Legal {
      gamestate= gamestate st;
      numplayers=numplayers st;
      direction=current_direction st; 
      turn = cycle_skip(current_turn st) (current_direction st) (numplayers st) 
          (gamestate st);
      current_card = [Uno.match_card_name_to_card card_name Uno.cards];
      player_one_cards = 
        if current_turn st = 1
        then take_out_one "Free Card" st.player_one_cards [] else 
          st.player_one_cards;
      player_two_cards= 
        if current_turn st =2 
        then take_out_one "Free Card" st.player_two_cards [] else 
          st.player_two_cards;
      player_three_cards =
        if current_turn st =3 
        then take_out_one "Free Card" st.player_three_cards [] else 
          st.player_three_cards; 
      player_four_cards= 
        if current_turn st =4
        then take_out_one "Free Card" st.player_four_cards [] else 
          st.player_four_cards; 
      computer_player_cards=
        if current_turn st =5
        then take_out_one "Free Card" st.computer_player_cards [] else 
          st.computer_player_cards;
      stack_cards = [];
      swapping =0;}
  else failwith "not possible"

(** [wild_card_free color_name st] is the result of the current player 
    choosing to to turn their free card into a wild card  with the [color_name] 
    of their choice at state [st]. This removes the Free Card from current 
    player's deck and changes the current card to be card [color_name]. 
    Additionally, it becomes the next player's turn.*)
let wild_card_free color_name st = 
  let _ = print_endline ("Player " ^(current_turn st |> string_of_int )
                         ^" has changed the color! \nFive seconds until it is 
                         player " 
                         ^( cycle (current_turn st) (current_direction st) 
                              (numplayers st) (gamestate st)|> string_of_int)
                         ^"'s turn.") in 
  Legal{
    gamestate=gamestate st;
    turn = cycle (current_turn st) (current_direction st)(numplayers st)
        (gamestate st);
    direction= current_direction st;
    numplayers=numplayers st;
    current_card = [Uno.match_card_name_to_card color_name Uno.cards];
    player_one_cards = if current_turn st = 1
      then take_out_one "Free Card" st.player_one_cards [] else 
        st.player_one_cards;
    player_two_cards= if current_turn st =2 
      then take_out_one "Free Card"  st.player_two_cards [] else 
        st.player_two_cards;
    player_three_cards =if current_turn st =3 
      then take_out_one "Free Card"  st.player_three_cards [] else 
        st.player_three_cards; 
    player_four_cards= if current_turn st =4
      then take_out_one "Free Card"  st.player_four_cards [] else 
        st.player_four_cards; 
    computer_player_cards= if current_turn st =5
      then take_out_one "Free Card"  st.computer_player_cards [] else 
        st.computer_player_cards;
    stack_cards = [];
    swapping =0;}

(** [draw_four_free color_name st] is the result of the current player choosing 
    to turn their free card into a wild card draw 4 card with the [color_name] of 
    their choice at state [st]. This removes the free card from the current 
    player's deck, and prompts the next player that they have to drop a wild 
    draw four too, or they lose their turn and will be given the currently 
    stacked cards in [st]. This also changes the current card of the game to be 
    the card [color_name]. It becomes the next player's turn. *)
let draw_four_free color_name st=
  let _ = print_endline("Five seconds until the next player.") in
  let _ =  
    if cycle (current_turn st) (current_direction st) (numplayers st) 
        (gamestate st) = 5 then 
      print_endline ("Player " ^ (current_turn st |> string_of_int) ^ " has 
      tried to add four cards to the computers deck!")
    else print_endline("Player " ^ (current_turn st |> string_of_int) ^ " 
    has tried to add four cards to your deck! 
    \nIf you do not drop a Wild Card Draw 4, you will be given all the currently 
    stacked cards and your turn will be forfeited!") in 
  Legal { gamestate=gamestate st;
          numplayers=numplayers st;
          direction= current_direction st;
          turn =cycle (current_turn st) (current_direction st)(numplayers st) 
              (gamestate st);
          current_card= [Uno.match_card_name_to_card color_name Uno.cards];
          player_one_cards = if current_turn st =1 then 
              take_out_one "Free Card" st.player_one_cards []
            else st.player_one_cards;
          player_two_cards = if current_turn st =2 then 
              take_out_one "Free Card" st.player_two_cards []
            else st.player_two_cards;
          player_three_cards = if current_turn st =3 then 
              take_out_one "Free Card" st.player_three_cards []
            else st.player_three_cards;
          player_four_cards = if current_turn st =4 then 
              take_out_one "Free Card" st.player_four_cards []
            else st.player_four_cards;
          computer_player_cards = if current_turn st =5 then 
              take_out_one "Free Card" st.computer_player_cards []
            else st.computer_player_cards;
          stack_cards = card_list_to_card_name_list 
              (Uno.draw_random cards 4 115 []) @ st.stack_cards;
          swapping =0;
        }

(** [swap_cycle_free player_name st color_name] is the result of the 
    current player changing their free card to a Wild Card Swap Hands
    with the [color_name] of their choice at state [st]. This removes the
    Free Card from current player's deck and changes the current card to be 
    card [color_name].*)
let swap_cycle_free player_name st color_name= 
  let _ = print_endline ("Player " ^(current_turn st |> string_of_int )
                         ^" has swapped hands with player " ^ 
                         string_of_int player_name ^"! 
                         \nFive seconds until it is player " 
                         ^( cycle (current_turn st) (current_direction st) 
                              (numplayers st) (gamestate st)|> string_of_int)
                         ^"'s turn.") in 
  {
    gamestate=gamestate st;
    turn = st.turn;
    direction= current_direction st;
    numplayers=numplayers st;
    current_card =  [Uno.match_card_name_to_card color_name Uno.cards];
    player_one_cards = if current_turn st = 1
      then take_out_one "Free Card" st.player_one_cards [] else 
        st.player_one_cards;
    player_two_cards= if current_turn st =2 
      then take_out_one "Free Card"  st.player_two_cards [] else 
        st.player_two_cards;
    player_three_cards =if current_turn st =3 
      then take_out_one "Free Card" st.player_three_cards [] else 
        st.player_three_cards; 
    player_four_cards= if current_turn st =4
      then take_out_one "Free Card"  st.player_four_cards [] else 
        st.player_four_cards; 
    computer_player_cards= if current_turn st =5
      then take_out_one "Free Card"  st.computer_player_cards [] else 
        st.computer_player_cards;
    stack_cards = [];
    swapping =player_name;}



