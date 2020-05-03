type t
val testing_initializer: int->int->string-> int-> Uno.card list ->
  string list -> string list -> string list-> string list-> string list -> 
  string list -> int -> t
val init: int -> int -> t
val current_direction: t->string
val numplayers: t-> int
val gamestate : t -> int
val numplayers: t->int
val get_stack_cards: t -> string list
val current_turn : t-> int
val current_card_name: t-> string
val current_card_color: t->Uno.color
val current_card_number: t-> int
val current_card_special: t-> Uno.special
val player_one: t-> string list
val player_two: t-> string list
val player_three: t-> string list
val player_four: t-> string list
val vs_player: t-> string list

type result = Legal of t | Illegal

val take_out_one: string -> string list -> string list->string list
val check_valid: string -> string list-> t-> bool
val cycle: int -> string -> int -> int -> int
val cycle_skip: int -> string -> int -> int -> int
val whose_deck: int-> t -> string list
val drop: string-> t -> result
val draw: t -> result
val add_stack_cards: t -> result
val draw_two: string -> t -> result
val turn_skip: string -> t -> result
val turn_reverse: string -> t -> result
val wild_card: string -> t -> result
val draw_four: string -> t -> result
val comp_choose_color: string
val vscomputer: t -> string list -> result
val draw_two_uno:  string -> t -> result
val swap_hands: t -> string list -> string list -> result
val swap_cycle: int -> t -> string -> t
val swapping: t -> int
val free_card: string -> t -> result
val draw_two_free: string -> t -> result
val turn_skip_free: string -> t -> result
val turn_reverse_free: string -> t -> result
val swap_cycle_free: int -> t -> string -> t
val wild_card_free: string -> t -> result
val draw_four_free: string -> t -> result
