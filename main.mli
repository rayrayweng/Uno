val parsespecial: Deck.special -> string
val parsecard: Deck.card -> string
val parsehand: Deck.card list -> string -> string
val convertcolor: string -> Deck.color option
val converttype: string -> Deck.card_type option
val convertcard: string list -> Deck.card
val parse_line: Deck.game -> Deck.game
val turn_play: Deck.game -> bool -> unit
val play_game: int -> unit
val main: unit -> unit
exception NoPlayers of string
exception InvalidCard
