type color = Black | Red | Blue | Green | Yellow

type special = None | Skip | Reverse | D2 | D4 | Wild | Swap

type card_type = Number of int | Special of special

type card = {color: color; card_type: card_type}

type drew = {draw_pile: card list; player_hand: card list}

type played = {discard_pile: card list; player_hand: card list}

type game = {draw_pile: card list; 
             discard_pile: card list; 
             player_hands: card list list;
             turn:int;
             reversed:bool;
             scores: int list}

type ai_played = {draw_pile: card list;
                  discard_pile: card list;
                  player_hand: card list}

(** Raised when a player tries to play a card that they don't have. *)
exception CardNotFound
exception InvalidPlayCard

val shuffle : 'a list -> 'a list

val init : int -> game

val play_card : card -> card list -> card list -> card list -> played

val play_card_2 : card -> game -> int -> card list list -> int -> game

val draw_card : card list -> card list -> drew

val draw_card_2 : game -> int -> card list list -> int -> game

val find_playable_cards : card -> card list -> card list

val ai_play : card list -> card list -> card list -> ai_played

val ai_turn : game -> int -> card list list -> int -> game

val check_win : game -> int -> int list 

val check_piles : game -> game

val best_player : ?m:int -> ?p:int -> ?a:int -> int -> 'a list list -> int

val best_color : ?m:int -> ?c:color -> ?acc:(color * int) list -> card list -> color

val random_color_except : color -> color

val find_card_by_color : color -> card list -> card

val find_card_by_type : card_type -> card list -> card 

val find_playable_cards : card -> card list -> card list

val find_wild_card : card list -> card

val random_color_except : color -> color

val best_player : ?m:int -> ?p:int -> ?a:int -> int -> 'a list list -> int
