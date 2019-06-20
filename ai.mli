open Deck
type aimove = Play of Deck.card | Draw
type node = {move:aimove option; mutable parent:node option; 
             mutable children:node list; mutable wins:int; mutable visits:int; 
             mutable avails:int; recentplayer:int option}
type tree = {nodelist: node list}

(** Returns a deck game state after a move has been made *)
val do_move : aimove -> Deck.game -> Deck.game

(** Returns the optimal move as found through an imperfect information set 
    Monte Carlo Tree Search*)
val ismcts : Deck.game -> int -> aimove option
