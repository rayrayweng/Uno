type color = Black | Red | Blue | Green | Yellow
type special = None | Skip | Reverse | D2 | D4 | Wild | Swap
type card_type = Number of int | Special of special
type card = {color: color; card_type: card_type}
type game = {draw_pile: card list; 
             discard_pile: card list; 
             player_hands: card list list;
             turn:int;
             reversed:bool;
             scores: int list}
type drew = {draw_pile: card list; player_hand: card list}
type played = {discard_pile: card list; player_hand: card list}
type ai_played = {draw_pile: card list;
                  discard_pile: card list;
                  player_hand: card list}
exception CardNotFound
exception PlayerNotFound
exception InvalidPlayCard

(** [shuffle lst] is a random permutation of [lst]. *)
let shuffle (lst : 'a list) : 'a list =
  QCheck.Gen.(generate1 (shuffle_l lst))

(** [random_color] randomly returns one of Red, Blue, Green, or Yellow except 
    for [color] *)
let random_color_except color = 
  let num = Random.int 3 in 
  if color = Red then List.nth [Blue;Green;Yellow] num
  else if color = Blue then List.nth [Red;Green;Yellow] num
  else if color = Green then List.nth [Red;Blue;Yellow] num
  else List.nth [Red;Blue;Green] num

(** [make_deck num_decks] combines [num_decks] standard Uno decks into one deck 
    and shuffles it *)
let make_deck num_decks =
  let rec make_suit color total acc = 
    match total with
    | 0 -> 
      {color = Black; card_type = Special Wild}::
      {color = Black; card_type = Special D4}::
      {color = color; card_type = Special Skip}::
      {color = color; card_type = Special Skip}::
      {color = color; card_type = Special D2}::
      {color = color; card_type = Special D2}::
      {color = color; card_type = Special Reverse}::
      {color = color; card_type = Special Reverse}::
      {color = color; card_type = Number 0}::acc
    | x -> 
      let num = total mod 9 + 1 in
      let new_card = {color = color; card_type = Number num;} in
      make_suit color (total - 1) (new_card::acc) in 
  let deck = (make_suit Red 18 []) @ 
             (make_suit Blue 18 []) @ 
             (make_suit Green 18 []) @ 
             (make_suit Yellow 18 []) in
  let rec duplicate num acc = 
    match num with
    | 0 -> acc
    | x -> duplicate (num - 1) (deck@acc) in 
  shuffle (({color= Black; card_type=Special Swap})::(duplicate num_decks []))

(** [deal_cards deck hand num_cards] deals [num_cards] cards to [hand] from 
    [deck] *)
let rec deal_cards deck hand num_cards =
  match num_cards with
  | 0 -> [deck]@[hand]
  | x -> deal_cards (List.tl deck) ((List.hd deck)::hand) (num_cards - 1)

(** [init num_players] initializes a new game for [num_players] players *)
let init num_players = 
  let rec make_scores num acc = match num with
    | 0 -> acc
    | x -> make_scores (num-1) (0::acc) in
  let start_deck = {draw_pile = make_deck ((num_players - 1) / 4 + 1); 
                    discard_pile = []; 
                    player_hands = [];
                    turn = 0;
                    reversed=false;
                    scores=[]} in 
  let rec init_deal deck num_players acc = 
    match num_players with
    | 0 -> [deck]@acc
    | x -> 
      let new_deck = deal_cards deck [] 7 in 
      init_deal (List.hd new_deck) (x-1) (acc@List.tl new_deck) in
  let start = init_deal start_deck.draw_pile num_players [] in
  let new_pile = List.hd start in 
  {draw_pile = List.tl new_pile; 
   discard_pile = [List.hd new_pile]; 
   player_hands = List.tl start;
   turn = 0;
   reversed=false;
   scores=make_scores num_players []}

(** [find_playable_cards top_discard player_hand] is a list of cards in 
    [player_hand] that can be played after [top_discard] *)
let find_playable_cards top_discard player_hand =
  let top_color = top_discard.color in 
  let top_card_type = top_discard.card_type in
  let rec find color card_type hand acc =
    match hand with
    | [] -> acc
    | hd::tl -> 
      if hd.color = color || hd.card_type = card_type || hd.color = Black
      then find color card_type tl (hd::acc) 
      else find color card_type tl acc in 
  find top_color top_card_type player_hand []

(** [play_card card player_hand acc discard_pile] finds [card] in [player_hand] 
    and prepends it to [discard_pile]
    Raises: CardNotFound if [player_hand] does not contain [card] *)
let rec play_card card player_hand acc discard_pile =
  if List.mem card (find_playable_cards (List.hd discard_pile) player_hand) then
    match player_hand with
    | [] -> raise CardNotFound
    | hd::tl -> if hd = card 
      then {discard_pile = hd::discard_pile; player_hand = acc@tl}
      else play_card card tl (acc@[hd]) discard_pile
  else raise InvalidPlayCard

(* Call it with hands_acc = [], acc=0 to start*)
let rec play_card_2 card gam player_num hands_acc acc= 
  match gam.player_hands with
  | [] -> raise PlayerNotFound
  | hd::tl -> if player_num = acc
    then let result = play_card card hd [] gam.discard_pile in
      let rest_of_hands = tl in 
      if player_num = 0
      then {draw_pile = gam.draw_pile; 
            discard_pile = result.discard_pile; 
            player_hands = result.player_hand::rest_of_hands; 
            turn = gam.turn;reversed=gam.reversed; 
            scores=gam.scores}
      else {draw_pile = gam.draw_pile; 
            discard_pile = result.discard_pile; 
            player_hands = hands_acc@(result.player_hand::rest_of_hands); 
            turn = gam.turn;
            reversed=gam.reversed; 
            scores=gam.scores}
    else let next_gam = {draw_pile = gam.draw_pile; 
                         discard_pile = gam.discard_pile; 
                         player_hands = tl; 
                         turn = gam.turn;
                         reversed=gam.reversed; 
                         scores=gam.scores} in 
      play_card_2 card next_gam player_num (hands_acc@[hd]) (acc+1)

(** [draw_card draw_pile player_hand] removes the first card in [draw_pile] and 
    prepends it to [player_hand] *)
let draw_card draw_pile player_hand = 
  {draw_pile = List.tl draw_pile; 
   player_hand = (List.hd draw_pile)::player_hand}

let rec draw_card_2 gam player_num hands_acc acc = 
  match gam.player_hands with
  | [] -> raise PlayerNotFound
  | hd::tl -> if player_num = acc
    then if player_num = 0
      then {draw_pile = List.tl gam.draw_pile; 
            discard_pile = gam.discard_pile; 
            player_hands = ((List.hd gam.draw_pile)::hd)::tl; 
            turn = gam.turn;
            reversed=gam.reversed;
            scores=gam.scores}
      else {draw_pile = List.tl gam.draw_pile; 
            discard_pile = gam.discard_pile; 
            player_hands = hands_acc@(((List.hd gam.draw_pile)::hd)::tl); 
            turn = gam.turn;
            reversed=gam.reversed;
            scores=gam.scores}
    else let next_gam = {draw_pile = gam.draw_pile; 
                         discard_pile = gam.discard_pile; 
                         player_hands = tl; 
                         turn = gam.turn;
                         reversed=gam.reversed;
                         scores=gam.scores} in
      draw_card_2 next_gam player_num (hands_acc@[hd]) (acc+1)

(** [find_card_by_color color player_hand] is a card in [player_hand] with the 
    color [color] 
    Raises: CardNotFound if [player_hand] does not contain a card with color 
    [color]*)
let rec find_card_by_color color player_hand =
  match player_hand with
  | [] -> raise CardNotFound
  | hd::tl -> if hd.color = color then hd else find_card_by_color color tl

(** [find_card_by_type card_type player_hand] is a card in [player_hand] with 
    the card_type [card_type] 
    Raises: CardNotFound if [player_hand] does not contain a card with 
    card_type [card_type]*)
let rec find_card_by_type card_type player_hand =
  match player_hand with
  | [] -> raise CardNotFound
  | hd::tl -> if hd.card_type = card_type then hd 
    else find_card_by_type card_type tl

(** [find_wild_card player_hand] is a wild card in [player_hand] 
    Raises: CardNotFound if [player_hand] does not contain a wild card *)
let rec find_wild_card player_hand =
  match player_hand with
  | [] -> raise CardNotFound
  | hd::tl -> 
    if hd.card_type = Special Wild || hd.card_type = Special D4 || 
       hd.card_type = Special Swap then hd
    else find_wild_card tl

(** [best_color max color acc player_hand] is the color that [player_hand] has 
    the most of, excluding Black unless [player_hand] only has Black cards. *)
let rec best_color ?m:(max=0) ?c:(color=Black) ?acc:(a=[(Red, 0);(Blue, 0);(Green, 0);(Yellow, 0);]) player_hand : color =
  match player_hand with
  | [] -> if color != Black then color else Red
  | hd::tl -> 
    let col = hd.color in 
    if col = Black then best_color ~m:max ~c:color ~acc:a tl 
    else let num = (List.assoc col a) + 1 in 
      let lst = List.remove_assoc col a in
      if num > max then best_color ~m:(num) ~c:col ~acc:((col, (num))::lst) tl
      else best_color ~m:max ~c:color ~acc:((col, (num))::lst) tl

(** [best_player ?m ?p ?a current player_hands] returns the best hand in [player_hands] for player [current] to swap to *)
let rec best_player ?m:(min=1000) ?p:(player=0) ?a:(acc=0) curr player_hands =
  match player_hands with 
  | [] -> player
  | h::t -> let length = List.length h in
    if length < min && acc != curr then best_player ~m:length ~p:acc ~a:(acc+1) curr t
    else best_player ~m:min ~p:player ~a:(acc+1) curr t

(** [ai_play draw_pile discard_pile player_hand] is the resulting draw pile, 
    discard pile and player hand after an ai with hand [player_hand] takes 
    their turn*)
let ai_play draw_pile discard_pile player_hand =
  let top_discard = List.hd discard_pile in
  let color = top_discard.color in
  let card_type = top_discard.card_type in
  let playable_cards = find_playable_cards top_discard player_hand in
  try let card =
        try find_card_by_color color playable_cards with
        | CardNotFound -> 
          try find_card_by_type card_type playable_cards with
          | CardNotFound -> 
            find_wild_card playable_cards in 
    let played = play_card card player_hand [] discard_pile in
    {draw_pile = draw_pile; 
     discard_pile = played.discard_pile;
     player_hand = played.player_hand}
  with 
  | CardNotFound -> let drew = draw_card draw_pile player_hand in
    {draw_pile = drew.draw_pile; 
     discard_pile = discard_pile; 
     player_hand = drew.player_hand}

(* Call with hands_acc = [], acc = 0 to start, ai_num is the AI's index number in game.player_hands*)
let rec ai_turn gam ai_num hands_acc acc = 
  match gam.player_hands with
  | [] -> raise PlayerNotFound
  | hd::tl -> if ai_num = acc
    then let result = ai_play gam.draw_pile gam.discard_pile hd in 
      if ai_num = 0
      then {draw_pile = result.draw_pile; 
            discard_pile = result.discard_pile; 
            player_hands = (result.player_hand)::tl; 
            turn = gam.turn;
            reversed=gam.reversed;
            scores=gam.scores}
      else {draw_pile = result.draw_pile; 
            discard_pile = result.discard_pile; 
            player_hands = hands_acc@((result.player_hand)::tl); 
            turn = gam.turn;
            reversed=gam.reversed;
            scores=gam.scores}
    else let next_gam = {draw_pile = gam.draw_pile; 
                         discard_pile = gam.discard_pile; 
                         player_hands = tl; 
                         turn = gam.turn;
                         reversed=gam.reversed;
                         scores=gam.scores} in 
      ai_turn next_gam ai_num (hands_acc@[hd]) (acc+1)

(* Checks the draw pile and replenishes it if necessary*)
let rec check_piles (gam:game) = 
  if List.length gam.draw_pile < 3
  then let new_draw_pile = shuffle ((List.tl gam.discard_pile)@gam.draw_pile) in 
    {draw_pile = new_draw_pile; 
     discard_pile = [List.hd gam.discard_pile]; 
     player_hands = gam.player_hands; 
     turn = gam.turn;
     reversed=gam.reversed;
     scores=gam.scores}
  else gam

(* acc refers to the player number, call it with 0 to start
   returns a list with the player number that has an empty hand i.e. win
   if returned list is empty, no one has won (no empty hands)*)
let rec check_win (gam:game) acc = 
  match gam.player_hands with
  | [] -> []
  | hd::tl -> if (List.length hd) = 0 then [acc] 
    else let next_gam = {draw_pile = gam.draw_pile; 
                         discard_pile = gam.discard_pile; 
                         player_hands = tl; 
                         turn = gam.turn;
                         reversed=gam.reversed;
                         scores=gam.scores} in 
      check_win next_gam (acc+1)
(* Notes:
    I basically changed the play, draw, and ai_play to take in a game and return 
    a game (so a separate state file is not necessary). Every play through in 
    main creates a game using deck.init, and that deck.game is then "updated" 
    each time a player does something or an AI does something. The game type 
    essentially contains all the information required for a game of UNO, and 
    play, draw etc. just alter the game state. 

    I have created play_card_2, draw_card_2 and ai_turn that each take in a game 
    and return a game; these can be called directly from main as it progresses 
    through a run-through of UNO (as can init).

    In main, you can iterate over turns by checking the turn attribute of the 
    game type; if the turn is the human player's turn (default 0) then you can 
    parse input commands like play or draw (or later uno), that call the deck 
    methods play_card_2 and draw_card_2 respectively; if the turn is not the 
    human player's turn, then turn represents the AI's number, which you can 
    use to call ai_turn (with the turn number as the ai_num argument). 
    Remember to increment game.turns (with mod so it loops) after each turn.

    I've also made a check_piles that deals with combining the piles into a 
    new draw pile when it gets depleted. This can be called at the start or end 
    of each turn in main. I've also made a check_win that returns a list 
    containing the winning player's number; if the list is empty, then no one 
    has won. This can be called at the end of each turn in main to determine 
   whether the game has to continue or if someone has won.*)
