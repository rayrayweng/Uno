open Deck
type aimove = Play of Deck.card | Draw
type node = {move:aimove option; mutable parent:node option; 
             mutable children:node list; mutable wins:int; 
             mutable visits:int; mutable avails:int; 
             recentplayer:int option}
type tree = {nodelist: node list}

(* Helper function for making a move*)
let move_help move state = 
  match move with
  | Play a -> Deck.play_card_2 a state state.turn [] 0
  | Draw -> Deck.draw_card_2 state state.turn [] 0

(* Changes the turn numbers accordingly*)
let change_turn_num state : game= 
  let turn = state.turn in 
  let hands = state.player_hands in
  let numplayers = List.length hands in 
  if state.reversed then 
    let backwards = turn - 1 in
    {state with turn = (if backwards = -1 then numplayers-1 else backwards)}
  else {state with turn = Pervasives.(mod) (turn+1) numplayers}

(** Makes a move and changes the state accordingly
    Returns the deck game state after making a move*)
let do_move move state = 
  Deck.check_piles (change_turn_num (move_help move state))

(* Returns the other players' card hands
   Initialize with hand_acc = []*)
let rec get_other_hands gam pov hand_acc acc= 
  match gam.player_hands with
  | [] -> hand_acc
  | h::t -> if pov = acc then hand_acc@t 
    else get_other_hands {gam with player_hands = t} pov (hand_acc@[h]) (acc+1)

(* Helper function for combining hands into one list*)
let rec combinehands hands acc = 
  match hands with
  | [] -> acc
  | h::t -> combinehands t (h@acc)

(* Deals [num] number of cards (or as many as possible)*)
let rec dealhand (cards:Deck.card list) num acc =
  match num with
  | 0 -> acc
  | _ -> if List.length cards >0 then dealhand (List.tl cards) (num-1) 
        ((List.tl cards),(List.hd cards)::(snd acc))
    else acc

(* Helper function that deals [cards] to all players except player [pov], who
   keeps their original hand*)
let rec dealcards cards gam pov acc hands_acc= 
  match gam.player_hands with
  | []-> hands_acc
  | h::t -> if acc = pov 
    then dealcards cards {gam with player_hands = t} pov (acc+1) (hands_acc@[h])
    else let dealt = dealhand cards (List.length (h)) (cards,[]) in
      dealcards (fst dealt) {gam with player_hands = t} pov (acc+1) 
        (hands_acc@[snd dealt])

(* Makes a clone of the game state in order to run simulations on it.
   pov is ai player number*)
let make_clone (orig:Deck.game) (pov:int) = 
  let thishand = (List.nth orig.player_hands pov) in
  let seen = thishand @orig.discard_pile in
  let unseen = orig.draw_pile@(combinehands (get_other_hands orig pov [] 0) [])
  in let randcards = Deck.shuffle unseen in
  {orig with player_hands = dealcards randcards orig pov 0 []}

(* Get the game result from the viewpoint of player pov, +1 for win, -1 for 
   loss, 0 for neither*)
let rec get_result gam pov acc= 
  match gam.player_hands with
  | [] -> 0
  | h::t -> if acc = pov 
    then if List.length h = 0 then 1 
      else get_result {gam with player_hands = t} pov (acc+1)
    else if List.length h = 0 then -1 
    else get_result {gam with player_hands = t} pov (acc+1)

(* Get the moves that have been tried at this node*)
let rec get_tried nod acc= 
  match nod.children with
  | [] -> acc
  | h::t -> match h.move with 
    | None -> get_tried {nod with children = t} acc
    | Some a -> get_tried {nod with children = t} (a::acc)

(* Helper function for getting the untried moves
   Call legalmoves = Deck.find_playable_cards, triedmoves = get_tried*)
let rec get_untried legalmoves triedmoves acc= 
  match legalmoves with
  | [] -> acc
  | h::t -> if (List.mem h triedmoves) 
    then get_untried t triedmoves acc
    else get_untried t triedmoves (h::acc)

(* Get the untried moves at this node*)
let get_node_untried node legal = 
  get_untried legal (get_tried node []) []

(* Gets the legal nodes that can be reached from this node*)
let rec get_legal_children children legal acc=
  match children with
  | [] -> acc
  | h::t -> match h.move with
    | None -> get_legal_children t legal acc
    | Some a -> if (List.mem a legal) then get_legal_children t legal (h::acc)
      else get_legal_children t legal acc

(* Helper function to calculate the evaluation function on the node*)
let get_val node ex = 
  ((float_of_int node.wins)/.(float_of_int node.visits) +.
   (ex *.(sqrt (log ((float_of_int node.avails)/.(float_of_int node.visits))))))

(* Returns the arg max node*)
let rec maxchild children currmax currmaxval ex= 
  match children with
  | [] -> currmax
  | h::t -> let thisval = (get_val h ex) in
    if thisval > currmaxval then maxchild t h thisval ex
    else maxchild t currmax currmaxval ex

(* Returns a node list with updated availabilities*)
let rec update_avail (children:node list) legal acc = 
  match children with
  | [] -> acc
  | h::t -> update_avail t legal ({h with avails = (h.avails+1)}::acc)

(* Selection function that picks the next node to visit/explore, based on a function
   that balances exploration and exploitation
   Use exploration = 0.7, has to have at least one legal child*)
let select_child node legal exploration = 
  let legalchildren = get_legal_children node.children legal [] in
  let selected = if List.length legalchildren >0 then maxchild legalchildren 
        (List.hd legalchildren) (get_val (List.hd legalchildren) exploration) 
        exploration
    else failwith "no legal child nodes" in 
  selected.avails <- selected.avails +1;
  ({node with children = (update_avail node.children legalchildren [])},
   selected)

(* Adds a new child node to this node, reached by move mov*)
let add_child nod mov recent= 
  let newnode = {move = mov; parent = Some nod; children = [];wins = 0; 
                 visits = 0; avails = 0; recentplayer = recent} in
  nod.children <- newnode::nod.children;
  newnode

(* Updates node visit numbers and results of simulation*)
let update_node node gam = 
  let newvisits = node.visits+1 in
  match node.recentplayer with
  | None -> node.visits <- newvisits
  | Some a -> node.visits <- newvisits;
    node.wins <- node.wins+(get_result gam a 0)

(* Helper function to get a list of aimoves from cards*)
let rec cards_to_aimoves cards acc = 
  match cards with
  | [] -> acc
  | h::t -> cards_to_aimoves t (Play(h)::acc)

(* Select a new child node to explore if this node is not terminal and all
   possible moves have been done at least once*)
let rec select (state:Deck.game) node =
  if (List.length state.discard_pile) < 1
  then let legalmoves = if List.length state.draw_pile>1 
         then (cards_to_aimoves (List.nth state.player_hands state.turn) [Draw])
         else (cards_to_aimoves (List.nth state.player_hands state.turn) []) in 
    if List.length legalmoves >0 && 
       List.length (get_node_untried node legalmoves) = 0
    then let (a,newnode) = select_child node legalmoves 0.7 in
      let newstate = match newnode.move with
        | None -> failwith "trying to select root node"
        | Some a -> do_move a state in
      select newstate newnode
    else (state,node)
  else 
  if List.length state.discard_pile >0 
  then let legalmoves = if List.length state.draw_pile>1 
         then (cards_to_aimoves 
                 (find_playable_cards (List.hd state.discard_pile) 
                    (List.nth state.player_hands state.turn)) [Draw])
         else (cards_to_aimoves 
                 (find_playable_cards (List.hd state.discard_pile) 
                    (List.nth state.player_hands state.turn)) []) in
    if List.length legalmoves>0 && 
       List.length (get_node_untried node legalmoves) = 0
    then let (a, newnode) = select_child node legalmoves 0.7 in
      let newstate = match newnode.move with
        | None -> failwith "trying to select root node"
        | Some a -> do_move a state in
      select newstate newnode
    else (state, node)
  else failwith "invalid discard pile"

(* Pick a node able to be reached from this node stochastically and explore it*)
let expand state node moves untry= 
  if untry then
    let m = List.nth moves (Random.int (List.length moves)) in
    let player = state.turn in
    ((do_move m state),(add_child node (Some m) (Some player)))
  else (state, node)

(* Simulate the rest of a trial game after picking a move until terminal node*)
let rec simulate state =
  if get_result state state.turn 0 = 0 then
    if (List.length state.discard_pile) > 0 
    then let legalmoves = if List.length state.draw_pile >1 
           then (cards_to_aimoves 
                   (find_playable_cards (List.hd state.discard_pile) 
                      (List.nth state.player_hands state.turn)) [Draw])
           else (cards_to_aimoves 
                   (find_playable_cards (List.hd state.discard_pile) 
                      (List.nth state.player_hands state.turn)) []) in
      if List.length legalmoves >0 
      then simulate (do_move (List.nth legalmoves 
                                (Random.int (List.length legalmoves))) state)
      else state
    else let legalmoves = if List.length state.draw_pile >1 
           then (cards_to_aimoves (List.nth state.player_hands state.turn) 
                   [Draw])
           else (cards_to_aimoves (List.nth state.player_hands state.turn) [])
      in
      if List.length legalmoves>0 
      then simulate (do_move (List.nth legalmoves 
                                (Random.int (List.length legalmoves))) state)
      else state
  else state


(* When terminal node has been reached, backpropagate and update visits and wins
   for previous nodes along the path*)
let rec backprop node state = 
  update_node node state;
  match node.parent with
  | None -> ()
  | Some a -> backprop a state

(* Return the most visited node, indicative of the most optimal move choice at
   this point*)
let rec mostvisited childnodes maxnode maxval = 
  match childnodes with
  | []->maxnode
  | h::t-> if h.visits > maxval then mostvisited t h h.visits
    else mostvisited t maxnode maxval

(**Returns the most promising move found by Monte Carlo Tree Search method with
   imperfect information set.

   Runs a certain number of simulations from this state, and picks the move
   that appears to be the most promising after [maxiter] number of iterations.
   Applying Monte Carlo Tree Search with imperfect information set, balancing 
   picking the best move during simulations with further exploration. At end of
   maxiterations, returns the most promising move*)
let ismcts rootstate maxiter = 
  let rootnode = {move = None; parent = None; children = []; wins = 0; 
                  visits = 0; avails = 0; recentplayer = None} in
  let rec ismctshelp rtnode rtstate itermax acc = 
    if itermax <> acc then
      let newnode = rtnode in
      let state = make_clone rtstate rtstate.turn in
      let (selstate, selnode) = select state newnode in 
      let untriedmoves = if List.length selstate.discard_pile >0 
        then if List.length selstate.draw_pile>1 
          then (get_node_untried selnode 
                  (cards_to_aimoves 
                     (find_playable_cards 
                        (List.hd selstate.discard_pile) 
                        (List.nth selstate.player_hands selstate.turn)) [Draw]))
          else (get_node_untried selnode 
                  (cards_to_aimoves 
                     (find_playable_cards 
                        (List.hd selstate.discard_pile) 
                        (List.nth selstate.player_hands selstate.turn)) []))
        else if List.length selstate.draw_pile>1 
        then (get_node_untried selnode 
                (cards_to_aimoves 
                   (List.nth selstate.player_hands selstate.turn) [Draw]))
        else (get_node_untried selnode 
                (cards_to_aimoves 
                   (List.nth selstate.player_hands selstate.turn) [])) in
      let (expstate, expnode) = expand selstate selnode untriedmoves 
          (List.length untriedmoves >0) in
      let simstate = simulate expstate in
      backprop expnode simstate;
      ismctshelp rtnode simstate itermax (acc+1)
    else if List.length rtnode.children >0 
    then (mostvisited rtnode.children (List.hd rtnode.children) 0).move
    else None in
  ismctshelp rootnode rootstate maxiter 0
