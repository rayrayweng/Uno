open Deck
open Pervasives
open Command
open Ai

exception NoPlayers of string
exception InvalidCard

(** [calculate_score player_hands acc] is the scores for [player_hands] *)
let rec calculate_score ?a:(acc=0) player_hands =
  let rec calculate player_hand acc =
    match player_hand with 
    | [] -> acc
    | card::tl -> 
      match card.card_type with 
      | Number x -> calculate tl (acc + x)
      | Special D2 -> calculate tl (acc + 20)
      | Special Reverse -> calculate tl (acc + 20)
      | Special Skip -> calculate tl (acc + 20)
      | Special _ -> calculate tl (acc+50)
  in
  match player_hands with
  | [] -> acc
  | h::t -> calculate_score ~a:((calculate h 0) + acc) t

(** [print_scores acc scores] prints each of the values in [scores] *)
let rec print_scores ?a:(acc=0) scores =
  match scores with 
  | [] -> ()
  | h::t -> print_endline ("Player " ^ (string_of_int acc) ^ " has " ^ (string_of_int h) ^ " points!");
    print_scores ~a:(acc+1) t

(** [add_score scores player_num score] adds [score] to the value in the 
    position [player_num] in [scores] *)
let rec add_score ?a:(acc=[]) scores player_num score =
  match player_num with
  | 0 -> (List.rev acc) @ ((score + List.hd scores)::(List.tl scores))
  | x -> add_score ~a:((List.hd scores )::acc) (List.tl scores) (player_num - 1) score

(** [parsespecial spec] converts a special card type to a string *)
let parsespecial spec = 
  match spec with 
  | None -> "none"
  | Skip -> "skip"
  | Reverse -> "reverse"
  | D2 -> "draw2" 
  | D4 -> "draw4"
  | Wild -> "wild"
  | Swap -> "swap"

(** [parsecard card] converts a card record to a string representation *)
let parsecard card = 
  let color = match card.color with 
    | Red -> "red" 
    | Blue -> "blue"
    | Green -> "green"
    | Yellow -> "yellow"
    | Black -> "black" 
  in let special = match card.card_type with
      | Number num -> string_of_int num
      | Special spec -> parsespecial spec
  in String.concat " " [color; special] 

(** [parsehand hand parsed] takes a hand and parses it into a string, where
    parsed is the string representation of the hand already processed*)
let rec parsehand hand parsed = 
  match hand with
  | [] -> parsed 
  | h::t -> parsehand t (String.concat ", " [(parsecard h); parsed])

(** [ansi_color color] is the ANSITerminal.style version of [color] *)
let ansi_color color =
  match color with
  | Red -> ANSITerminal.red
  | Blue -> ANSITerminal.blue 
  | Green -> ANSITerminal.green 
  | Yellow -> ANSITerminal.yellow
  | Black -> ANSITerminal.white

(** [convertcolor color] takes a string and converts it to a color option. 
    If the string is invalid, it returns [None]. If it is valid, it returns
    [Some (color)]*)
let convertcolor color = 
  match color with 
  | "red" -> Some Red
  | "blue" -> Some Blue
  | "green" -> Some Green
  | "yellow" -> Some Yellow
  | "black" -> Some Black
  | _ -> None

(** [converttype str] takes a string and converts it to a card type option
    If the string is invalid, it returns [None]. If it is valid, it returns
    [Some (card type)] *)
let converttype str = 
  match str with
  | "none" -> Some (Special None)
  | "skip" -> Some (Special Skip)
  | "reverse" -> Some (Special Reverse)
  | "draw2" -> Some (Special D2)
  | "draw4" -> Some (Special D4)
  | "wild" -> Some (Special Wild)
  | "swap" -> Some (Special Swap)
  | num -> let n = int_of_string_opt num in 
    match n with
    | None -> None
    | Some number -> if number >= 0 && number <= 9 
      then Some (Number number) else None

(** [convertcard query] takes a string list [query] and converts it to a card
    record defined in deck. 
    Raises: InvalidCard if [query] is not correctly formatted *)
let convertcard query = 
  if List.length query <> 2 then raise InvalidCard else
    let color = match convertcolor (List.hd query) with 
      | None -> raise InvalidCard
      | Some c -> c in 
    let cardtype = match converttype (List.hd (List.tl query)) with 
      | None -> raise InvalidCard
      | Some c -> c in 
    {color = color; card_type = cardtype}

(** [numcards hands str acc] returns the number of cards in each hand of [hands]*)
let rec numcards hands str acc = 
  match hands with 
  | [] -> str
  | h::t -> numcards t (String.concat " "  [str;"\nPlayer"; string_of_int acc; "has";
                                            string_of_int (List.length h); "cards"]) (acc+1) 

(** [change_turn_num state] changes turn of the game, incremeting or decrementing
    it depending on how many reverse cards were played *)
let change_turn_num state : game= 
  let turn = state.turn in 
  let hands = state.player_hands in
  let numplayers = List.length hands in 
  if state.reversed then 
    let backwards = turn - 1 in
    {state with turn = (if backwards = -1 then numplayers-1 else backwards)}
  else {state with turn = Pervasives.(mod) (turn+1) numplayers}

(** [get_hand hands num] returns the hand corresponding to the index [num]
    in [hands]*)
let rec get_hand hands num = 
  if num = 0 then List.hd hands else get_hand (List.tl hands) (num-1)

(** [general_draw gamest turn times] gives player [turn] a number [times] cards*)
let rec general_draw gamest turn times = 
  let newgame = draw_card_2 gamest turn [] 0 in 
  let drawn = List.hd (get_hand newgame.player_hands turn) in 
  let card_reveal = (if turn = 0 then parsecard drawn else "") in 
  let _ = print_string ("Player " ^ string_of_int turn ^ " drew "); in
  let _ = ANSITerminal.(print_string [ansi_color drawn.color] ((card_reveal) ^ "\n")); in 
  if times = 1 then newgame else general_draw newgame turn (times-1)

(** [change_color color state] changes the color of a wild or draw4 card
    to the one specified by the user *)
let rec change_color ?c:(color=Black) (state:game) : game =
  let color_to_string col =
    if col = Red then "red"
    else if col = Blue then "blue"
    else if col = Green then "green"
    else if col = Yellow then "yellow" 
    else "black"
  in
  let discard = state.discard_pile in 
  let first = List.hd discard in
  if color != Black then 
    (let wild = {first with color = color} in 
     print_string ("Color changed to ");
     ANSITerminal.(print_string [ansi_color color] ((color_to_string color) ^ "\n"));
     {state with discard_pile = wild::(List.tl discard)})
  else
    (print_endline "Enter a color to change to: ";
     print_string "> "; 
     let input = read_line() in 
     match (parse_color input) with 
     | (exception Empty) -> print_endline "Enter a valid command\n";
       change_color ~c:color state
     | (exception Malformed) -> print_endline "Bad command\n";
       change_color ~c:color state
     | c -> if c != Black then let wild = {first with color = c} in 
         print_string ("Color changed to ");
         ANSITerminal.(print_string [ansi_color c] ((color_to_string c) ^ "\n"));
         {state with discard_pile = wild::(List.tl discard)}
       else state)

(** [swap curent_player chosen_player state] takes a player to swap hands with
    and swaps the two hands of [current_player] and [chosen_player]*)
let rec swap current_player ?c:(chosen_player=(-1)) state =
  let player_hands = state.player_hands in 
  let current_player_hand = List.nth player_hands current_player in
  let rec swap1 ?c:(current=0) ?a:(acc=[]) curr_h chos_h chosen player_hands =
    match player_hands with 
    | [] -> {state with player_hands = (List.rev acc)}
    | h::t -> 
      if current = current_player then swap1 ~c:(current+1) ~a:(chos_h::acc) curr_h chos_h chosen t
      else if current = chosen then swap1 ~c:(current+1) ~a:(curr_h::acc)curr_h chos_h chosen t
      else swap1 ~c:(current+1) ~a:(h::acc) curr_h chos_h chosen t in
  if chosen_player = -1 then
    let _ = print_endline "Enter a player to swap with: " in
    let _ = print_string "> " in
    match read_line() with 
    | (exception Empty) -> print_endline "Enter a valid player number\n";
      swap current_player state
    | (exception Malformed) -> print_endline "Bad command\n";
      swap current_player state
    | num -> match int_of_string_opt num with 
      | None -> (print_endline "Enter a valid player number\n";
                 swap current_player state)
      | Some x -> if x < 1 || x > ((List.length state.player_hands) - 1) then
          (print_endline "Enter a valid player number\n";
           swap current_player state)
        else swap1 current_player_hand (List.nth player_hands (int_of_string num)) (int_of_string num) player_hands
  else let chosen_player_hand = List.nth player_hands chosen_player in 
    swap1 current_player_hand chosen_player_hand chosen_player player_hands

(** [process_special ai current_player chosen_player state] processes and 
    produces special cards (i.e wild, draw2) to the desired effect 
    as necessitated by the rules of Uno *)
let process_special ?a:(ai=[{color=Black; card_type=Number 0}]) ?c:(current_player=0) ?c:(chosen_player=0) (state : game) : game = 
  let top = List.hd state.discard_pile in 
  let turn = state.turn in 
  let hands = state.player_hands in
  let numplayers = List.length hands in 
  match top.card_type with 
  | Number n -> state
  | Special None -> state
  | Special Skip -> 
    print_endline 
      (String.concat " " ["Player"; string_of_int turn; "has been skipped"]); 
    change_turn_num state
  | Special Reverse -> 
    if numplayers = 2 then change_turn_num state else
      let reversed = state.reversed in 
      let changed = change_turn_num {state with reversed = not reversed} in 
      if (List.length state.discard_pile) = 1 then changed else change_turn_num changed
  | Special D2 -> change_turn_num (general_draw state turn 2)
  | Special D4 -> let result = change_turn_num (general_draw state turn 4) in
    if ai = [{color=Black; card_type=Number 0}] then change_color result
    else change_color ~c:(best_color ai) result
  | Special Wild -> if ai = [{color=Black; card_type=Number 0}] then change_color state
    else change_color ~c:(best_color ai) state
  | Special Swap -> 
    if current_player = 0 then 
      let state = swap current_player state in change_color state
    else let color = random_color_except (best_color ai) in 
      let state = swap current_player ~c:(best_player current_player state.player_hands) state in change_color ~c:(color) state

(** [parse_line gamest] handles and executes the command given from the 
    terminal. It changes the game state [gamest] according to specifications.*)
let rec parse_line gamest = 
  let play_card card (uno:bool) = 
    (match convertcard card with 
     | (exception InvalidCard) -> print_endline "Enter a valid card\n";
       if uno then raise InvalidCard else parse_line gamest
     | orig -> match play_card_2 orig gamest 0 [] 0 with 
       | (exception InvalidPlayCard) -> print_endline "Card cannot be played\n";
         if uno then raise InvalidCard else parse_line gamest
       | newstate ->  
         let _ = print_string ("Card played was "); in
         let card = List.hd newstate.discard_pile in
         let _ = ANSITerminal.(print_string [ansi_color card.color] ((parsecard card) ^ "\n")); in
         process_special (change_turn_num newstate)) in
  print_string "> ";
  let turn = gamest.turn in
  let input = read_line() in 
  match (parse input) with 
  | (exception Empty) -> print_endline "Enter a valid command\n";
    parse_line gamest
  | (exception Malformed) -> print_endline "Bad command\n";
    parse_line gamest
  | Play card -> let newstate = play_card card false in 
    let numcards = List.length (List.hd (newstate.player_hands)) in 
    if numcards = 1 then (print_endline "You forgot to call uno!"; (general_draw newstate turn 4))
    else newstate
  | Draw -> change_turn_num (general_draw gamest turn 1)
  | NumCards -> print_endline (numcards gamest.player_hands "" 0);
    parse_line gamest
  | Uno card -> 
    let newstate = try play_card card true with
      | InvalidCard -> gamest
    in
    let numcards = List.length (List.hd (newstate.player_hands)) in 
    if numcards <> 1 && newstate<>gamest
    then (print_endline "You called uno at the wrong time!"; (general_draw newstate turn 1))
    else if newstate = gamest then parse_line gamest
    else newstate
  | Turn -> change_turn_num gamest
  | Quit -> exit 0 

(** [turn_play gamest] changes the turn for each player. It also handles the 
    AI turns and changes the game state [gamest] accordingly*)
let rec turn_play (gamest:game) (start:bool) = 
  let winners = check_win gamest 0 in 
  if List.length winners <> 0 
  then let _ = print_endline (String.concat " " ["Player"; (string_of_int (List.hd winners)); "has won!"])in 
    let winner_score = calculate_score gamest.player_hands in 
    let scores = add_score gamest.scores (List.hd winners) winner_score in
    let gamest = {gamest with scores = scores} in
    let _ = print_scores gamest.scores in
    let _ = print_endline "Would you like to play again?" in 
    let input = String.trim (read_line()) in 
    (if input = "yes" then (turn_play ({(init (List.length gamest.player_hands)) with scores = scores}) true) else exit 0)
  else let hands = gamest.player_hands in 
    let turn = gamest.turn in
    if turn = 0 then let _ = print_string ("\nTop card is "); in
      let card = List.hd gamest.discard_pile in
      let _ = ANSITerminal.(print_string [ansi_color card.color] ((parsecard card) ^ "\n")); in
      let rec print_hand hand =
        match hand with
        | [] -> ()
        | hd::tl -> 
          if List.length tl > 0 then let _ = ANSITerminal.(print_string [ansi_color hd.color] ((parsecard hd) ^ ", ")); in print_hand tl
          else let _ = ANSITerminal.(print_string [ansi_color hd.color] ((parsecard hd) ^ "\n")); in print_hand tl
      in
      (print_string "\nHand is ";
       print_hand (List.hd hands);
       (*print_endline (String.concat " " ["Hand1 is"; parsehand (List.hd (List.tl hands)) ""]); *)
       if start then match (List.hd (gamest.discard_pile)).card_type with
         | Number x -> turn_play (parse_line gamest) false
         | Special D4 -> turn_play (change_color gamest) false
         | Special _ -> turn_play (process_special gamest) false
       else turn_play (parse_line gamest) false)
    else let ai_turn1 = match (Ai.ismcts gamest 500) with
        | None -> failwith "shit"
        | Some a -> Ai.do_move a gamest in
      (* let ai_turn1 = change_turn_num (ai_turn gamest turn [] 0) in  *)
      let start_hand = get_hand hands turn in
      let end_hand = get_hand ai_turn1.player_hands turn in
      let end_length = List.length end_hand in 
      if end_length >= List.length start_hand then 
        (print_endline (String.concat " " ["Player"; string_of_int turn; "drew"]);
         (turn_play ai_turn1) false) 
      else let card = (List.hd ai_turn1.discard_pile) in
        let _ = print_string ("Player " ^ string_of_int turn ^ " played "); in
        let _ = ANSITerminal.(print_string [ansi_color card.color] ((parsecard card) ^ "\n")); 
        in
        if end_length = 1 then print_endline (String.concat " " ["Player"; string_of_int turn; "called Uno!";]);
        let spec_process = process_special ~a:end_hand ~c:turn ai_turn1 in 
        turn_play spec_process false

(* [play_game n] starts the game with [n] players. *)
let play_game n = 
  ANSITerminal.(print_string [yellow]
                  "\nCommands: play [color] [cardtype]: play a card onto the board 
                  draw : draw a card 
                  numcards : check number of cards in each hand
                  uno [color] [cardtype]: play a card and call uno 
                  quit : quit the game \n");
  let new_game = Deck.init n in
  let _ = turn_play new_game true in () 

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [yellow]
                  "\nWelcome to Uno!\n");
  print_endline "Please enter the number of players.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | number -> if int_of_string number > 0 then play_game (int_of_string number) 
    else raise (NoPlayers number)

(* Execute the game engine. *)
let () = main ()
