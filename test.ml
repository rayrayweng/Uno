open OUnit2
open Deck
open Command


let deck_tests = 
  let numplayers = 2 in 
  let deck_state = init numplayers in 
  let draw_pile = deck_state.draw_pile in 
  let discard_pile = deck_state.discard_pile in 
  let player_hands = deck_state.player_hands in 
  let player_1 = List.hd player_hands in 
  let card = List.hd player_1 in 
  let game_state = play_card card player_1 [] [card] in 
  let new_discard = game_state.discard_pile in 
  let new_hand = game_state.player_hand in 
  let draw_state = draw_card draw_pile new_hand in 
  let drawn_deck = draw_state.draw_pile in 
  let drawn_hand = draw_state.player_hand in 
  let game = {draw_pile = []; 
              discard_pile = []; 
              player_hands = [[]];
              turn=0; reversed = false; scores = []} in 
  let new_state = draw_card_2 deck_state 0 [] 0 in 
  let user = List.hd new_state.player_hands in 
  let playerHand2 = [{color = Blue; card_type = Number(7)}; 
                     {color = Red; card_type = Special(Skip)};
                     {color = Red; card_type = Number(3)}] in
  let top_card1 = {color = Red; card_type = Number(2)} in
  let top_card2 = {color = Green; card_type = Number(7)} in
  let top_card3 = {color = Black; card_type = Number(1)} in
  let wild_card_hand = [{color = Green; card_type = Special(Wild)}] in 
  let ai_draw_pile = [{color = Green; card_type = Number(5)}] in
  let ai_discard_pile = [{color = Blue; card_type = Number(5)}] in
  let _ = (numplayers-1)/4 + 1 in 
  [
    "drawlength" >:: (fun _ -> assert_equal 94 (List.length draw_pile) 
                         ~printer:string_of_int);
    "discardlength" >:: (fun _ -> assert_equal 1 (List.length discard_pile));
    "handlength" >:: (fun _ -> assert_equal 7 (List.length player_1));
    "newdiscardlength" >:: (fun _ -> assert_equal 2 (List.length new_discard));
    "newhandlength" >:: (fun _ -> assert_equal 6 (List.length new_hand) 
                            ~printer:string_of_int);
    "drawdecklength" >:: (fun _ -> assert_equal 93 (List.length drawn_deck) 
                             ~printer:string_of_int);
    "drawhandlength" >:: (fun _ -> assert_equal 7 (List.length drawn_hand) 
                             ~printer:string_of_int);
    "win" >:: (fun _ -> assert_equal [0] (check_win game 0));
    "draw2" >:: (fun _ -> assert_equal 8 (List.length user));
    "findcardbycolor" >:: (fun _ -> assert_equal 
                              ({color = Red; card_type = Special(Skip)})
                              (find_card_by_color Red playerHand2));
    "findcardbytype" >:: (fun _ -> assert_equal 
                             ({color = Red; card_type = Special(Skip)})
                             (find_card_by_type (Special(Skip)) playerHand2));
    "findplayablecards1" >:: (fun _ -> assert_equal 
                                 ([{color = Red; card_type = Number(3)};
                                   {color = Red; card_type = Special(Skip)}])
                                 (find_playable_cards top_card1 playerHand2));
    "findplayablecards2" >:: (fun _ -> assert_equal 
                                 ([{color = Blue; card_type = Number(7)}])
                                 (find_playable_cards top_card2 playerHand2));
    "findplayablecards3" >:: (fun _ -> assert_equal ([]) 
                                 (find_playable_cards top_card3 playerHand2));

    "findwildcard" >:: (fun _ -> assert_equal 
                           {color = Green; card_type = Special(Wild)}
                           (find_wild_card wild_card_hand));

    "ai_play" >:: (fun _ -> assert_equal 
                      ({draw_pile = ai_draw_pile; 
                        discard_pile = [{color = Blue; card_type = Number(7)}; 
                                        {color = Blue; card_type = Number(5)}]; 
                        player_hand = [{color = Red; card_type = Special(Skip)};                                                                                                                         
                                       {color = Red; card_type = Number(3)}]})
                      (ai_play ai_draw_pile ai_discard_pile playerHand2));

  ]

let command_tests = 
  [
    "parseTest1" >:: (fun _ -> assert_equal Draw (parse "draw"));
    "parseTest2" >:: (fun _ -> assert_equal Draw (parse "   draw   "));
    "parseColor" >:: (fun _ -> assert_equal Red (parse_color "  red  "));
  ]

(*let main_tests =
  [
    "parseSpecial" >:: (fun _ -> assert_equal "skip" (parsespecial Skip));
    "parseCard" >:: (fun _ -> assert_equal "red 7" 
            (parsecard {color = Red; card_type = Number(7)}));
    "parseHand" >:: (fun _ -> assert_equal "yellow 4, red 5" (parsehand 
            ([{color = Yellow; card_type = Number(4)}; 
            {color = Red; card_type = Number(5)}])""));
    "convertColor1" >:: (fun _ -> assert_equal (Some Red) (convertcolor "red"));
    "convertType1" >:: (fun _ -> assert_equal 
            (Some(Special Skip)) (converttype "skip"));
    "convertType2" >:: (fun _ -> assert_equal 
            (Some(Number 5)) (converttype "5"));
    "convertCard" >:: (fun _ -> assert_equal 
                          {color = Red; card_type = Number(3)}
                          (convertcard ["red 3"]));
  ]*)

let suite = 
  "test suite" >::: List.flatten [
    deck_tests; command_tests;  (*main_tests*)
  ]

let _ = run_test_tt_main suite 
