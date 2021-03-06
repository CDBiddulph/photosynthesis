open OUnit2
open TestUtil
open Game

let test_light_points name game expected_lp =
  "light points: " ^ name >:: fun _ ->
  assert_equal expected_lp
    (List.map
       (fun p_id -> p_id |> player_of game |> Player.light_points)
       (player_order game))
    ~printer:(pp_list string_of_int)

let rec iter_turns n game =
  match n with 0 -> game | n -> game |> end_turn |> iter_turns (n - 1)

let game4_almost_done =
  _init_game_test 4
    (Board.testing_init_board Board.Normal
       [
         Cell.init_cell 1
           (Some (Plant.init_plant 1 Plant.Small))
           { col = 0; diag = 0 };
         Cell.init_cell 1
           (Some (Plant.init_plant 2 Plant.Medium))
           { col = 1; diag = 0 };
         Cell.init_cell 1
           (Some (Plant.init_plant 3 Plant.Medium))
           { col = 3; diag = 2 };
         Cell.init_cell 1
           (Some (Plant.init_plant 4 Plant.Medium))
           { col = 3; diag = 0 };
       ])
    1 1 2
    [ (1, []); (2, []); (3, []); (4, []) ]
    0 Game.Normal
  |> iter_turns 7

let game4 = game4_almost_done |> end_turn

let light_points_tests =
  [
    test_light_points "-1 turn" game4_almost_done [ 0; 0; 0; 0 ];
    test_light_points "0 turns" game4 [ 1; 2; 0; 2 ];
    test_light_points "4 turns" (iter_turns 4 game4) [ 2; 4; 0; 4 ];
    test_light_points "8 turns" (iter_turns 8 game4) [ 2; 4; 2; 6 ];
  ]

let rec grow_to_stage player_id coord stage board =
  match Board.plant_at coord board with
  | None ->
      board
      |> Board.plant_seed player_id coord
      |> grow_to_stage player_id coord stage
  | Some p ->
      if Plant.plant_stage p = stage then board
      else
        Board.grow_plant player_id coord board
        |> grow_to_stage player_id coord stage

let testing_cell stage player_id coord soil =
  Cell.init_cell soil (Some (Plant.init_plant player_id stage)) coord

let harvest_cell = testing_cell Plant.Large

let harvest_board () =
  Board.testing_init_board Board.Normal
    [
      harvest_cell 1 { col = 3; diag = 0 } 1;
      harvest_cell 1 { col = 3; diag = 1 } 2;
      harvest_cell 1 { col = 3; diag = 2 } 3;
      harvest_cell 1 { col = 3; diag = 3 } 4;
    ]

let testing_players num_players =
  List.map
    (fun id ->
      let player = Player.init_player id |> Player.add_lp 20 in
      (id, player))
    (PlayerId.generate_player_ids num_players)

let harvest_game sp =
  _init_game_test 2 (harvest_board ()) 1 1 0 sp 0 Game.Normal
  |> Game._update_players_test (testing_players 2)

let test_scoring_points_left name init_sp harvest_soil expected_sp =
  let game =
    harvest { col = 3; diag = harvest_soil - 1 } (harvest_game init_sp)
  in
  "scoring points left: " ^ name >:: fun _ ->
  let soils = [ 1; 2; 3; 4 ] in
  let actual_sp = scoring_points game in
  assert_equal expected_sp
    (List.map (fun soil -> List.assoc soil actual_sp) soils)
    ~printer:(pp_list (pp_list string_of_int))

let test_scoring_points_player name init_sp harvest_soil expected_sp =
  let game =
    harvest { col = 3; diag = harvest_soil - 1 } (harvest_game init_sp)
  in
  "scoring points left: " ^ name >:: fun _ ->
  assert_equal expected_sp
    (List.map
       (fun p_id -> p_id |> player_of game |> Player.score_points)
       (player_order game))
    ~printer:(pp_list string_of_int)

let basic_sp =
  [
    (1, [ 14; 14; 13; 13; 13; 12; 12; 12; 12 ]);
    (2, [ 17; 16; 16; 14; 14; 13; 13 ]);
    (3, [ 19; 18; 18; 17; 17 ]);
    (4, [ 22; 21; 20 ]);
  ]

let almost_empty_sp = [ (1, [ 1 ]); (2, []); (3, []); (4, []) ]

let empty_sp = [ (1, []); (2, []); (3, []); (4, []) ]

let scoring_points_tests =
  [
    test_scoring_points_player "soil 1" basic_sp 1 [ 14; 0 ];
    test_scoring_points_player "soil 2" basic_sp 2 [ 17; 0 ];
    test_scoring_points_player "soil 3" basic_sp 3 [ 19; 0 ];
    test_scoring_points_player "soil 4" basic_sp 4 [ 22; 0 ];
    test_scoring_points_left "soil 3" basic_sp 3
      [
        [ 14; 14; 13; 13; 13; 12; 12; 12; 12 ];
        [ 17; 16; 16; 14; 14; 13; 13 ];
        [ 18; 18; 17; 17 ];
        [ 22; 21; 20 ];
      ];
    test_scoring_points_player "almost_empty" almost_empty_sp 4 [ 1; 0 ];
    test_scoring_points_left "almost_empty" almost_empty_sp 4
      [ []; []; []; [] ];
    test_scoring_points_player "empty" empty_sp 4 [ 0; 0 ];
    test_scoring_points_left "empty" empty_sp 4 [ []; []; []; [] ];
  ]

let suite =
  "test suite for Game"
  >::: List.flatten [ light_points_tests; scoring_points_tests ]

let player_params =
  [
    (1, ('s', ANSITerminal.Green));
    (2, ('c', ANSITerminal.Red));
    (3, ('x', ANSITerminal.Blue));
    (4, ('o', ANSITerminal.Yellow));
  ]

let gui =
  let render_game = iter_turns 8 game4 in
  Gui.init_gui [ []; []; []; [] ] [ 0; 0; 0; 0 ]
    [ None; None; None; None ]
    None false
    (Game.sun_dir render_game)
    0
    (Game.cells render_game)
    player_params

let to_render = false

let test =
  if to_render then Gui.render gui;
  run_test_tt_main suite
