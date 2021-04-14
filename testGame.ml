open OUnit2
open TestUtil
include Game

let init_game_test = init_game

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
  init_game 4 Board.Normal
  |> plant_small 1 { col = 0; diag = 0 }
  |> end_turn
  |> plant_small 2 { col = 1; diag = 0 }
  |> grow_plant 2 { col = 1; diag = 0 }
  |> end_turn
  |> plant_small 3 { col = 3; diag = 0 }
  |> grow_plant 3 { col = 3; diag = 0 }
  |> end_turn
  |> plant_small 4 { col = 0; diag = 3 }
  |> grow_plant 4 { col = 0; diag = 3 }
  |> iter_turns 4

let game4 = game4_almost_done |> end_turn

let player_tests =
  [
    test_light_points "-1 turn" game4_almost_done [ 0; 0; 0; 0 ];
    test_light_points "0 turns" game4 [ 1; 2; 0; 2 ];
    test_light_points "4 turns" (iter_turns 4 game4) [ 2; 2; 0; 0 ];
    test_light_points "8 turns" (iter_turns 8 game4) [ 4; 4; 0; 0 ];
  ]

let suite = "test suite for Game" >::: List.flatten [ player_tests ]

let player_params =
  [
    (1, ('s', ANSITerminal.Green));
    (2, ('c', ANSITerminal.Red));
    (3, ('x', ANSITerminal.Blue));
    (4, ('o', ANSITerminal.Yellow));
  ]

let gui = Gui.init_gui (Game.cells game4) player_params

let to_render = false

let test =
  if to_render then Gui.render gui;
  run_test_tt_main suite
