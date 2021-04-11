open OUnit2
include Game

let test_light_points name player_id game expected_lp =
  name >:: fun _ ->
  assert_equal expected_lp
    (player_id |> player_of game |> Player.light_points)
    ~printer:string_of_int

let seed_pipeline player_id coord game = plant_seed game player_id coord

let grow_pipeline player_id coord game = grow_plant game player_id coord

let rec iter_turns n game =
  match n with 0 -> game | n -> game |> end_turn |> iter_turns (n - 1)

let game4 =
  init_game 4 Board.Normal
  |> seed_pipeline 1 { col = 4; diag = 6 }
  |> grow_pipeline 1 { col = 4; diag = 6 }
  |> end_turn
  |> seed_pipeline 2 { col = 2; diag = 1 }
  |> grow_pipeline 2 { col = 2; diag = 1 }
  |> iter_turns 7

let player_tests =
  [
    test_light_points "zero turns P1" 1 game4 0;
    test_light_points "zero turns P2" 2 game4 0;
    test_light_points "three turns P1" 1 (iter_turns 3 game4) 0;
    test_light_points "four turns P1" 1 (iter_turns 4 game4) 1;
    test_light_points "eight turns P1" 1 (iter_turns 8 game4) 2;
  ]

let suite = "test suite for Game" >::: List.flatten [ player_tests ]

let test = run_test_tt_main suite
