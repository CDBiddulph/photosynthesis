open OUnit2
include Game

let test_light_points name player_id game expected_lp =
  name >:: fun _ ->
  assert_equal expected_lp
    (player_id |> player_of game |> Player.light_points)
    ~printer:string_of_int

let rec iter_turns n game =
  match n with 0 -> game | n -> game |> end_turn |> iter_turns (n - 1)

let game4 =
  init_game 4 Board.Normal
  |> plant_small 1 { col = 0; diag = 0 }
  |> grow_plant 1 { col = 0; diag = 0 }
  |> end_turn
  |> plant_small 2 { col = 0; diag = 3 }
  |> grow_plant 2 { col = 0; diag = 3 }
  |> iter_turns 7

let player_tests =
  [
    test_light_points "zero turns P1" 1 game4 0;
    test_light_points "zero turns P2" 2 game4 0;
    test_light_points "three turns P1" 1 (iter_turns 3 game4) 0;
    test_light_points "four turns P1" 1 (iter_turns 4 game4) 2;
    test_light_points "eight turns P1" 1 (iter_turns 8 game4) 4;
  ]

let suite = "test suite for Game" >::: List.flatten [ player_tests ]

let test = run_test_tt_main suite
