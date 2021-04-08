open OUnit2
include Game

let test_light_points name player_id actual_game expected_game =
  name >:: fun _ ->
  assert_equal
    (player_id |> player_of actual_game |> Player.light_points)
    (player_id |> player_of expected_game |> Player.light_points)
    ~printer:string_of_int

let base_plant = Plant.init_plant 1 'x' ANSITerminal.Green Plant.Seed

let game4 = init_game 4 Board.Normal

let game4 = place_plant game4 base_plant { col = 3; diag = 3 }

let rec iter_turns n game =
  match n with 0 -> game | n -> game |> end_turn |> iter_turns (n - 1)

let player_tests =
  [
    test_light_points "zero turns" 1 game4 game4;
    test_light_points "one turn" 1 game4 (iter_turns 1 game4);
  ]

let suite = "test suite for Game" >::: List.flatten [ player_tests ]

let test = run_test_tt_main suite
