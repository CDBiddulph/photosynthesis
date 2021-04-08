open OUnit2
include Game

let test_light_points name player_id game expected_lp =
  name >:: fun _ ->
  assert_equal expected_lp
    (player_id |> player_of game |> Player.light_points)
    ~printer:string_of_int

let plant_pipeline plant coord game = place_plant game plant coord

let rec iter_turns n game =
  match n with 0 -> game | n -> game |> end_turn |> iter_turns (n - 1)

let game4 =
  init_game 4 Board.Normal
  |> plant_pipeline
       (Plant.init_plant 1 'x' ANSITerminal.Green Plant.Medium)
       { col = 4; diag = 6 }
  |> end_turn
  |> plant_pipeline
       (Plant.init_plant 2 'x' ANSITerminal.Green Plant.Small)
       { col = 2; diag = 1 }
  |> plant_pipeline
       (Plant.init_plant 2 'x' ANSITerminal.Green Plant.Medium)
       { col = 3; diag = 1 }
  |> iter_turns 3

let player_tests =
  [
    test_light_points "zero turns P1" 1 game4 2;
    test_light_points "zero turns P2" 2 game4 2;
    (* test_light_points "three turns" 1 (iter_turns 3 game4) 0;
       test_light_points "four turns" 1 (iter_turns 4 game4) 2;
       test_light_points "eight turns" 1 (iter_turns 8 game4) 4; *)
  ]

let suite = "test suite for Game" >::: List.flatten [ player_tests ]

let test = run_test_tt_main suite
