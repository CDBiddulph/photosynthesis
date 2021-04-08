open OUnit2
open Game

let test_players name actual_game expected_game =
  name >:: fun _ -> assert_equal actual_game expected_game

(* ~printer: *)
(* ~cmp:(fun g1 g2 -> Game.player_of g1) *)

let base_game4 = init_game 4 Board.Normal

let player_tests = [ test_players "no change" base_game4 base_game4 ]

let suite = "test suite for Game" >::: List.flatten [ player_tests ]

let test = run_test_tt_main suite
