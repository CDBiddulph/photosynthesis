open OUnit2
open Player

let lp_test
    (name : string)
    (f : t -> t)
    (starting_lp : int)
    (expected_lp : int) : test =
  name >:: fun _ ->
  let player = Player.init_player 1 |> add_lp starting_lp in
  assert_equal expected_lp
    (player |> f |> light_points)
    ~printer:string_of_int

let player = Player.init_player 1 |> add_lp 25

let lp_tests =
  [ lp_test "overflow" Fun.id 25 20; 
lp_test "harvest" (harvest 10) 10 6; ]

let suite =
  "test suite for HexMap" >::: List.flatten [ lp_tests ]

let test = run_test_tt_main suite