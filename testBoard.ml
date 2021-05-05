open OUnit2
open Board

let c00 : HexUtil.coord = { col = 0; diag = 0 }

let c33 : HexUtil.coord = { col = 3; diag = 3 }

let empty_normal = init_board Normal

let with_1small_33 =
  testing_init_board Normal
    [ Cell.init_cell 1 (Some (Plant.init_plant 1 Plant.Small)) c33 ]

let with_1medium_33 =
  testing_init_board Normal
    [ Cell.init_cell 1 (Some (Plant.init_plant 1 Plant.Medium)) c33 ]

let with_1large_33 =
  testing_init_board Normal
    [ Cell.init_cell 1 (Some (Plant.init_plant 1 Plant.Large)) c33 ]

let can_plant_seed_test
    (name : string)
    (player_id : PlayerId.t)
    (coord : HexUtil.coord)
    (board : t)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (can_plant_seed player_id coord board)
    ~printer:string_of_bool

let can_plant_seed_tests =
  [
    can_plant_seed_test "seed on empty board" 1 c00 empty_normal false;
    can_plant_seed_test "seed with small 33 dir 0" 1
      { col = 4; diag = 4 } with_1small_33 true;
    can_plant_seed_test "seed with small 33 dir 1" 1
      { col = 3; diag = 4 } with_1small_33 true;
    can_plant_seed_test "seed with small 33 dir 2" 1
      { col = 2; diag = 3 } with_1small_33 true;
    can_plant_seed_test "seed with small 33 dir 3" 1
      { col = 2; diag = 2 } with_1small_33 true;
    can_plant_seed_test "seed with small 33 dir 4" 1
      { col = 3; diag = 2 } with_1small_33 true;
    can_plant_seed_test "seed with small 33 dir 5" 1
      { col = 4; diag = 3 } with_1small_33 true;
    can_plant_seed_test "seed with small 33 dir 0" 2
      { col = 2; diag = 3 } with_1small_33 false;
    can_plant_seed_test "seed with medium 33 dir 0 dist 1" 1
      { col = 4; diag = 4 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 1 dist 1" 1
      { col = 3; diag = 4 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 2 dist 1" 1
      { col = 2; diag = 3 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 3 dist 1" 1
      { col = 2; diag = 2 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 4 dist 1" 1
      { col = 3; diag = 2 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 5 dist 1" 1
      { col = 4; diag = 3 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 0 dist 2" 1
      { col = 5; diag = 5 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 01 dist 2" 1
      { col = 4; diag = 5 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 1 dist 2" 1
      { col = 3; diag = 5 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 12 dist 2" 1
      { col = 2; diag = 4 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 2 dist 2" 1
      { col = 1; diag = 3 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 23 dist 2" 1
      { col = 1; diag = 2 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 3 dist 2" 1
      { col = 1; diag = 1 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 34 dist 2" 1
      { col = 2; diag = 1 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 4 dist 2" 1
      { col = 3; diag = 1 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 45 dist 2" 1
      { col = 4; diag = 2 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 5 dist 2" 1
      { col = 5; diag = 3 } with_1medium_33 true;
    can_plant_seed_test "seed with medium 33 dir 50 dist 2" 1
      { col = 5; diag = 4 } with_1medium_33 true;
  ]

let suite =
  "test suite for Board" >::: List.flatten [ can_plant_seed_tests ]

let test = run_test_tt_main suite
