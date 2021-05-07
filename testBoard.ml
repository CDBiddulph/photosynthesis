open OUnit2
open Board

let c00 : HexUtil.coord = { col = 0; diag = 0 }

let c04 : HexUtil.coord = { col = 0; diag = 4 }

let c33 : HexUtil.coord = { col = 3; diag = 3 }

let empty_normal = init_board Normal

let with_1seed_00 =
  testing_init_board Normal
    [ Cell.init_cell 1 (Some (Plant.init_plant 1 Plant.Seed)) c00 ]

let with_1small_33 =
  testing_init_board Normal
    [ Cell.init_cell 1 (Some (Plant.init_plant 1 Plant.Small)) c33 ]

let with_1medium_33 =
  testing_init_board Normal
    [ Cell.init_cell 1 (Some (Plant.init_plant 1 Plant.Medium)) c33 ]

let with_1large_33 =
  testing_init_board Normal
    [ Cell.init_cell 1 (Some (Plant.init_plant 1 Plant.Large)) c33 ]

(** TODO *)
let getter_1arg_test (name : string) (board : Board.t) f expected_output
    : test =
  name >:: fun _ -> assert_equal expected_output (f board)

(** TODO *)
let getter_2arg_test (name : string) arg1 arg2 f expected_output : test
    =
  name >:: fun _ -> assert_equal expected_output (f arg1 arg2)

(** TODO *)
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

(** TODO *)
let can_plant_small_test
    (name : string)
    (coord : HexUtil.coord)
    (board : t)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (can_plant_small coord board)
    ~printer:string_of_bool

let getter_1arg_tests =
  [
    getter_1arg_test "get map" empty_normal Board.map
      (HexMap.init_map ());
    getter_1arg_test "get ruleset" empty_normal Board.ruleset
      Board.Normal;
    getter_1arg_test "get cells" empty_normal Board.cells
      (HexMap.flatten (HexMap.init_map ()));
  ]

let getter_2arg_tests =
  [
    getter_2arg_test "get cell at 00 empty map" c00 empty_normal
      Board.cell_at
      (Some (Cell.init_cell 1 None c00));
    getter_2arg_test "get cell at 33 with large tree" c33 with_1large_33
      Board.cell_at
      (Some
         (Cell.init_cell 1 (Some (Plant.init_plant 1 Plant.Large)) c33));
    getter_2arg_test "valid coord of valid" c00 empty_normal
      Board.valid_coord true;
    getter_2arg_test "valid coord of invalid" c04 empty_normal
      Board.valid_coord false;
    getter_2arg_test "plant at 00 empty" c00 empty_normal Board.plant_at
      None;
    getter_2arg_test "plant at 04 invalid" c04 empty_normal
      Board.plant_at None;
    getter_2arg_test "plant at 33 medium" c33 with_1medium_33
      Board.plant_at
      (Some (Plant.init_plant 1 Plant.Medium));
  ]

let can_plant_tests =
  [
    can_plant_seed_test "seed on invalid coord" 1 c04 empty_normal false;
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
    can_plant_small_test "small with empty board at 00" c00 empty_normal
      true;
    can_plant_small_test "small on invalid coord" c04 empty_normal false;
    can_plant_small_test "small with empty board at 33" c33 empty_normal
      false;
    can_plant_small_test "small with seed at 00" c00 with_1seed_00 false;
  ]

let suite =
  "test suite for Board"
  >::: List.flatten
         [ getter_1arg_tests; getter_2arg_tests; can_plant_tests ]

let test = run_test_tt_main suite
