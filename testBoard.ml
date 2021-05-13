open OUnit2
open Board

let c00 : HexUtil.coord = { col = 0; diag = 0 }

let c04 : HexUtil.coord = { col = 0; diag = 4 }

let c11 : HexUtil.coord = { col = 1; diag = 1 }

let c32 : HexUtil.coord = { col = 3; diag = 2 }

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

(** [getter_1arg_test name board f expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [f board]. *)
let getter_1arg_test (name : string) (board : Board.t) f expected_output
    : test =
  name >:: fun _ -> assert_equal expected_output (f board)

(** [getter_2arg_test name arg1 arg2 f expected_output] constructs an
    OUnit test named [name] that asserts the quality of
    [expected_output] with [f arg1 arg2]. *)
let getter_2arg_test (name : string) arg1 arg2 f expected_output : test
    =
  name >:: fun _ -> assert_equal expected_output (f arg1 arg2)

(** [can_plant_seed_test name player_id coord board expected_output]
    constructs an OUnit test named [name] that asserts the quality of
    [expected_output] with [can_plant_seed player_id coord board]. *)
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

(** [can_plant_small_test name coord board expected_output] constructs
    an OUnit test named [name] that asserts the quality of
    [expected_output] with [can_plant_small coord board]. *)
let can_plant_small_test
    (name : string)
    (coord : HexUtil.coord)
    (board : t)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (can_plant_small coord board)
    ~printer:string_of_bool

(** [plant_test name id coord board stage f] constructs an OUnit test
    named [name] that asserts the quality of
    [Some (Plant.init_plant id stage)] with
    [(plant_at coord (f id coord board))]. *)
let plant_test
    (name : string)
    (id : PlayerId.t)
    (coord : HexUtil.coord)
    (board : t)
    (stage : Plant.plant_stage)
    f =
  name >:: fun _ ->
  assert_equal
    (Some (Plant.init_plant id stage))
    (plant_at coord (f id coord board))

(** [plant_fail name id coord board f] constructs an OUnit test named
    [name] that asserts that [IllegalPlacePlant] is raised at
    [f id coord board]. *)
let plant_fail
    (name : string)
    (id : PlayerId.t)
    (coord : HexUtil.coord)
    (board : t)
    f : test =
  name >:: fun _ ->
  assert_raises IllegalPlacePlant (fun _ -> f id coord board)

(** [can_grow_test name id coord board expected_output] constructs an
    OUnit test named [name] that asserts the quality of
    [expected_output] with [can_grow_plant id coord board]. *)
let can_grow_test
    (name : string)
    (id : PlayerId.t)
    (coord : HexUtil.coord)
    (board : t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (can_grow_plant id coord board)
    ~printer:string_of_bool

(** [grow_test name id coord board next] constructs an OUnit test named
    [name] that asserts the quality of [next] with
    [plant_at coord (grow_plant coord id board)]. *)
let grow_test
    (name : string)
    (id : PlayerId.t)
    (coord : HexUtil.coord)
    (board : t)
    (next : Plant.t option) : test =
  name >:: fun _ ->
  assert_equal next (plant_at coord (grow_plant id coord board))

(** [grow_fail name coord id board] constructs an OUnit test named
    [name] that asserts that [IllegalGrowPlant] is raised at
    [grow_plant coord id board]. *)
let grow_fail
    (name : string)
    (coord : HexUtil.coord)
    (id : PlayerId.t)
    (board : t) : test =
  name >:: fun _ ->
  assert_raises IllegalGrowPlant (fun _ -> grow_plant id coord board)

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

let plant_tests =
  [
    plant_test "plant small 00 empty board" 1 c00 empty_normal
      Plant.Small plant_small;
    plant_fail "plant small 00 occupied" 1 c00 with_1seed_00 plant_small;
    plant_fail "plant small invalid" 1 c04 empty_normal plant_small;
    plant_test "plant seed 32 with small at 33" 1 c32 with_1small_33
      Plant.Seed plant_seed;
    plant_test "plant seed 11 with large at 33" 1 c11 with_1large_33
      Plant.Seed plant_seed;
    plant_fail "plant p2 seed 11 with p1 large at 33" 2 c11
      with_1large_33 plant_seed;
    plant_fail "plant seed 11 out of range" 1 c11 with_1small_33
      plant_seed;
    plant_fail "plant seed invalid" 1 c04 empty_normal plant_seed;
    plant_fail "plant seed 00 occupied" 1 c00 with_1seed_00 plant_seed;
  ]

let can_grow_tests =
  [
    can_grow_test "can grow seed correct id" 1 c00 with_1seed_00 true;
    can_grow_test "can grow small correct id" 1 c33 with_1small_33 true;
    can_grow_test "can grow small wrong id" 2 c33 with_1small_33 false;
    can_grow_test "can grow no plant" 1 c00 with_1small_33 false;
  ]

let grow_tests =
  [
    grow_test "grow seed 00" 1 c00 with_1seed_00
      (Some (Plant.init_plant 1 Plant.Small));
    grow_test "grow small 33" 1 c33 with_1small_33
      (Some (Plant.init_plant 1 Plant.Medium));
    grow_test "grow medium 33" 1 c33 with_1medium_33
      (Some (Plant.init_plant 1 Plant.Large));
    grow_fail "grow wrong id" c00 2 with_1seed_00;
  ]

let suite =
  "test suite for Board"
  >::: List.flatten
         [
           getter_1arg_tests;
           getter_2arg_tests;
           can_plant_tests;
           plant_tests;
           can_grow_tests;
           grow_tests;
         ]

let test = run_test_tt_main suite
