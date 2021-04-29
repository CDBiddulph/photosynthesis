open OUnit2
open HexMap

let cell_at_test
    (name : string)
    (map : t)
    (coord : HexUtil.coord)
    (expected_output : Cell.t option) =
  name >:: fun _ -> assert_equal expected_output (cell_at map coord)

let set_cell_test
    (name : string)
    (map : t)
    (cell : Cell.t option)
    (coord : HexUtil.coord) =
  name >:: fun _ ->
  assert_equal cell (cell_at (set_cell map cell coord) coord)

let set_cell_fail
    (name : string)
    (map : t)
    (cell : Cell.t option)
    (coord : HexUtil.coord) =
  name >:: fun _ ->
  assert_raises (Failure "Invalid location") (fun _ ->
      cell_at (set_cell map cell coord) coord)

let block_test
    (name : string)
    (map : t)
    (dir : HexUtil.dir)
    (c1 : HexUtil.coord)
    (c2 : HexUtil.coord)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (does_block map dir c1 c2)
    ~printer:string_of_bool

let dist_test
    (name : string)
    (map : t)
    (c1 : HexUtil.coord)
    (c2 : HexUtil.coord)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (dist map c1 c2) ~printer:string_of_int

let i_map = HexMap.init_map ()

let c00 : HexUtil.coord = { col = 0; diag = 0 }

let c01 : HexUtil.coord = { col = 1; diag = 0 }

let cell_at_tests =
  [
    cell_at_test "cell at 0 0" i_map c00
      (Some (Cell.init_cell 1 None c00));
    cell_at_test "cell at 5 0" i_map { col = 5; diag = 0 } None;
    cell_at_test "cell at 3 3" i_map { col = 3; diag = 3 }
      (Some (Cell.init_cell 4 None { col = 3; diag = 3 }));
  ]

let set_cell_tests =
  [
    set_cell_test "set 0 0 None" i_map None c00;
    set_cell_test "set 3 3 None" i_map None { col = 3; diag = 3 };
    set_cell_test "set 3 0 Some soil 4" i_map
      (Some (Cell.init_cell 4 None { col = 3; diag = 0 }))
      { col = 3; diag = 0 };
    set_cell_fail "set 5 0 failure" i_map None { col = 5; diag = 0 };
  ]

let does_block_tests =
  [
    block_test "horizontal adjacent block true" i_map 5 c00 c01 true;
    block_test "horizontal 1 separation block true" i_map 5 c00
      { col = 2; diag = 0 } true;
    block_test "horizontal 2 separation block true" i_map 5 c00
      { col = 3; diag = 0 } true;
    block_test "horizontal 3 separation block true" i_map 5 c00
      { col = 4; diag = 0 } true;
    block_test "horizontal 4 separation block true" i_map 5 c00
      { col = 5; diag = 0 } true;
  ]

let center : HexUtil.coord = { col = 3; diag = 3 }

let dist_tests =
  [
    dist_test "dist to self" i_map c00 c00 0;
    dist_test "1 dist dir 0" i_map center { col = 4; diag = 4 } 1;
    dist_test "1 dist dir 1" i_map center { col = 3; diag = 4 } 1;
    dist_test "1 dist dir 2" i_map center { col = 2; diag = 3 } 1;
    dist_test "1 dist dir 3" i_map center { col = 2; diag = 2 } 1;
    dist_test "1 dist dir 4" i_map center { col = 3; diag = 2 } 1;
    dist_test "1 dist dir 5" i_map center { col = 4; diag = 3 } 1;
    dist_test "2 dist dir 0" i_map center { col = 5; diag = 5 } 2;
    dist_test "2 dist dir 01" i_map center { col = 4; diag = 5 } 2;
    dist_test "2 dist dir 1" i_map center { col = 3; diag = 5 } 2;
    dist_test "2 dist dir 12" i_map center { col = 2; diag = 4 } 2;
    dist_test "2 dist dir 2" i_map center { col = 1; diag = 3 } 2;
    dist_test "2 dist dir 23" i_map center { col = 1; diag = 2 } 2;
    dist_test "2 dist dir 3" i_map center { col = 1; diag = 1 } 2;
    dist_test "2 dist dir 34" i_map center { col = 2; diag = 1 } 2;
    dist_test "2 dist dir 4" i_map center { col = 3; diag = 1 } 2;
    dist_test "2 dist dir 45" i_map center { col = 4; diag = 2 } 2;
    dist_test "2 dist dir 5" i_map center { col = 5; diag = 3 } 2;
    dist_test "2 dist dir 50" i_map center { col = 5; diag = 4 } 2;
  ]

let suite =
  "test suite for HexMap"
  >::: List.flatten
         [ cell_at_tests; set_cell_tests; does_block_tests; dist_tests ]

let test = run_test_tt_main suite
