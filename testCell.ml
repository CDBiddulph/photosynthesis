open OUnit2
open Cell
open Plant

let coord_test
    (name : string)
    (c : Cell.t)
    (expected_output : HexUtil.coord) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (coord c)

let soil_test
    (name : string)
    (c : Cell.t)
    (expected_output : Cell.soil) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (soil c)
~printer: string_of_int

let plant_test
    (name : string)
    (c : Cell.t)
    (expected_output : Plant.t option) : test = 
  name >:: fun _ ->
  assert_equal expected_output (plant c)

let p1 = Plant.init_plant 1 Plant.Small

let c00 : HexUtil.coord = { col = 0; diag = 0 }

let c0 = Cell.init_cell 2 None ({ col = 2; diag = 6})

let c1 = Cell.init_cell 1 None c00

let c2 = Cell.set_plant c1 (Some p1)

let cell_tests = [
    coord_test "Cell with coord (0,0)" c1 { col = 0; diag = 0};
    coord_test "Cell with different coord" c0 { col = 2; diag = 6};
    soil_test "Cell with 1 for soil" c1 1;
    soil_test "Cell with 2 for soil" c0 2;
    plant_test "Cell with None plant" c1 None;
    plant_test "Cell with Some plant" c2 (Some p1);
  ]

let suite =
  "test suite for HexMap" >::: List.flatten [ cell_tests ]

let test = run_test_tt_main suite
