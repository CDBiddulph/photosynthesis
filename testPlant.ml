open OUnit2
open PlayerId
open Plant

let player_id_test
    (name : string)
    (p : Plant.t)
    (expected_output : PlayerId.t ) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (player_id p)

let plant_stage_test
    (name : string)
    (p : Plant.t)
    (expected_output : Plant.plant_stage) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (plant_stage p)

let int_of_plant_stage_test
    (name : string)
    (p : Plant.plant_stage)
    (expected_output : int) : test = 
  name >:: fun _ ->
  assert_equal expected_output (int_of_plant_stage p)
~printer: string_of_int

let next_stage_test 
    (name : string)
    (p : Plant.plant_stage)
    (expected_output : plant_stage option) : test =
  name >:: fun _ ->
  assert_equal expected_output (next_stage p)

let p0 = init_plant 1 Plant.Seed

let p1 = init_plant 2 Plant.Small

let p2 = init_plant 3 Plant.Medium

let p3 = init_plant 4 Plant.Large


let plant_tests = [
    player_id_test "Plant with Player Id of 1" p0 1;
    player_id_test "Plant with Player Id of 2" p1 2;
    plant_stage_test "Plant with Seed" p0 Plant.Seed;
    plant_stage_test "Plant with Small" p1 Plant.Small;
    int_of_plant_stage_test "int_of_plant with Seed" Plant.Seed 0;
    int_of_plant_stage_test "int_of_plant with Small" Plant.Small 1;
    int_of_plant_stage_test "int_of_plant with Medium" Plant.Medium 2;
    int_of_plant_stage_test "int_of_plant with Medium" Plant.Large 3;
    next_stage_test "next_stage with Seed" Plant.Seed (Some Plant.Small);
    next_stage_test "next_stage with Small" Plant.Small (Some Plant.Medium);
    next_stage_test "next_stage with Medium" Plant.Medium (Some Plant.Large);
    next_stage_test "next_stage with Large" Plant.Large None;
  ]

let suite =
  "test suite for HexMap" >::: List.flatten [ plant_tests ]

let test = run_test_tt_main suite
