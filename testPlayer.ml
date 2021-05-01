open OUnit2
open Player

let lp_player starting_lp =
  Player.init_player 1
  |> _set_available
       (PlantInventory._init_plant_inventory
          (List.map (fun s -> (s, 1)) Plant.all_stages))
  |> add_lp starting_lp

let lp_test
    (name : string)
    (f : t -> t)
    (starting_lp : int)
    (expected_lp : int) : test =
  name >:: fun _ ->
  let player = lp_player starting_lp in
  assert_equal expected_lp
    (player |> f |> light_points)
    ~printer:string_of_int

let lp_test_exn
    (name : string)
    (f : t -> t)
    (starting_lp : int)
    expected_exn =
  name >:: fun _ ->
  let player = lp_player starting_lp in
  assert_raises expected_exn (fun () -> f player)

let player = Player.init_player 1 |> add_lp 25

let lp_tests =
  [
    lp_test "overflow" Fun.id 25 20;
    lp_test "harvest" (harvest 10) 10 6;
    lp_test_exn "harvest ILP" (harvest 10) 3
      (Store.InsufficientLightPoints 4);
    lp_test "harvest almost ILP" (harvest 10) 4 0;
    lp_test "plant 1" (plant_plant Seed) 10 10;
    lp_test "plant 2" (plant_plant Small) 0 0;
    lp_test "grow" (grow_plant Large) 10 7;
    lp_test_exn "grow ILP" (grow_plant Medium) 1
      (Store.InsufficientLightPoints 2);
    lp_test "grow almost ILP" (grow_plant Small) 1 0;
  ]

let suite = "test suite for HexMap" >::: List.flatten [ lp_tests ]

let test = run_test_tt_main suite
