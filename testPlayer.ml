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

let avail_player avail =
  Player.init_player 1
  |> _set_available
       (PlantInventory._init_plant_inventory
          (List.map2 (fun s n -> (s, n)) Plant.all_stages avail))
  |> add_lp 20

let avail_test name f starting_avail expected_avail =
  name >:: fun _ ->
  let player = avail_player starting_avail in
  assert_equal expected_avail
    (player |> f |> _available |> PlantInventory._contents
   |> List.map snd)
    ~printer:(TestUtil.pp_list string_of_int)

let player = Player.init_player 1 |> add_lp 25

let available_tests =
  [ avail_test "harvest" (harvest 10) [ 1; 1; 1; 1 ] [ 1; 1; 1; 1 ] ]

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
    lp_test "buy" (buy_plant Large) 10 6;
    lp_test "buy 2"
      (fun g -> g |> buy_plant Large |> buy_plant Large)
      10 1;
    lp_test_exn "buy ILP" (buy_plant Seed) 0
      (Store.InsufficientLightPoints 1);
    lp_test "buy almost ILP" (buy_plant Seed) 1 0;
  ]

let sp_tests =
  [
    ( "harvest 10" >:: fun _ ->
      let player = lp_player 20 in
      assert_equal 10
        (player |> harvest 10 |> score_points)
        ~printer:string_of_int );
  ]

let suite =
  "test suite for HexMap"
  >::: List.flatten [ lp_tests; sp_tests; available_tests ]

let test = run_test_tt_main suite
