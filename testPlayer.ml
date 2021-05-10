open OUnit2
open Player

let lp_avail_store_player lp avail_lst store_lst =
  Player.init_player 1
  |> _set_available
       (PlantInventory._init_plant_inventory
          (List.map2 (fun s n -> (s, n)) Plant.all_stages avail_lst))
  |> _set_store (Store._init_store store_lst)
  |> add_lp lp

let test_exn name f init_lp init_avail init_store expected_exn =
  "test_exn " ^ name >:: fun _ ->
  let player = lp_avail_store_player init_lp init_avail init_store in
  assert_raises expected_exn (fun () -> f player)

let avail_store_player = lp_avail_store_player 20

let store_test name f starting_store expected_store =
  "store_test " ^ name >:: fun _ ->
  let player = avail_store_player [ 1; 1; 1; 1 ] starting_store in
  assert_equal expected_store
    ( player |> f |> _store |> fun store ->
      List.map ((Fun.flip Store.num_remaining) store) Plant.all_stages
    )
    ~printer:(TestUtil.pp_list string_of_int)

let store_test_exn name f starting_store expected_exn =
  "store_test_exn " ^ name >:: fun _ ->
  let player = avail_store_player [ 1; 1; 1; 1 ] starting_store in
  assert_raises expected_exn (fun () -> f player)

let store_tests =
  [
    store_test "harvest" (harvest 10) [ 1; 1; 1; 1 ] [ 1; 1; 1; 2 ];
    store_test "harvest full" (harvest 10) [ 1; 1; 1; 2 ] [ 1; 1; 1; 2 ];
    store_test "buy" (buy_plant Small) [ 1; 1; 1; 1 ] [ 1; 0; 1; 1 ];
    store_test_exn "buy empty" (buy_plant Medium) [ 1; 1; 0; 1 ]
      (PlantInventory.OutOfPlant Medium);
    store_test "grow" (grow_plant Large) [ 1; 1; 2; 1 ] [ 1; 1; 3; 1 ];
    store_test "grow full" (grow_plant Medium) [ 1; 4; 1; 1 ]
      [ 1; 4; 1; 1 ];
    store_test "plant seed" (plant_plant Seed) [ 1; 1; 1; 1 ]
      [ 1; 1; 1; 1 ];
    store_test "plant small" (plant_plant Small) [ 1; 1; 1; 1 ]
      [ 1; 1; 1; 1 ];
  ]

let avail_test name f starting_avail expected_avail =
  "avail_test " ^ name >:: fun _ ->
  let player = avail_store_player starting_avail [ 1; 1; 1; 1 ] in
  assert_equal expected_avail
    (player |> f |> _available |> PlantInventory._contents
   |> List.map snd)
    ~printer:(TestUtil.pp_list string_of_int)

let avail_test_exn name f starting_avail expected_exn =
  "avail_test_exn " ^ name >:: fun _ ->
  let player = avail_store_player starting_avail [ 1; 1; 1; 1 ] in
  assert_raises expected_exn (fun () -> f player)

let available_tests =
  [
    avail_test "harvest" (harvest 10) [ 1; 1; 1; 1 ] [ 1; 1; 1; 1 ];
    avail_test "plant seed" (plant_plant Seed) [ 1; 2; 3; 4 ]
      [ 0; 2; 3; 4 ];
    avail_test "plant small" (plant_plant Small) [ 1; 2; 3; 4 ]
      [ 1; 1; 3; 4 ];
    avail_test "grow medium" (grow_plant Medium) [ 1; 2; 3; 4 ]
      [ 1; 2; 2; 4 ];
    avail_test "grow large" (grow_plant Large) [ 1; 2; 3; 4 ]
      [ 1; 2; 3; 3 ];
    avail_test "buy seed" (buy_plant Seed) [ 1; 2; 3; 4 ] [ 2; 2; 3; 4 ];
  ]

let lp_test name f init_lp init_avail init_store expected_lp =
  "lp_test " ^ name >:: fun _ ->
  let player = lp_avail_store_player init_lp init_avail init_store in
  assert_equal expected_lp
    (player |> f |> light_points)
    ~printer:string_of_int

let lp_test_exn name f init_lp expected_exn =
  test_exn name f init_lp [ 1; 1; 1; 1 ] [ 1; 1; 1; 1 ] expected_exn

let lp_test_default name f init_lp expected_lp =
  lp_test name f init_lp [ 1; 1; 1; 1 ] [ 1; 1; 1; 1 ] expected_lp

let lp_tests =
  [
    lp_test_default "overflow" Fun.id 25 20;
    lp_test_default "harvest" (harvest 10) 10 6;
    lp_test_exn "harvest ILP" (harvest 10) 3
      (Store.InsufficientLightPoints 4);
    lp_test_default "harvest almost ILP" (harvest 10) 4 0;
    lp_test_default "plant 1" (plant_plant Seed) 10 9;
    lp_test_default "plant 2" (plant_plant Small) 0 0;
    lp_test_default "grow" (grow_plant Large) 10 7;
    lp_test_exn "grow ILP" (grow_plant Medium) 1
      (Store.InsufficientLightPoints 2);
    lp_test_default "grow almost ILP" (grow_plant Small) 1 0;
    lp_test_default "buy" (buy_plant Large) 10 5;
    lp_test "buy 2"
      (fun g -> g |> buy_plant Large |> buy_plant Large)
      10 [ 1; 1; 1; 1 ] [ 1; 1; 1; 2 ] 1;
    lp_test_exn "buy ILP" (buy_plant Seed) 1
      (Store.InsufficientLightPoints 2);
    lp_test_default "buy almost ILP" (buy_plant Seed) 2 0;
  ]

let sp_tests =
  [
    ( "sp_test harvest 10" >:: fun _ ->
      let player =
        lp_avail_store_player 20 [ 1; 1; 1; 1 ] [ 1; 1; 1; 1 ]
      in
      assert_equal 10
        (player |> harvest 10 |> score_points)
        ~printer:string_of_int );
  ]

let suite =
  "test suite for HexMap"
  >::: List.flatten [ lp_tests; sp_tests; available_tests; store_tests ]

let test = run_test_tt_main suite
