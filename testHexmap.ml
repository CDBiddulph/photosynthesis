open OUnit2
open HexMap

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

let i_map = HexMap.init_map ()

let c00 : HexUtil.coord = { col = 0; diag = 0 }

let c01 : HexUtil.coord = { col = 1; diag = 0 }

let does_block_tests =
  [ block_test "horizontal block true" i_map 0 c00 c01 true ]

let suite =
  "test suite for HexMap" >::: List.flatten [ does_block_tests ]

let test = run_test_tt_main suite
