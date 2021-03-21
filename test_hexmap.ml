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
  assert_equal expected_output (does_block map dir c1 c2)

let i_map = HexMap.init_map

let does_block_tests = [ block_test "horizontal block true" ]

let suite =
  "test suite for HexMap" >::: List.flatten [ does_block_tests ]

let _ = run_test_tt_main suite
