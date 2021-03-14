type map

type dir

type coord

val coord_from_ints : int -> int -> coord

val row : coord -> int

val diag : coord -> int

val is_behind : map -> dir -> coord -> bool

val place_tree : map -> coord -> map

val dist : map -> coord -> coord -> int

val neighbor : map -> dir -> coord -> Cell.t option
