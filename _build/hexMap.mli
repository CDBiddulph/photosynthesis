type t

type dir = int

type coord = {
  row : int;
  diag : int;
}

val is_behind : t -> dir -> coord -> coord -> bool

val place_tree : t -> coord -> t

val dist : t -> coord -> coord -> int

val neighbor : t -> coord -> dir -> coord option
