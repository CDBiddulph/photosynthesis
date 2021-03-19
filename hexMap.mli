(** The abstract type representing a hexagonal board. *)
type t

(** The type representing directions on the hexagonal board. Valid [dir]
    values are the integers between 0-5, inclusive. *)
type dir = int

(** The type representing the coordinates of cells in the board. *)
type coord = {
  row : int;
  diag : int;
}

(** [is_behind map d c1 c2] determines if [c1] is behind [c2] in the
    direction of [d] on [map]. Requires: [c1] and [c2] are valid
    coordinates in [map]. *)
val is_behind : t -> dir -> coord -> coord -> bool

(** [place_tree map c] places a tree at [c] on [map]. Requires: [c] is a
    valid coordinate in [map], and placing a tree in the cell at [c] is
    a valid placement. *)
val place_tree : t -> coord -> t

(** [dist map c1 c2] is the distance between [c1] and [c2] in terms of
    steps (i.e. the number of tiles between the two coordinates).
    Requires: [c1] and [c2] are valid coordinates in [map]. *)
val dist : t -> coord -> coord -> int

(** [neighbor map c d] is the coordinate of the neighboring cell to [c]
    in direction [d], if it exists. If there are no neighbors in
    direction [d] from [c], then the result is [None]. Requires: [c] is
    a valid coordinate in [map]. *)
val neighbor : t -> coord -> dir -> coord option
