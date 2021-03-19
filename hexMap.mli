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

(** [does_block map d c1 c2] is [true] iff it is possible to move on
    [map] one hex at a time, only in direction [d] and starting from
    [c1], and eventually land on [c2]. If [c1 = c2], returns false.
    Requires: [c1] and [c2] are valid coordinates in [map]. *)
val does_block : t -> dir -> coord -> coord -> bool

(** [set_cell map cell coord] is [map] where the cell at [coord] is set
    to [cell]. Requires: [coord] is a valid coordinate in [map]. *)
val set_cell : t -> Cell.t -> coord -> t

(** [cell_at map coord] is the cell at [coord] of [map]. Requires: [c]
    is a valid coordinate in [map]. *)
val cell_at : t -> coord -> Cell.t

(** [dist map c1 c2] is the hex distance between [c1] and [c2], i.e. the
    minimum number of steps to reach [c1] from [c2]. If [c1 = c2],
    returns 0. Requires: [c1] and [c2] are valid coordinates in [map]. *)
val dist : t -> coord -> coord -> int

(** [neighbor map c d] is the coordinate of the closest neighboring cell
    to [c] in direction [d], if it exists. If there are no neighbors in
    direction [d] from [c], then the result is [None]. Requires: [c] is
    a valid coordinate in [map]. *)
val neighbor : t -> coord -> dir -> coord option
