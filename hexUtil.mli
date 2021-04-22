(** Type representing the position of a flat-topped hexagon in axial
    coordinates. [col] increases from left to right when viewed via the
    GUI. That is, assuming [HexMap.neighbor map c1 dir = Some c2],
    [c1.col = c2.col] when [dir = 1] or [dir = 4], [c1.col - 1 = c2.col]
    when [dir = 2] or [dir = 3], and [c1.col + 1 = c2.col] when
    [dir = 0] or [dir = 5]. Likewise, [diag] increases from the
    bottom-left of the map to the top-right. *)
type coord = {
  col : int;
  diag : int;
}

(** The type representing directions on the hexagonal board. [dir]
    represents [60 * dir + 30] degrees clockwise from the vector
    pointing right. Valid [dir] values are the integers between 0-5,
    inclusive. *)
type dir = int

(** [move_cw dir] is [dir] after it moves clockwise once. *)
val move_cw : dir -> dir

val dir_of_int : int -> dir
