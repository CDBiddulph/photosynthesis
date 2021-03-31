(* TODO: discuss the best place to define coord and dir. Defining them
   in HexMap was giving me a circular import error, but this might not
   be an insurmountable obstacle, I could just be doing it wrong. *)

(* TODO: discuss how to define diag *)

(** Type representing the position of a flat-topped hexagon in axial
    coordinates. [col] increases from left to right when viewed via the
    GUI. That is, assuming [HexMap.neighbor map c1 dir = Some c2],
    [c1.col = c2.col] when [dir = 1] or [dir = 4], [c1.col - 1 = c2.col]
    when [dir = 2] or [dir = 3], and [c1.col + 1 = c2.col] when
    [dir = 0] or [dir = 5]. Likewise, [diag] increases from TODO *)
type coord = {
  col : int;
  diag : int;
}

(** The type representing directions on the hexagonal board. [dir]
    represents [60 * dir + 30] degrees counterclockwise from the
    horizontal. Valid [dir] values are the integers between 0-5,
    inclusive. *)
type dir = int
