(** The abstract type representing the game's board. *)
type t

type ruleset =
  | Normal
  | Extended

(** Raised when an invalid plant placement is attempted. *)
exception InvalidPlacement

(** [init_game players map sun] initializes a game with the given
    players, board, and sun direction. The first player in the list is
    assumed to be the first, and the order of play follows the order of
    the players in the list. Requires: [players] is non-empty *)
val init_game : Player.t list -> HexMap.t -> HexUtil.dir -> t

(** [is_place_plant_legal board cell plant] returns [true] iff placing
    [plant] in [cell] on [board] is a legal move. *)
val is_place_plant_legal : t -> Cell.t -> Plant.t -> bool

(* this function will probably only be used by GUI *)

(** [place_plant board cell plant] places [plant] in [cell] on [board].
    Raises: [InvalidPlacement] if placing [plant] in [cell] on [board]
    is not a legal move. *)
val place_plant : t -> Cell.t -> Plant.t -> t

val end_turn : t -> t

val sun_dir : t -> HexUtil.dir
