(** The abstract type representing the game's board. *)
type t

(** The type [ruleset] represents the ruleset used in the game. *)
type ruleset =
  | Normal
  | Extended

(** Raised when an invalid plant placement is attempted. *)
exception InvalidPlacement

(** [init_game players map sun] initializes a game with the given
    players, board, and sun direction. The first player in the list is
    assumed to be the first, and the order of play follows the order of
    the players in the list. Requires: [players] is non-empty *)
val init_game : Player.t list -> HexMap.t -> HexUtil.dir -> ruleset -> t

(** [is_place_plant_legal board cell plant] returns [true] iff placing
    [plant] in [cell] on [board] is a legal move. *)
val is_place_plant_legal : t -> Cell.t -> Plant.t -> bool

(** [place_plant board cell plant] places [plant] in [cell] on [board].
    Raises: [InvalidPlacement] if placing [plant] in [cell] on [board]
    is not a legal move. *)
val place_plant : t -> Cell.t -> Plant.t -> t

(** [flat_board board] is the list of all valid [Cell]s in [board]. *)
val flat_board : t -> Cell.t list

(** [end_turn board] TODO *)
val end_turn : t -> t

(** [sun_dir board] is the current sun direction for [board]. *)
val sun_dir : t -> HexUtil.dir

(** [can_remove board c] determines if the plant at [c] can be removed.
    If there is no plant at [c] or [c] is invalid, returns false. *)
val can_remove : t -> HexUtil.coord -> bool

(** [remove_plant board c] removes the plant at [c]. If there is no
    plant or [c] is invalid, no change occurs. *)
val remove_plant : t -> HexUtil.coord -> t
