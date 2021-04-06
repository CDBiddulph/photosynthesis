(** The abstract type representing the game's board. *)
type t

(** The type [ruleset] represents the ruleset used in the game. *)
type ruleset =
  | Normal
  | Extended

(** Raised when an invalid plant placement is attempted. *)
exception InvalidPlantPlacement

(** Raised when an invalid harvest is attempted. *)
exception InvalidHarvest

(** [init_board ruleset] initializes a game with the given ruleset. *)
val init_board : ruleset -> t

(** [is_place_plant_legal board coord plant] returns [true] iff placing
    [plant] at [coord] on [board] is a legal move. *)
val is_place_plant_legal : t -> HexUtil.coord -> Plant.t -> bool

(** [place_plant board coord plant] places [plant] at [coord] on
    [board]. Raises: [InvalidPlantPlacement] if placing [plant] in
    [coord] on [board] is not a legal move. *)
val place_plant : t -> HexUtil.coord -> Plant.t -> t

(** [harvest board player coord] removes the plant at [coord] on
    [board]. Raises: [InvalidPlantPlacement] if placing [plant] in
    [coord] on [board] with [player] is not a legal move. *)
val harvest : t -> PlayerId.t -> HexUtil.coord -> t

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

(** [get_photo_lp board players] is an association list of player ids to
    the pairs of [HexUtil.coord]s that have plants that the the player
    owns and the light points gained by the plant in that cell. *)
val get_photo_lp :
  t -> PlayerId.t list -> (PlayerId.t * (HexUtil.coord * int) list) list

(** [cells board] is a list of the Cells in [board], in any order. *)
val cells : t -> Cell.t list
