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

(** [is_place_plant_legal board coord player stage] returns [true] iff
    [player] placing a plant of stage [stage] at [coord] on [board] is a
    legal move. *)
val is_place_plant_legal :
  t -> HexUtil.coord -> PlayerId.t -> Plant.plant_stage -> bool

(** [place_plant board coord plant] places [plant] at [coord] on
    [board]. Raises: [InvalidPlantPlacement] if placing [plant] in
    [coord] on [board] is not a legal move. *)
val place_plant : t -> HexUtil.coord -> Plant.t -> t

(** [can_remove board player c] determines if the plant at [c] can be
    removed by [player]. If there is no plant at [c], or [c] is invalid,
    or the plant is not owned by [player], returns false. *)
val can_remove : t -> PlayerId.t -> HexUtil.coord -> bool

(** [harvest board player coord] removes the plant at [coord] on
    [board]. Raises: [InvalidHarvest] if removing the plant in [coord]
    on [board] with [player] is not a legal move. *)
val harvest : t -> PlayerId.t -> HexUtil.coord -> t

(** [move_sun board] moves the board's sun by one position. *)
val move_sun : t -> t

(** [sun_dir board] is the current sun direction for [board]. *)
val sun_dir : t -> HexUtil.dir

(** [get_photo_lp board players] is an association list of player ids to
    the pairs of [HexUtil.coord]s that have plants that the the player
    owns and the light points gained by the plant in that cell. *)
val get_photo_lp :
  t -> PlayerId.t list -> (PlayerId.t * (HexUtil.coord * int) list) list

(** [cells board] is a list of the Cells in [board], in any order. *)
val cells : t -> Cell.t list

(** [cell_at board coord] is the cell at [coord] in [board]. *)
val cell_at : t -> HexUtil.coord -> Cell.t
