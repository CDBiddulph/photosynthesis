(** The abstract type representing the game's board. *)
type t

(** The type [ruleset] represents the ruleset used in the game. *)
type ruleset =
  | Normal
  | Extended

(** Raised when an illegal seed-planting is attempted. *)
exception IllegalPlantSeed

(** Raised when an illegal plant-growing is attempted. *)
exception IllegalGrowPlant

(** Raised when an invalid harvest is attempted. *)
exception IllegalHarvest

(** [init_board ruleset] initializes a game with the given ruleset. *)
val init_board : ruleset -> t

(** [plant_seed board coord player_id] places a seed belonging to
    [player] at [coord] on [board]. Raises: [IllegalPlantSeed] if
    placing a seed as [player] in [coord] on [board] is not a legal
    move. *)
val plant_seed : t -> PlayerId.t -> HexUtil.coord -> t

(** [grow_plant board coord player_id] grows the plant belonging to
    [player] at [coord] on [board]. Raises: [IllegalGrowPlant] if
    placing a seed as [player] in [coord] on [board] is not a legal
    move. *)
val grow_plant : t -> HexUtil.coord -> PlayerId.t -> t

(** [harvest board player coord] removes the plant at [coord] on
    [board]. Raises: [IllegalHarvest] if removing the plant in [coord]
    on [board] with [player] is not a legal move. *)
val harvest : t -> PlayerId.t -> HexUtil.coord -> t

(** [can_plant_seed board coord player_id stage] returns [true] iff
    player of [player_id] planting a seed at [coord] on [board] is
    possible. *)
val can_plant_seed : t -> PlayerId.t -> HexUtil.coord -> bool

(** [can_grow_plant board coord player_id stage] returns [true] iff
    player of [player_id] growing a plant of [stage] at [coord] on
    [board] is possible. *)
val can_grow_plant : t -> PlayerId.t -> HexUtil.coord -> bool

(** [can_harvest board player c] returns true if the plant at [c] can be
    harvested by [player]. If the cell at [c] does not contain a Large
    plant, or [c] is invalid, or the plant is not owned by [player],
    returns false. *)
val can_harvest : t -> PlayerId.t -> HexUtil.coord -> bool

(** [get_photo_lp board sun_dir players] is an association list of
    player ids to the pairs of [HexUtil.coord]s that have plants that
    the the player owns and the light points gained by the plant in that
    cell with the given sun direction. *)
val get_photo_lp :
  t ->
  HexUtil.dir ->
  PlayerId.t list ->
  (PlayerId.t * (HexUtil.coord * int) list) list

(** [cells board] is a list of the Cells in [board], in any order. *)
val cells : t -> Cell.t list

(** [cell_at board coord] is the cell at [coord] in [board]. *)
val cell_at : t -> HexUtil.coord -> Cell.t option

(** [valid_coord board coord] is true if [coord] is a valid coordinate
    in [board]. *)
val valid_coord : t -> HexUtil.coord -> bool

(** [plant_at board coord] is the plant at [coord] on [board], if it
    exists. *)
val plant_at : t -> HexUtil.coord -> Plant.t option
