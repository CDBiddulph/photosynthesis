(** TODO *)

(** The abstract type representing the game's board. *)
type t

(** The type [ruleset] represents the ruleset used in the game. *)
type ruleset =
  | Normal
  | Extended

(** Raised when an illegal plant placement is attempted. *)
exception IllegalPlacePlant

(** Raised when an illegal plant-growing is attempted. *)
exception IllegalGrowPlant

(** Raised when an invalid harvest is attempted. *)
exception IllegalHarvest

(** [init_board ruleset] initializes a game with the given ruleset. *)
val init_board : ruleset -> t

(** [testing_init_board ruleset cells] initializes a board with the
    given ruleset and cells. USED ONLY FOR TESTING PURPOSES. *)
val testing_init_board : ruleset -> Cell.t list -> t

(** [plant_seed stage board coord player_id] places a seed belonging to
    [player] at [coord] on [board]. Raises: [IllegalPlacePlant] if
    placing a seed as [player] in [coord] on [board] is not a legal
    move, under the assumption that the game is not in the setup phase. *)
val plant_seed : PlayerId.t -> HexUtil.coord -> t -> t

(** [plant_small stage board coord player_id] places a small tree
    belonging to [player] at [coord] on [board]. Raises:
    [IllegalPlacePlant] if placing a seed as [player] in [coord] on
    [board] is not a legal move, under the assumption that the game is
    in the setup phase. *)
val plant_small : PlayerId.t -> HexUtil.coord -> t -> t

(** [grow_plant board coord player_id] grows the plant belonging to
    [player] at [coord] on [board]. Raises: [IllegalGrowPlant] if
    placing a seed as [player] in [coord] on [board] is not a legal
    move. *)
val grow_plant : HexUtil.coord -> PlayerId.t -> t -> t

(** [harvest board player coord] removes the plant at [coord] on
    [board]. Raises: [IllegalHarvest] if removing the plant in [coord]
    on [board] with [player] is not a legal move. *)
val harvest : PlayerId.t -> HexUtil.coord -> t -> t

(** [can_plant_seed coord player_id board] returns [true] iff player of
    [player_id] planting a seed at [coord] on [board] is possible,
    assuming the game is not in the setup stage. *)
val can_plant_seed : PlayerId.t -> HexUtil.coord -> t -> bool

(** [can_plant_small board coord player_id stage] returns [true] iff
    player of [player_id] planting a seed at [coord] on [board] is
    possible, assuming the game is in the setup stage. *)
val can_plant_small : HexUtil.coord -> t -> bool

(** [can_grow_plant board coord player_id stage] returns [true] iff
    player of [player_id] growing a plant of [stage] at [coord] on
    [board] is possible. *)
val can_grow_plant : PlayerId.t -> HexUtil.coord -> t -> bool

(** [can_harvest board player c] returns true if the plant at [c] can be
    harvested by [player]. If the cell at [c] does not contain a Large
    plant, or [c] is invalid, or the plant is not owned by [player],
    returns false. *)
val can_harvest : PlayerId.t -> HexUtil.coord -> t -> bool

(** [get_photo_lp sun_dir players board] is an association list of
    player ids to the pairs of [HexUtil.coord]s that have plants that
    the player owns and the light points gained by the plant in that
    cell when the sun is facing in direction [sun_dir]. *)
val get_photo_lp :
  HexUtil.dir ->
  PlayerId.t list ->
  t ->
  (PlayerId.t * (HexUtil.coord * int) list) list

(** [cells board] is a list of the Cells in [board], in any order. *)
val cells : t -> Cell.t list

(** [cell_at board coord] is the cell at [coord] in [board]. *)
val cell_at : HexUtil.coord -> t -> Cell.t option

(** [valid_coord board coord] is true if [coord] is a valid coordinate
    in [board]. *)
val valid_coord : HexUtil.coord -> t -> bool

(** [plant_at board coord] is the plant at [coord] on [board], if it
    exists. *)
val plant_at : HexUtil.coord -> t -> Plant.t option

(** [end_turn board] resets the cells touched in the current turn,
    allowing the next player to execute their actions. *)
val end_turn : t -> t

(** [actionable_cells board] is a list of cells that can have operations
    performed on them. *)
val actionable_cells : t -> HexUtil.coord list
