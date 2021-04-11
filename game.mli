(** Handles actions that can be taken by a player on the state of the
    player objects and the board *)

(** Type of the game state *)
type t

(* Setter functions*)

(** [init_game num_players] is a new game with [num_players] players and
    all initial defaults. *)
val init_game : int -> Board.ruleset -> t

(** [plant_seed game player_id coord] is game with a seed of [player]
    planted at [coord] and the available seeds of the player decremented
    by 1. Raises: [Board.IllegalPlantSeed] if planting a seed at [coord]
    is an illegal move; [PlantInventory.OutOfPlant Plant.Seed] if the
    player does not have any seeds in their available area. See
    [can_plant_seed] and [is_plant_available]. *)
val plant_seed : t -> PlayerId.t -> HexUtil.coord -> t

(** [grow_plant game player_id coord] is [game] with the plant at
    [coord] in the board grown to the next stage, the available plants
    of the next stage of the plant at [coord] of the player
    corresponding to [player_id] decremented by 1, and the store of the
    plant stage that [plant] is replacing incremented by 1 if the store
    of [last_plant] is full (otherwise, it will stay the same, meaning
    the replaced plant was thrown away). Raises:
    [Board.IllegalGrowPlant] if growing the plant at [coord] is an
    illegal or impossible move; [PlantInventory.OutOfPlant next_stage]
    if the player does not have any of [next_stage] in their available
    area. See [can_grow_plant] and [is_plant_available]. *)
val grow_plant : t -> PlayerId.t -> HexUtil.coord -> t

(** [harvest game coord] is [game] with the plant at [coord] removed and
    the scoring points of the current player of [game] increased
    appropriately. Raises: [Board.IllegalHarvest] if harvesting from
    [coord] with the current player of [game] is illegal. *)
val harvest : t -> PlayerId.t -> HexUtil.coord -> t

(** [buy_plant game stage] is [game] where the current player of [game]
    has one less plant of [stage] in their store and one more plant of
    [stage] in their available area. Raises:
    [Store.InsufficientLightPoints] if Player does not have enough light
    points to buy a plant of [stage];
    [PlantInventory.OutOfPlant (Plant.plant_stage plant)] if the player
    does not have any of [Plant.plant_stage plant] in their store. *)
val buy_plant : t -> Plant.plant_stage -> t

(* In the future, we may want to alert the UI that photosynthesis is
   about to happen instead of just lumping photosynthesis in with
   end_turn, so that we can have a screen on which numbers appear under
   the trees that collect light points. *)

(** [end_turn game] is [game] with control relinquished to the player
    who plays after the current player, and with photosynthesis
    performed if appropriate. *)
val end_turn : t -> t

(* Getter functions. *)

(** [can_plant_seed game coord player_id stage] is true iff planting a
    seed with the player of [player_id] at [coord] is a legal move,
    disregarding whether the player actually has a seed in their
    available area. *)
val can_plant_seed : t -> HexUtil.coord -> PlayerId.t -> bool

(** [can_grow_plant game coord player_id] is true iff growing the plant
    at [coord] with the player of [player_id] at [coord] is a legal
    move, disregarding whether the player actually has the plant in
    their available area. *)
val can_grow_plant : t -> HexUtil.coord -> PlayerId.t -> bool

(** [turn game] is the player_id of the player whose turn it is in
    [game]. *)
val turn : t -> PlayerId.t

(** [player_of game player_id] is the player of [player_id] in [game]. *)
val player_of : t -> PlayerId.t -> Player.t

(** [player_of_turn game] is the player whose turn it is in [game]. *)
val player_of_turn : t -> Player.t

(** [cell_at game coord] is the cell at [coord] in [game]. *)
val cell_at : t -> HexUtil.coord -> Cell.t

(** [next_scoring_points game soil] is the number of scoring points that
    will be collected by the next player to harvest a tree of type
    [soil]. *)
val next_scoring_points : t -> Cell.soil -> int

(** [cells game] is a list of the Cells in the board of game, in any
    order. *)
val cells : t -> Cell.t list
