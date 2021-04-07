(** Handles actions that can be taken by a player on the state of the
    player objects and the board *)

(** Type of the game state *)
type t

(* Setter functions*)

(** [init_game num_players] is a new game with [num_players] players and
    all initial defaults. *)
val init_game : int -> Board.ruleset -> t

(** [place_plant game plant coord] is [game] with [plant] placed at
    [coord] in the board, the available plants of type
    [Plant.plant_stage plant] of the player corresponding to
    [Plant.player_id plant] decremented by 1, and the store of the plant
    stage that [plant] is replacing, [last_plant], incremented by 1, if
    [last_plant] exists, and if the store of [last_plant] is full
    (otherwise, it will stay the same, meaning the replaced plant was
    thrown away). Raises: [Board.InvalidPlantPlacement] if placing
    [plant] at [coord] is an illegal move;
    [PlantInventory.OutOfPlant (Plant.plant_stage plant)] if the player
    does not have any of [Plant.plant_stage plant] in their available
    area. See [is_place_plant_legal] and [is_plant_available]. *)
val place_plant : t -> Plant.t -> HexUtil.coord -> t

(** [harvest game player_id coord] is [game] with the plant at [coord]
    removed and the scoring points player of [player_id] increased
    appropriately. Raises: [Board.InvalidHarvest] if harvesting from
    [coord] with player of [player_id] is illegal. *)
val harvest : t -> PlayerId.t -> HexUtil.coord -> t

(** [buy_plant game player_id stage] is [game] where player of
    [player_id] has one less plant of [stage] in their store and one
    more plant of [stage] in their available area. Raises:
    [Store.InsufficientLightPoints] if Player does not have enough light
    points to buy a plant of [stage];
    [PlantInventory.OutOfPlant (Plant.plant_stage plant)] if the player
    does not have any of [Plant.plant_stage plant] in their store. *)
val buy_plant : t -> PlayerId.t -> Plant.plant_stage -> t

(* In the future, we may want to alert the UI that photosynthesis is
   about to happen instead of just lumping photosynthesis in with
   end_turn, so that we can have a screen on which numbers appear under
   the trees that collect light points. *)

(** [end_turn game] is [game] with control relinquished to the player
    who plays after the current player, and with photosynthesis
    performed if appropriate. *)
val end_turn : t -> t

(* Getter functions *)

(** [is_place_plant_legal game coord player_id stage] is true iff
    placing a plant of [stage] with the player of [player_id] at [coord]
    is a legal move, disregarding whether the player of [player_id]
    actually has the plant in their available area. *)
val is_place_plant_legal :
  t -> HexUtil.coord -> PlayerId.t -> Plant.plant_stage -> bool

(** [is_plant_in_available game player_id stage] is true iff player of
    [player_id] has more than zero plants of [stage] in their available
    area. *)
val is_plant_in_available : t -> PlayerId.t -> Plant.plant_stage -> bool

(** [num_in_available game player_id stage] is the number of plants of
    [stage] that player of [player_id] has in their available area. *)
val num_in_available : t -> PlayerId.t -> Plant.plant_stage -> int

(** [num_in_store game player_id stage] is the number of plants of
    [stage] that player of [player_id] has in their store. *)
val num_in_store : t -> PlayerId.t -> Plant.plant_stage -> int

(** [store_capacity game player_id stage] is the maximum number of
    plants of [stage] that player of [player_id] can have in their
    store. *)
val store_capacity : t -> PlayerId.t -> Plant.plant_stage -> int

(** [is_store_full game player_id stage] is true iff the number of
    plants of [stage] in player of [player_id]'s store is at maximum
    capacity. *)
val is_store_full : t -> PlayerId.t -> Plant.plant_stage -> bool

(** [turn game] is the player whose turn it is in [game]. *)
val turn : t -> Player.t

(** [player_of game player_id] is the player of [player_id] in [game]. *)
val player_of : t -> PlayerId.t -> Player.t

(** [cell_at game coord] is the cell at [coord] in [game]. *)
val cell_at : t -> HexUtil.coord -> Cell.t

(** [can_buy_plant game player_id stage] is true iff player of
    [player_id] can buy a plant of [stage] and place it in their
    available area. *)
val can_buy_plant : t -> PlayerId.t -> Plant.plant_stage -> bool

(** [player_light_points game player_id] is the number of light points
    that player of [player_id] has. *)
val player_light_points : t -> PlayerId.t -> int

(** [player_scoring_points game player_id] is the number of scoring
    points that player of [player_id] has. *)
val player_scoring_points : t -> PlayerId.t -> int

(** [next_scoring_points game soil] is the number of scoring points that
    will be collected by the next player to harvest a tree of type
    [soil]. *)
val next_scoring_points : t -> Cell.soil -> int

(** [cells game] is a list of the Cells in the board of game, in any
    order. *)
val cells : t -> Cell.t list
