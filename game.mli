(** Handles actions that can be taken by a player on the state of the
    player objects and the board *)

(** Type of the game state *)
type t

(** The type [ruleset] represents the ruleset (extended or normal) used
    in the game. *)
type ruleset =
  | Normal
  | Extended

(* Setter functions*)

(** [init_game num_players shadow_ruleset round_ruleset] is a new game
    with [num_players] players and all initial defaults. *)
val init_game : int -> Board.ruleset -> ruleset -> t

(** [_init_game_test num_players board turn starting_turn
    setup_rounds_left scoring_points num_rounds rounds_rule]
    is a new game with all of those things. Should only be used for
    testing. *)
val _init_game_test :
  int ->
  Board.t ->
  PlayerId.t ->
  PlayerId.t ->
  int ->
  (Cell.soil * int list) list ->
  int ->
  ruleset ->
  t

(** [_update_players_test players game] is a new game with [players].
    Should only be used for testing. *)
val _update_players_test : (PlayerId.t * Player.t) list -> t -> t

val board : t -> Board.t

(** [plant_seed coord player_id game] is game with a seed of [player]
    planted at [coord] and the available seeds of the player decremented
    by 1. Raises: [Board.IllegalPlacePlant] if planting a seed at
    [coord] is an illegal move; [PlantInventory.OutOfPlant Plant.Seed]
    if the player does not have any seeds in their available area. See
    [can_plant_seed] and [is_plant_available]. *)
val plant_seed : PlayerId.t -> HexUtil.coord -> t -> t

(** [plant_small coord game] is game with a small tree of [player]
    planted at [coord] and the available seeds of the player decremented
    by 1. Raises: [Board.IllegalPlacePlant] if planting a seed at
    [coord] is an illegal move; [PlantInventory.OutOfPlant Plant.Small]
    if the player does not have any small trees in their available area.
    See [can_plant_small] and [is_plant_available]. *)
val plant_small : HexUtil.coord -> t -> t

(** [grow_plant game coord] is [game] with the plant at [coord] in the
    board grown to the next stage, the available plants of the next
    stage of the plant at [coord] of the player whose turn it is
    decremented by 1, and the store of the plant stage that [plant] is
    replacing incremented by 1 if the store of [last_plant] is full
    (otherwise, it will stay the same, meaning the replaced plant was
    thrown away). Raises: [Board.IllegalGrowPlant] if growing the plant
    at [coord] is an illegal or impossible move;
    [PlantInventory.OutOfPlant next_stage] if the player does not have
    any of [next_stage] in their available area. See [can_grow_plant]
    and [is_plant_available]. *)
val grow_plant : HexUtil.coord -> t -> t

(** [harvest coord game] is [game] with the plant at [coord] removed and
    the scoring points of player whose turn it is increased
    appropriately. Raises: [Board.IllegalHarvest] if harvesting from
    [coord] with the current player of [game] is illegal. *)
val harvest : HexUtil.coord -> t -> t

(** [buy_plant stage game] is [game] where the current player of [game]
    has one less plant of [stage] in their store and one more plant of
    [stage] in their available area. Raises:
    [Store.InsufficientLightPoints] if Player does not have enough light
    points to buy a plant of [stage];
    [PlantInventory.OutOfPlant (Plant.plant_stage plant)] if the player
    does not have any of [Plant.plant_stage plant] in their store. *)
val buy_plant : Plant.plant_stage -> t -> t

(* In the future, we may want to alert the UI that photosynthesis is
   about to happen instead of just lumping photosynthesis in with
   end_turn, so that we can have a screen on which numbers appear under
   the trees that collect light points. *)

(** [end_turn game] is [game] with control relinquished to the player
    who plays after the current player, and with photosynthesis
    performed if appropriate. If [game] is not in setup, photosynthesis
    is accompanied by the direction of the sun moving and the number of
    rounds being incremented. Photosynthesis is performed when moving
    from setup to the normal game, and whenever a round in the normal
    game ends. *)
val end_turn : t -> t

(* Getter functions. *)

(** [can_plant_seed coord player_id game] is true if planting a seed
    with the player of [player_id] at [coord] is a legal move,
    disregarding whether the player actually has a seed in their
    available area. Will always be [false] if [game] is in setup mode. *)
val can_plant_seed : HexUtil.coord -> PlayerId.t -> t -> bool

(** [can_plant_small coord game] is true if planting a small tree at
    [coord] is a legal move. Will always be [false] if [game] is not in
    setup mode. *)
val can_plant_small : HexUtil.coord -> t -> bool

(** [can_grow_plant coord player_id game] is true iff growing the plant
    at [coord] with the player of [player_id] at [coord] is a legal
    move, disregarding whether the player actually has the plant in
    their available area. *)
val can_grow_plant : HexUtil.coord -> t -> bool

(** [can_harvest coord player_id game] is true iff harvesting the plant
    at [coord] with the player of [player_id] at [coord] is a legal
    move. *)
val can_harvest : HexUtil.coord -> t -> bool

(** [turn game] is the player_id of the player whose turn it is in
    [game]. *)
val turn : t -> PlayerId.t

(** [player_of game player_id] is the player of [player_id] in [game]. *)
val player_of : t -> PlayerId.t -> Player.t

(** [player_of_turn game] is the player whose turn it is in [game]. *)
val player_of_turn : t -> Player.t

(** [player_order game] is the list of players in order of play, with
    the first player to ever make a move listed first. *)
val player_order : t -> PlayerId.t list

(** [cell_at game coord] is the cell at [coord] in [game]. *)
val cell_at : t -> HexUtil.coord -> Cell.t

(** [next_scoring_points game soil] is the number of scoring points that
    will be collected by the next player to harvest a tree of type
    [soil]. *)
val next_scoring_points : t -> Cell.soil -> int

(** [cells game] is a list of the Cells in the board of game, in any
    order. *)
val cells : t -> Cell.t list

(** [sun_dir game] is the direction the sun is facing in [game]. *)
val sun_dir : t -> HexUtil.dir

(** [scoring_points game] is an association list of the list of
    remaining scoring points for each soil type, in order from the next
    available to the last available. *)
val scoring_points : t -> (Cell.soil * int list) list

(** [is_setup game] is true if [game] is in setup mode. *)
val is_setup : t -> bool

(** [winner game] is the winner of the game. If the game isn't over,
    then returns [None]. *)
val winner : t -> PlayerId.t option
