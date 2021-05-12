(** A player in the game. *)

(** The abstract type representing a player. *)
type t

(** [init_player id] is a player with player_id [id] and points and
    inventories set to default values. *)
val init_player : PlayerId.t -> t

(** [cost_to_buy stage player] is the number of light points that it
    costs to buy a plant of [stage] with [player]. *)
val cost_to_buy : Plant.plant_stage -> t -> int

(** [cost_to_grow stage] is the number of light points that it costs to
    grow a plant of stage one below [stage] into [stage] by moving it
    from the available area to the board. Raises:
    [PlantInventory.OutOfPlant stage] if there are no more plants of
    [stage] in [player]'s store.*)
val cost_to_grow : Plant.plant_stage -> int

(** [cost_to_harvest] is the number of light points that it costs to
    harvest a Large tree. *)
val cost_to_harvest : int

(** [cost_to_buy_and_grow stage player] is the number of light points
    that it costs to grow (or plant when [stage = Seed]) a plant of
    [stage], plus the cost of buying it, if and only if the available
    area does not already contain any plants of [stage]. Raises:
    [PlantInventory.OutOfPlant stage] if there are no more plants of
    [stage] in [player]'s store. *)
val cost_to_buy_and_grow : Plant.plant_stage -> t -> int

(** [can_buy_plant stage player] is true iff [player] can buy a plant of
    [stage] and place it in their available area. *)
val can_buy_plant : Plant.plant_stage -> t -> bool

(** [can_plant_plant stage player] is true iff [player] can plant a
    plant of [stage] from their available area. *)
val can_plant_plant : Plant.plant_stage -> t -> bool

(** [can_grow_plant stage player] is true iff [player] can grow a plant
    with stage one below [stage] into [stage] using a plant of [stage]
    from their available area. *)
val can_grow_plant : Plant.plant_stage -> t -> bool

(** [can_harvest player] is true iff [player] can harvest a plant. *)
val can_harvest : t -> bool

(** [buy_plant stage player] moves a plant of [stage] from [player]'s
    store to their available area, and deducts the appropriate amount of
    light points from the player's light point total. Raises:
    [PlantInventory.OutOfPlant stage] if there are no more plants of
    [stage]; [Player.InsufficientLightPoints cost] if [player] does not
    have enough light points to make the purchase. *)
val buy_plant : Plant.plant_stage -> t -> t

(** [plant_plant stage player] moves a plant of [stage] out of
    [player]'s available area. Raises: [PlantInventory.OutOfPlant stage]
    if there are no more plants of [stage] in the available area;
    [Player.InsufficientLightPoints cost] if [player] does not have
    enough light points to place a plant of [stage]. *)
val plant_plant : Plant.plant_stage -> t -> t

(** [grow_plant stage player] moves a plant of [stage] out of [player]'s
    available area, and deducts the appropriate amount of light points.
    Increments the number of trees of the stage previous to [stage] in
    the store of [player], if it is not already at capacity. Raises:
    [PlantInventory.OutOfPlant stage] if there are no more plants of
    [stage] in the available area; [Player.InsufficientLightPoints cost]
    if [player] does not have enough light points to grow a plant to
    [stage]. *)
val grow_plant : Plant.plant_stage -> t -> t

(** [harvest sp player] deducts the appropriate number of light points
    and increases the number of scoring points by [sp]. Increments the
    number of Large trees in the store of [player], if it is not already
    at capacity. Raises: [Player.InsufficientLightPoints cost] if
    [player] does not have enough light points to harvest a plant. *)
val harvest : int -> t -> t

(** [buy_and_grow_plant stage player] first checks to see if the
    available area has any plants of [stage]. If it does not,
    [buy_plant stage] is performed on [player]. Then, if [stage = Seed],
    [plant_plant stage] is performed on [player]. Otherwise, if
    [stage <> Seed], [grow_plant stage] is performed. Raises:
    [PlantInventory.OutOfPlant] or [Player.InsufficientLightPoints], as
    described in [buy_plant], except that in
    [Player.InsufficientLightPoints cost],
    [cost = cost_to_buy_and_grow stage player]. [plant_plant], and
    [grow_plant]. *)
val buy_and_grow_plant : Plant.plant_stage -> t -> t

(** [player_id player] is the unique PlayerId of [player]. *)
val player_id : t -> PlayerId.t

(** [light_points player] is the number of light points that [player]
    has. *)
val light_points : t -> int

(** [score_points player] is the number of score points that [player]
    has. *)
val score_points : t -> int

(** [add_lp player pts] adds [pts] light points to [player]'s light
    points. *)
val add_lp : int -> t -> t

(* TODO: can we remove this? *)
val store : t -> Store.t

(** [is_in_available stage player] is true if [player] has a plant of
    [stage] in their available area. *)
val is_in_available : Plant.plant_stage -> t -> bool

(** [num_in_available stage player] is the number of plants of [stage]
    in [player]'s available area. *)
val num_in_available : Plant.plant_stage -> t -> int

(** [num_in_store stage player] is the number of plants of [stage] that
    [player] has in their store. *)
val num_in_store : Plant.plant_stage -> t -> int

(** [store_capacity stage player] is the maximum number of plants of
    [stage] that [player] can have in their store. *)
val store_capacity : Plant.plant_stage -> t -> int

(** [is_store_full stage player] is true iff the number of plants of
    [stage] in [player]'s store is at maximum capacity. *)
val is_store_full : Plant.plant_stage -> t -> bool

(** [_set_available available player] is [player] with its available
    area set to [available]. Should only be used for testing. *)
val _set_available : PlantInventory.t -> t -> t

(** [_available player] is the available area of [player]. Should only
    be used for testing.*)
val _available : t -> PlantInventory.t

(** [_set_store store player] is [player] with its store set to [store].
    Should only be used for testing. *)
val _set_store : Store.t -> t -> t

(** [_store player] is the store of [player]. Should only be used for
    testing.*)
val _store : t -> Store.t
