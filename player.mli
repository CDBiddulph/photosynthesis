(** A player in the game. *)

(** The abstract type representing a player. *)
type t

(** [init_player id] is a player with player_id [id] and points and
    inventories set to default values. *)
val init_player : PlayerId.t -> t

(** [cost_to_plant stage] is the number of light points that it
    costs to plant a plant of [stage] by moving it from the available 
    area to the board. *)
val cost_to_plant : Plant.plant_stage -> int

(** [cost_to_harvest] is the number of light points that it costs
    to harvest a Large tree. *)
val cost_to_harvest : int

(** [can_buy_plant stage player] is true iff [player] can buy a
    plant of [stage] and place it in their available area. *)
val can_buy_plant : Plant.plant_stage -> t -> bool

(** [can_plant_plant stage player] is true iff [player] can plant a
    plant of [stage] from their available area. *)
val can_plant_plant : Plant.plant_stage -> t -> bool

(** [can_harvest player] is true iff [player] can harvest a plant. *)
val can_harvest : t -> bool

(** [buy_plant player stage] moves a plant of [stage] from [player]'s store to
    their available area, and deducts the appropriate amount of light
    points from the player's light point total. Raises:
    [PlantInventory.OutOfPlant stage] if there are no more plants of
    [stage]; [Player.InsufficientLightPoints] if [player] does not have
    enough light points to make the purchase. *)
val buy_plant : Plant.plant_stage -> t -> t

(** [plant_plant stage player] moves a plant of [stage] out of [player]'s available area,
    and deducts the appropriate amount of light points. Raises: [PlantInventory.OutOfPlant stage]
    if there are no more plants of [stage] in the available area; [Player.InsufficientLightPoints]
    if [player] does not have enough light points to place a plant of [stage]. *)
val plant_plant : Plant.plant_stage -> t -> t

(** [harvest sp player] deducts the appropriate number of light points
    and increases the number of scoring points by [sp]. Raises: [Player.InsufficientLightPoints]
    if [player] does not have enough light points to harvest a plant. *)
val harvest : int -> t -> t

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
