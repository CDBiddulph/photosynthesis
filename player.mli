(** A player in the game. *)

(** The abstract type representing a player. *)
type t

(** [init_player id] is a player with player_id [id] and points and
    inventories set to default values. *)
val init_player : PlayerId.t -> t

(** [buy_plant player stage] moves a plant from [player]'s store to
    their available area, and deducts the appropriate amount of light
    points from the player's light point total. Raises:
    [PlantInventory.OutOfPlant stage] if there are no more plants of
    [stage]; [Store.InsufficientLightPoints] if [player] does not have
    enough light points to make the purchase. *)
val buy_plant : t -> Plant.plant_stage -> t

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

(** [add_sp player pts] adds [pts] score points to [player]'s score
    points. *)
val add_sp : int -> t -> t

(** [is_in_available stage player] is true if [player] has a plant of
    [stage] in their available area. *)
val is_in_available : Plant.plant_stage -> t -> bool

(** [num_in_available stage player] is the number of plants of [stage]
    in [player]'s available area. *)
val num_in_available : Plant.plant_stage -> t -> int

(** [num_in_store stage player] is the number of plants of [stage] that
    [player] has in their store. *)
val num_in_store : t -> Plant.plant_stage -> int

(** [store_capacity stage player] is the maximum number of plants of
    [stage] that [player] can have in their store. *)
val store_capacity : t -> Plant.plant_stage -> int

(** [is_store_full stage player] is true iff the number of plants of
    [stage] in [player]'s store is at maximum capacity. *)
val is_store_full : t -> Plant.plant_stage -> bool

(** [can_buy_plant game player_id stage] is true iff [player] can buy a
    plant of [stage] and place it in their available area. *)
val can_buy_plant : t -> Plant.plant_stage -> bool
