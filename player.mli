(** The abstract type representing a player. *)
type t

(** [init_player id] is a player with player_id [id] and points and
    inventories set to default values. *)
val init_player : PlayerId.t -> t

(** [buy_plant player stage] moves a plant from [player]'s store to
    their available area. Raises: [PlantInventory.OutOfPlant stage] if
    there are no more plants of [stage]; [Store.InsufficientLightPoints]
    if [player] does not have enough light points to make the purchase. *)
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
val add_lp : t -> int -> t

(** [add_sp player pts] adds [pts] score points to [player]'s score
    points. *)
val add_sp : t -> int -> t
