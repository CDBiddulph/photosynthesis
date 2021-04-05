type t

(** [buy_plant player stage] moves a plant from [player]'s store to
    their available area. Raises: [Store.OutOfPlant stage] if there are
    no more plants of [stage]; [InsufficientLightPoints] if [player]
    does not have enough light points to make the purchase. *)
(* val buy_plant : t -> Plant.plant_stage -> t *)

(** [player_id player] is the unique PlayerId of [player]. *)
val player_id : t -> PlayerId.t

(** [light_points player] is the number of light points that [player]
    has. *)
val light_points : t -> int
