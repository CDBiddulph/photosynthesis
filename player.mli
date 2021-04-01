(** The abstract type representing a player. *)
type t

(** [player_id] is a unique integer representing a player. *)
type player_id = int

(** [buy_plant player stage] moves a plant from [player]'s store to
    their available area. Raises: [Store.OutOfPlant stage] if there are
    no more plants of [stage]; [InsufficientLightPoints] *)
val buy_plant : t -> Plant.plant_stage -> t

(** [player_id player] is the unique [player_id] of [player]. *)
val player_id : t -> player_id

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
